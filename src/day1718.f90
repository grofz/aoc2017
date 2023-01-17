module day1718_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I8B => int64
  use queue_mod, only : queue_t, queue_removeall
  implicit none

  integer, parameter :: IDMAX = iachar('z')-iachar('a')+1
  integer, parameter :: MAX_QUEUE = 200
  integer, parameter :: STATUS_OK = 0, STATUS_SENDING = 10, STATUS_WAITING = 20

  type register_t
    integer :: id = 0
    integer(I8B) :: v = 0
  contains
    procedure :: val => register_val
  end type register_t

  type instruction_t
    character(len=3) :: ch
    type(register_t) :: op(2)
  end type instruction_t

  type comp_t
    type(register_t) :: regs(IDMAX)
    type(instruction_t), allocatable :: prog(:)
    integer :: ip = 1
    integer :: sendcounter = 0
    integer(I8B) :: saved = 0
    type(queue_t) :: receiving
  contains
    procedure :: exec_one => comp_exec_one
  end type comp_t
  interface comp_t
    module procedure comp_new
  end interface

contains

  subroutine day1718(file)
    character(len=*), intent(in) :: file

    type(comp_t) :: c1, c2
    integer :: i1, i2

    ! Part 1
    c1 = comp_new(file, 0)
    do
      call comp_exec_one(c1, i1)
      if (i1==STATUS_WAITING) then
        associate(op=>c1%prog(c1%ip)%op)
        if (op(1)%val(c1%regs)/=0) exit
        c1%ip = c1%ip+1
        end associate
      end if
    end do
    call queue_removeall(c1%receiving)
    print '("Answer 18/1 ",i0,l2)', c1%saved, c1%saved==8600

    ! Part 2
    c1 = comp_new(file, 0)
    c2 = comp_new(file, 1)
    do
      call comp_exec_one(c1, i1)
      call comp_exec_one(c2, i2)
      if (i1==STATUS_SENDING) call c2%receiving%enqueue(int(c1%saved))
      if (i2==STATUS_SENDING) call c1%receiving%enqueue(int(c2%saved))
      if (i1==STATUS_WAITING .and. i2==STATUS_WAITING) exit
    end do
    call queue_removeall(c1%receiving)
    call queue_removeall(c2%receiving)
    print '("Answer 18/2 ",i0,l2)', c2%sendcounter, c2%sendcounter==7239

  end subroutine day1718


  subroutine comp_exec_one(this, iexit)
    class(comp_t), intent(inout) :: this
    integer, intent(out) :: iexit

    integer :: ijump, itmp

    ijump = 1
    iexit = STATUS_OK
    if (this%ip < 1 .or. this%ip > size(this%prog)) error stop 'exec_one - ip out of program range'
    associate(op=>this%prog(this%ip)%op)
      select case(this%prog(this%ip)%ch)
      case('set')
        this%regs(op(1)%id)%v = op(2)%val(this%regs)
      case('add')
        this%regs(op(1)%id)%v = op(1)%val(this%regs) + op(2)%val(this%regs)
      case('sub')
        this%regs(op(1)%id)%v = op(1)%val(this%regs) - op(2)%val(this%regs)
      case('mul')
        this%regs(op(1)%id)%v = op(1)%val(this%regs) * op(2)%val(this%regs)
      case('mod')
        this%regs(op(1)%id)%v = modulo(op(1)%val(this%regs), op(2)%val(this%regs))
        !this%regs(op(1)%id)%v = mod(op(1)%val(this%regs), op(2)%val(this%regs))
      case('jgz')
        if (op(1)%val(this%regs)>0) ijump = op(2)%val(this%regs)
      case('jnz')
        if (op(1)%val(this%regs)/=0) ijump = op(2)%val(this%regs)
      case('snd')
        this%saved = op(1)%val(this%regs)
if (abs(this%saved) > huge(itmp)-1) error stop 'queue is not int64'
        iexit = STATUS_SENDING
        this%sendcounter = this%sendcounter+1
      case('rcv')
        if (this%receiving%isempty()) then
          ijump = 0
          iexit = STATUS_WAITING
        else
          call this%receiving%dequeue(itmp)
          this%regs(op(1)%id)%v = itmp
        end if
      case default
        print *, this%prog(this%ip)%ch
        error stop 'exec_one'
      end select
      this%ip = this%ip + ijump
    end associate
  end subroutine comp_exec_one


  integer(I8B) function register_val(this, regs) result(val)
    class(register_t), intent(in) :: this
    class(register_t), intent(in) :: regs(:)

    if (this%id==0) then
      val = this%v
    else
      if (this%id<1 .or. this%id>size(regs)) error stop 'register_val - wrong id'
      val = regs(this%id)%v
      if (regs(this%id)%id /= this%id) error stop 'register_value - aaa'
    end if
  end function register_val


  type(comp_t) function comp_new(file, rank) result(new)
    character(len=*), intent(in) :: file
    integer, intent(in), optional :: rank

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)
    allocate(new%prog(size(lines)))
    do i=1,size(lines)
      call parse(lines(i)%str, new%prog(i)%ch, new%prog(i)%op)
    end do

    do i=1, size(new%regs)
      new%regs(i)%v = 0
      new%regs(i)%id = i
    end do

    ! Set rank and initialize the receiving queue
    if (present(rank)) then
      new%regs(iachar('p')-iachar('a')+1)%v = rank
    end if
    new%receiving = queue_t(MAX_QUEUE)
  end function comp_new


  subroutine parse(str, inst, regs)
    character(len=*), intent(in) :: str
    character(len=3), intent(out) :: inst
    type(register_t), intent(out) :: regs(2)

    type(string_t), allocatable :: toks(:)
    integer :: k

    call split(str, ' ', toks)
    if (size(toks)/=3 .and. size(toks)/=2) error stop 'parse - not three tokens'
    if (len(toks(1)%str)/=3) error stop 'parse - command not three letters'
    inst = toks(1)%str

    do k=1, size(toks)-1
      associate(t=>toks(k+1)%str)
      if (iachar(t(1:1))<=iachar('z') .and. iachar(t(1:1))>=iachar('a')) then
        ! register
        regs(k)%id = iachar(t(1:1))-iachar('a')+1
        if (len(t)/=1) error stop 'parse - register name not one letter'
        regs(k)%v = -huge(regs(k)%v)
      else
        ! value
        regs(k)%id = 0
        read(t,*) regs(k)%v
      end if
      end associate
    end do
  end subroutine parse

end module day1718_mod