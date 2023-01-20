module day1725_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type turing_t
    character(len=1) :: state
    logical, allocatable :: tape(:)
    type(rule_t), allocatable :: rules(:)
    integer :: pos=1
    integer :: steps_left
  contains
    procedure :: move => turing_move
    procedure :: step => turing_step
  end type

  type rule_t
    character(len=1) :: state
    logical :: newval(2)
    integer :: move(2)
    character(len=2) :: newstate(2)
  end type

  integer, parameter :: MOVE_LEFT = -1, MOVE_RIGHT = +1

contains

  subroutine day1725(file)
    character(len=*), intent(in) :: file

    type(turing_t) :: cpu
    type(string_t), allocatable :: lines(:)
    integer :: i, j

    !lines = read_strings('inp/25/test.txt')
    lines = read_strings(file)
    i = 1
    j = index(lines(i)%str, 'Begin in state ')
    if (j==0) error stop 'day 25 - first line unexpected'
    j = j + 15
    cpu%state = lines(i)%str(j:j)
    i = 2
    j = index(lines(i)%str, 'checksum after ')
    if (j==0) error stop 'day 25 - second line unexpected'
    j = j + 15
    read(lines(i)%str(j:),*) cpu%steps_left

    allocate(cpu%tape(5), source=.false.)
    allocate(cpu%rules(0))
    cpu%pos = 3
    i = 4
    do
      if (i > size(lines)) exit
      cpu%rules = [cpu%rules, parse_rule(lines, i)]
    end do

    do
      call cpu%step()
      if (cpu%steps_left <= 0) exit
    end do
    print '("Answer 25 ",i0,l2)', count(cpu%tape), count(cpu%tape)==4217
!print *, size(cpu%tape), cpu%pos, cpu%state

  end subroutine day1725


  subroutine turing_step(this)
    class(turing_t), intent(inout) :: this

    integer :: j, k

    do j=1,size(this%rules)
      if (this%rules(j)%state /= this%state) cycle

      if (this%tape(this%pos)) then
        k = 2
      else
        k = 1
      end if
      this%tape(this%pos) = this%rules(j)%newval(k)
      call this%move(this%rules(j)%move(k))
      this%state = this%rules(j)%newstate(k)
      this%steps_left = this%steps_left - 1
      exit
    end do
    if (j==size(this%rules)+1) error stop 'step - instruction not found'
  end subroutine turing_step


  function parse_rule(lines, ipos) result(new)
    type(string_t), intent(in) :: lines(:)
    integer, intent(inout) :: ipos
    type(rule_t) :: new

    integer :: j, j2, k
    character(len=1) :: ch

    j = index(lines(ipos)%str, 'In state ')
    if (j==0) then
      error stop 'parse - first line'
    else
      j = j + 9
      new%state = lines(ipos)%str(j:j)
      ipos = ipos + 1
    end if

    do k=1, 2
      write(ch,'(i1)') k-1
      j = index(lines(ipos)%str, 'If the current value is '//ch//':')
      if (j==0) error stop 'parse - second line'
      ipos = ipos + 1

      ! Write the value
      j = index(lines(ipos)%str, 'value ')
      if (j==0) error stop 'parse - value'
      j = j + 6
      select case (lines(ipos)%str(j:j))
      case('0')
        new%newval(k) = .false.
      case('1')
        new%newval(k) = .true.
      case default
        error stop 'parse - value uknonw'
      end select
      ipos = ipos + 1

      ! Move the tape
      j = index(lines(ipos)%str, 'Move one slot to the ')
      if (j==0) error stop 'parse - move'
      j = j + 21
      select case (lines(ipos)%str(j:j+3))
      case('left')
        new%move(k) = MOVE_LEFT
      case('righ')
        new%move(k) = MOVE_RIGHT
      case default
        error stop 'parse - move uknonw'
      end select
      ipos = ipos + 1

      ! New state
      j = index(lines(ipos)%str, 'Continue with state ')
      if (j==0) error stop 'parse - new state'
      j = j + 20
      new%newstate(k) = lines(ipos)%str(j:j)
      ipos = ipos + 1
    end do

    ! skip empty line
    if (ipos <= size(lines)) then
      if (len(lines(ipos)%str) /= 0) error stop 'parse - not empty line'
      ipos = ipos + 1
    end if
  end function parse_rule


  subroutine turing_move(this, imove)
    class(turing_t), intent(inout) :: this
    integer, intent(in) :: imove

    logical, allocatable :: exttape(:)
    integer :: n, next

    ! Extend the tape (if necessary) by adding "next" items at beginning
    ! and at the end
    n = size(this%tape)
    if (this%pos<= 1 .or. this%pos>=n) then
      next = n/2
      allocate(exttape(2*next+n), source=.false.)
      exttape(next+1:next+n) = this%tape
      this%pos = this%pos + next
      call move_alloc(exttape, this%tape)
    end if

    ! Move the head
    this%pos = this%pos + imove
  end subroutine turing_move

end module day1725_mod