module day1717_mod
  implicit none
  integer, parameter :: NSTEP_INPUT = 301
  integer, parameter :: NVALS_PART2 = 50000000, NVALS_PART1 = 2017

  type node_t
    integer :: val
    type(node_t), pointer :: next => null()
  end type

  type clist_t
    type(node_t), pointer :: curr => null()
    type(node_t), pointer :: head
    integer :: n = 0
    integer :: p = 0
    integer :: val1 = 0
  end type 

contains
  subroutine day1717
    type(clist_t) :: clist
    integer :: nstep, i, ans1, ans2

    clist = clist_new()
    nstep = NSTEP_INPUT

    do i=1, NVALS_PART2
      call clist_jump(clist, nstep)
      call clist_add(clist, i)
      if (i==NVALS_PART1) then
        ans1 = clist%curr%next%val
        print '("Answer 17/1 ",i0,l2)', ans1, ans1==1642
      end if
    end do
    ans2 = clist%val1
    print '("Answer 17/2 ",i0,l2)', ans2, ans2==33601318
    call clist_free(clist)
  end subroutine day1717


  function node_new(val) result(new)
    integer, intent(in) :: val
    type(node_t), pointer :: new

    allocate(new)
    new%val = val
  end function node_new


  type(clist_t) function clist_new() result(new)
    new%n = 1
    new%curr => node_new(0)
    new%curr%next => new%curr
    new%head => new%curr
  end function


  subroutine clist_add(this, val)
    class(clist_t), intent(inout) :: this
    integer, intent(in) :: val

    type(node_t), pointer :: new

    if (val <= NVALS_PART1) then
      new => node_new(val)
      new%next => this%curr%next
      this%curr%next => new
      this%curr => new
    end if
    this%n = this%n + 1

    if (this%p==0) this%val1 = val
    this%p = this%p + 1
  end subroutine clist_add


  subroutine clist_free(this)
    type(clist_t), intent(inout) :: this

    type(node_t), pointer :: curr, dele

    curr => this%head%next
    do
      if (.not. associated(curr)) exit
      if (associated(curr,this%head)) exit
      dele => curr
      curr => dele%next
      deallocate(dele)
    end do
    deallocate(this%head)
  end subroutine clist_free


  subroutine clist_jump(this, n)
    class(clist_t), intent(inout) :: this
    integer, intent(in) :: n

    integer :: n0, i

    n0 = mod(n, this%n)
    if (this%n <= NVALS_PART1) then
      do i = 1, n0
        this%curr => this%curr%next
      end do
    end if

    this%p = this%p + n0
    this%p = mod(this%p, this%n)
  end subroutine clist_jump


  subroutine clist_print(this, nvals)
    type(clist_t), intent(in) :: this
    integer, intent(in), optional :: nvals

    type(node_t), pointer :: curr
    integer :: i, nvals0

    nvals0 = this%n
    if (present(nvals)) nvals0 = nvals
    curr => this%curr
    do i=1, nvals0
      write(*,'(i4,1x)', advance='no') curr%val
      curr => curr%next
    end do
    write(*,*)
  end subroutine clist_print

end module day1717_mod