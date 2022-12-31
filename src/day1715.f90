module day1715_mod
  use iso_fortran_env, only : I8 => int64
  implicit none

  integer(I8), parameter :: DIVIDOR=2147483647
  integer, parameter :: NBITS=16
  integer, parameter :: FACTORS(2) = [16807, 48271] 
  type generator_t 
    integer(I8) :: prev
    integer(I8) :: fac
    integer(i8) :: crit = 1 ! generated value must be multiple of "crit"
  contains
    procedure :: test => generator_test
    procedure :: next => generator_next
  end type

contains

  subroutine day1715
    type(generator_t) :: gena, genb
    integer, parameter :: NPART1 = 40000000, NPART2 = 5000000
    integer :: initvals(2)
    integer :: i, ans1, ans2, fid
    character(len=25) dummy

    gena%fac = FACTORS(1)
    genb%fac = FACTORS(2)
    open(newunit=fid, file='inp/15/input.txt', status='old')
      read(fid,'(24x,i3)') initvals(1)
      read(fid,'(24x,i3)') initvals(2)
    close(fid)

    ! Part 1
    ans1 = 0
    gena%prev = initvals(1)
    genb%prev = initvals(2)
    do i=1,NPART1
      call gena%next()
      call genb%next()
      !if (all(gena%test() .eqv. genb%test())) ans1 = ans1 + 1
      if (judge(gena,genb)) ans1 = ans1 + 1
    end do
    print '("Answer 15/1 ",i0,l2)', ans1, ans1==588 .or. ans1==573


    ! Part 2
    gena%prev = initvals(1)
    genb%prev = initvals(2)
    ans2 = 0
    gena%crit = 4
    genb%crit = 8
    do i=1,NPART2
      call gena%next()
      call genb%next()
      if (judge(gena,genb)) ans2 = ans2 + 1
    end do
    print '("Answer 15/2 ",i0,l2)', ans2, ans2==309 .or. ans2==294
  end subroutine day1715


  subroutine generator_next(this)
    class(generator_t), intent(inout) :: this
    do
      this%prev = mod(this%prev*this%fac, DIVIDOR)
      if (mod(this%prev,this%crit)==0) exit
    end do
  end subroutine generator_next


  function generator_test(this) result(bits)
    class(generator_t), intent(in) :: this
    logical :: bits(NBITS)

    integer :: i
    do i=NBITS-1, 0, -1
      bits(NBITS-i) = btest(this%prev, i)
    end do
  end function


  logical function judge(a,b) result(issame)
    class(generator_t), intent(in) :: a, b
    integer :: i

    issame = .false.
    do i=NBITS-1, 0, -1
      if (btest(a%prev, i) .neqv. btest(b%prev,i)) return
    end do
    issame = .true.
  end function judge

end module day1715_mod