module day1713_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type scanner_t
    integer :: depth
    integer :: range
  contains
    procedure :: caught => scanner_caught
  end type scanner_t

contains

  subroutine day1713(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(scanner_t), allocatable :: fwall(:)
    integer :: x, t, j, maxdepth, ans1, delay
    integer, parameter :: MAXITER=10000000

    lines = read_strings(file)
    allocate(fwall(size(lines)))
    do j=1, size(fwall)
      fwall(j) = scanner_new(lines(j)%str)
    end do
    maxdepth = maxval(fwall(:)%depth)

    ! assuming fwall is ordered by depth !!!
    ans1 = 0
    j = 1
    do x = 0, maxdepth
      if (fwall(j)%depth == x) then
        ans1 = ans1 + fwall(j)%caught(x)
        j = j + 1
      else
      end if
    end do
    ans1 = ans1 - 1 ! depth "0" return severity "1" if caught
    print '("Answer 13/1 ",i0,l2)', ans1, ans1==1612

    DLOOP: do delay = 0, MAXITER
      j = 1
      do x= 0, maxdepth
        ! cycle if no scanner at current depth "x"
        if (fwall(j)%depth /= x) cycle

        ! no need to continue if got caugth
        if (fwall(j)%caught(x+delay) > 0) cycle DLOOP

        ! passed through the scanner
        j = j + 1
      end do
      ! passed through without getting caught
      exit DLOOP
    end do DLOOP
    if (delay==MAXITER+1) error stop 'day 13 - loop maxiter too small'
    print '("Answer 13/2 ",i0,l2)', delay, delay == 3907994
  end subroutine day1713


  integer function scanner_caught(this, t) result(severity)
    class(scanner_t), intent(in) :: this
    integer, intent(in) :: t

    integer :: p

    ! scanner is at the first position each 2*(range-1) second
    p = 2*(this%range-1)

    if (mod(t,p)==0) then
      ! get caught
      severity = this%depth * this%range
      ! make sure "severity" is not zero if get caught
      severity = max(1, severity)
    else
      severity = 0
    end if
  end function scanner_caught


  type(scanner_t) function scanner_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i, ios 

    i = scan(str,': ')
    if (i==0) error stop 'new - invalid format'
    read(str(1:i-1),*,iostat=ios) new%depth
    if (ios/=0) error stop 'new - error reading depth'
    read(str(i+2:),*) new%range
    if (ios/=0) error stop 'new - error reading range'
  end function scanner_new

end module day1713_mod
