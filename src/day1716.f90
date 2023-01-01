module day1716_mod
  use parse_mod, only : split, read_strings, string_t
  implicit none

  integer, parameter :: NMAX = 16, MAXPATTERN = 200
  type dance_t
    character(len=NMAX) :: a
  contains
    procedure :: partner => dance_partner, spin => dance_spin, exchange => dance_exchange
  end type

contains

  subroutine day1716(file)
    character(len=*), intent(in) :: file

    type(dance_t) :: dance
    type(string_t), allocatable :: line(:), moves(:)
    integer :: i, j, k, n, pattern(MAXPATTERN)
    integer, parameter :: REPEAT=1000000000

    line = read_strings(file)
    if (size(line)/=1) error stop 'day 17 - single line expected'
    call split(line(1)%str, ',', moves)

    ! Part 1 - just one dance
    dance = dance_new(NMAX)
    do i=1,size(moves)
      call parse_move(dance, moves(i)%str)
    end do
    print '("Answer 16/1 ",a,l2)', dance%a, dance%a=='cknmidebghlajpfo'

    ! Part 2 - find cycle size
    dance = dance_new(NMAX)
    n = 1
    do j=1,MAXPATTERN
      ! Add current position of "a"
      pattern(j) = scan(dance%a,'a')
      ! Make dance
      do i=1,size(moves)
        call parse_move(dance, moves(i)%str)
      end do
      if (j >= 2*n) then
        ! Test pattern of lenght "n"
        do i=1,n
          if (pattern(i) /= pattern(i+n)) exit
        end do
        if (i==n+1) then
          ! Pattern of current length matches,
          ! ... test the whole range
          do i=1,j-n
            if (pattern(i)/=pattern(i+n)) exit
          end do
          if (i/=j-n+1) error stop 'pattern does not match not anymore'
        else
          ! Current length too small, increase it
          n = n + 1
        end if
      end if
    end do
!print *, 'Pattern repeats after n =', n

    ! Now dance just remainder of dances
    k = mod(REPEAT, n)
!print *, 'Dances equivalent to 1 bilion: ', k
    dance = dance_new(NMAX)
    do j=1,k
      do i=1,size(moves)
        call parse_move(dance, moves(i)%str)
      end do
    end do
    print '("Answer 16/2 ",a,l2)', dance%a, dance%a=='cbolhmkgfpenidaj'
  end subroutine day1716


  subroutine parse_move(this, str)
    class(dance_t), intent(inout) :: this
    character(len=*), intent(in) :: str

    integer :: i, j, k, ios
    character(len=1) :: x, y

    select case(str(1:1))
    case('s')
      read(str(2:),*,iostat=ios) i
      if (ios/=0) error stop 'spin - error reading size'
      call this%spin(i)
    case('x')
      i = scan(str, '/')
      if (i==0) error stop 'exchange - dividor not present'
      read(str(2:i-1),*,iostat=ios) j
      if (ios/=0) error stop 'exchange - error reading first position'
      read(str(i+1:),*,iostat=ios) k
      if (ios/=0) error stop 'exchange - error reading second position'
      call this%exchange(j, k)
    case('p')
      i = scan(str, '/')
      if (i/=3) error stop 'partner - dividor in wrong position'
      read(str(2:i-1),*) x
      read(str(i+1:),*) y
      call this%partner(x, y)
    case default
      error stop 'parse move - invalid move'
    end select
  end subroutine parse_move


  subroutine dance_partner(this, x,y)
    class(dance_t), intent(inout) :: this
    character(len=1), intent(in) :: x,y

    integer :: i, j
    character(len=1) :: tmp
    
    i = scan(this%a, x)
    j = scan(this%a, y)
    if (i==0 .or. j==0) error stop 'partner - character not found'
    tmp = this%a(i:i)
    this%a(i:i) = this%a(j:j)
    this%a(j:j) = tmp
!print *, 'partner ',x, y
  end subroutine dance_partner


  subroutine dance_exchange(this, x,y)
    class(dance_t), intent(inout) :: this
    integer, intent(in) :: x, y

    character(len=1) :: tmp

    if (x>NMAX .or. y>NMAx) error stop 'exchange - x/y too big'
    tmp = this%a(x+1:x+1)
    this%a(x+1:x+1) = this%a(y+1:y+1)
    this%a(y+1:y+1) = tmp
!print *, 'exchange ',x, y
  end subroutine dance_exchange


  subroutine dance_spin(this, x)
    class(dance_t), intent(inout) :: this
    integer, intent(in) :: x

    character(len=len(this%a)) :: tmp

    if (x > NMAX) error stop 'spin - x too big'
    tmp(1:x) = this%a(NMAX-x+1:NMAX)
    tmp(x+1:NMAX) = this%a(1:NMAX-x)
    this%a = tmp
!print *, 'spin ',x
  end subroutine dance_spin


  type(dance_t) function dance_new(n) result(new)
    integer, intent(in) :: n

    integer :: i

    if (n/=NMAX) error stop 'n not same as NMAX'

    do i=0, n-1
      new%a(i+1:i+1) = achar(iachar('a')+i)
    end do
  end function dance_new
end module day1716_mod