module day1710_mod
  use parse_mod, only : string_t, split, read_strings
  implicit none
  private
  public day1710, knot_hash

  type hash_t
    integer, allocatable :: h(:)
    integer :: cp = 1 ! current position
    integer :: ss = 0 ! skip size
  end type

  integer, parameter :: SUFFIX(5) = [17, 31, 73, 47, 23]
  integer, parameter :: HASH_LEN = 256, HASH_ROUNDS = 64

contains

  subroutine day1710(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: lens(:)
    type(hash_t) :: hash
    character(len=32) :: ans2
    integer :: ans1

    lens = read_lengths(file)
    call hash_round(hash, lens, 1)
    ans1 = hash%h(1)*hash%h(2)
    print '("Answer 10/1 ",i0,l2)', ans1, ans1==6909

    ! Part 2
    lines = read_strings(file)
    ans2 = knot_hash(lines(1)%str)
    print '("Answer 10/2 ",a,l2)', ans2, ans2=='9d5f4561367d379cfbf04f8c471c0095'

  end subroutine day1710


  function knot_hash(str) result(dhash)
    character(len=*), intent(in) :: str
    character(len=32) :: dhash

    integer, allocatable :: lens(:)
    type(hash_t) :: hash
    integer :: i, j, x, ii, p

    lens = str_to_lengths(str)
    call hash_round(hash, lens, HASH_ROUNDS)

    ! Make dense hash
    do j=1,16
      do i=1,16
        ii = (j-1)*16 + i
        if (i==1) then 
          x = xor(hash%h(ii), hash%h(ii+1))
        else if (i==2) then
          continue
        else
          x = xor(x, hash%h(ii))
        end if
      end do
      p = 2*j-1
      dhash(p:p+1) = dec2hex(x)     
    end do
  end function knot_hash


  pure function dec2hex(x) result(c)
    integer, intent(in) :: x
    character(len=2) :: c

    integer :: i, x0, d

    x0 = x
    do i=2,1,-1
      d = x0/(16**(i-1))
      x0 = x0 - d*16**(i-1)
      if (d<10) then
        c(3-i:3-i) = achar(d+iachar('0'))
      else if (d<16) then
        c(3-i:3-i) = achar(d-10+iachar('a'))
      else
        error stop 'dec2hex - wrong conversion'
      end if
    end do
    if (x0/=0) error stop 'dec2hex - wrong conversion'
  end function dec2hex


  pure function str_to_lengths(str) result(lens)
    character(len=*), intent(in) :: str
    integer, allocatable :: lens(:)

    integer :: i, n

    n = len(str)+size(SUFFIX)
    allocate(lens(n))
    do i=1,len(str)
      lens(i) = iachar(str(i:i))
    end do
    lens(len(str)+1:n) = SUFFIX
  end function str_to_lengths


  function read_lengths(file) result(lens)
    character(len=*), intent(in) :: file
    integer, allocatable :: lens(:)

    integer :: fid, i 
    character(len=1000) :: line
    type(string_t), allocatable :: nums(:)

    open(newunit=fid, file=file, status='old')
    read(fid, '(a)') line
    close(fid)
    call split(trim(line), ',', nums)
    allocate(lens(size(nums)))
    do i=1,size(nums)
      read(nums(i)%str,*) lens(i)
    end do
  end function read_lengths


  pure type(hash_t) function hash_new(size) result(new)
    integer, intent(in) :: size

    integer :: i

    allocate(new%h(size))
    do i = 0, size-1
      new%h(i+1) = i
    end do
  end function hash_new


  pure subroutine hash_round(this, lens, nround)
    type(hash_t), intent(out) :: this
    integer, intent(in) :: lens(:), nround

    integer :: i, iround

    this = hash_new(HASH_LEN)
    do iround = 1, nround
      do i=1, size(lens)
        call hash_step(this, lens(i))
      end do
    end do
  end subroutine hash_round


  pure subroutine hash_step(this, ln)
    class(hash_t), intent(inout) :: this
    integer, intent(in) :: ln

    integer :: n, ib, ie, tmp(size(this%h))

    ! Step 1 - reverse order
    n = size(this%h)
    if (ln > n) error stop 'length larger than hash size'
    ib = this%cp
    ie = ib+ln-1
    ie = mod(ie-1,n)+1
    if (ib <= ie) then
      this%h(ib:ie) = this%h(ie:ib:-1)
    else if (ln == 0) then
      continue
    else
      if (ie /= ln - (n-ib+1)) error stop 'step - inconsistency'
      tmp(1:n-ib+1) = this%h(ib:n)
      tmp(n-ib+2:ln) = this%h(1:ie)
      tmp(1:ln) = tmp(ln:1:-1)
      this%h(ib:n) = tmp(1:n-ib+1)
      this%h(1:ie) = tmp(n-ib+2:ln)
    end if

    ! Step 2 - move current position
    this%cp = this%cp + ln + this%ss
    this%cp = mod(this%cp-1, n) + 1

    ! Step 3 - increase skip size
    this%ss = this%ss + 1
  end subroutine hash_step

end module day1710_mod