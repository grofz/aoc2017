module day1721_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day1721(file)
    character(len=*), intent(in) :: file

    character(len=*), parameter :: INITSTR = '..##.#.##'
    integer, parameter :: REP1=5, REP2=18
    type(string_t), allocatable :: lines(:)
    character(len=1), allocatable :: mm(:,:)
    integer :: i, ans1, ans2
    real :: t0, t1

    call cpu_time(t0)
    lines = read_strings(file)
    call readmat(INITSTR, mm)
    do i=1, REP2
      mm = enhance(mm, lines)
      !if (i<=REP1) call print_mat(mm)
      if (i==REP1) ans1 = count(mm=='#')
    end do
    ans2 = count(mm=='#')
    print '("Answer 21/1 ",i0,l2)', ans1, ans1==184
    print '("Answer 21/2 ",i0,l2)', ans2, ans2==2810258
    call cpu_time(t1)
    print '("Time taken ",f6.2, " seconds.")', t1-t0

  end subroutine day1721


  pure function enhance(src, lines) result(enh)
    character(len=1), intent(in) :: src(:,:)
    type(string_t), intent(in) :: lines(:)
    character(len=1), allocatable :: enh(:,:)

    integer :: nsrc, nenh, i, j, nshf

    nsrc = size(src,1)
    if (mod(nsrc,2)==0) then
      nshf = 2
      nenh = nsrc/2 * 3
    else if (mod(nsrc,3)==0) then
      nshf = 3
      nenh = nsrc/3 * 4
    else
      error stop 'not divisible by 2 or 3'
    end if
    allocate(enh(nenh,nenh))

    do i=1, nsrc/nshf
    do j=1, nsrc/nshf
      enh((i-1)*(nshf+1)+1:i*(nshf+1), (j-1)*(nshf+1)+1:j*(nshf+1)) = &
      &  enhance_core(src((i-1)*nshf+1:i*nshf, (j-1)*nshf+1:j*nshf), lines)
    end do
    end do
  end function enhance


  pure function enhance_core(s, lines) result(r)
    character(len=1), intent(in) :: s(:,:)
    type(string_t), intent(in) :: lines(:)
    character(len=1) :: r(size(s,1)+1,size(s,1)+1)

    character(len=1), allocatable :: ma(:,:), mb(:,:)
    integer :: i

    if (size(s,1)/=size(s,2)) error stop 'enhance_core - not square'
    do i=1, size(lines)
      call line2mat(lines(i)%str, ma, mb)
      if (size(ma,1) /= size(s,1)) cycle
      if (.not. ispattern(s, ma)) cycle
      if (size(mb,1) /= size(r,1)) error stop 'enhance_core - wrong result'
      r = mb
      exit
    end do
    if (i==size(lines)+1) error stop 'enhance_core - pattern not found'
  end function enhance_core


  pure function ispattern(ma, mb)
    character(len=1), intent(in) :: ma(:,:), mb(:,:)
    logical :: ispattern

    integer :: i

    ispattern = .false.
    do i=0, 7
      select case(i)
      case(0)
        if (all(ma==mb)) ispattern = .true.
      case(1)
        if (all(ma==flipx(mb))) ispattern = .true.
      case(2)
        if (all(ma==flipy(mb))) ispattern = .true.
      case(3)
        if (all(ma==rotccw(mb))) ispattern = .true.
      case(4)
        if (all(ma==rotcw(mb))) ispattern = .true.
      case(5)
        if (all(ma==flipy(flipx(mb)))) ispattern = .true.
      case(6)
        if (all(ma==flipy(rotccw(mb)))) ispattern = .true.
      case(7)
        if (all(ma==flipy(rotcw(mb)))) ispattern = .true.
      end select
      if (ispattern) exit
    end do
  end function ispattern


  ! ============================
  ! Rotate and flip the matrices
  ! ============================
  pure function flipx(mm) result(rr)
    character(len=1), intent(in) :: mm(:,:)
    character(len=1) :: rr(size(mm,1), size(mm,2))

    integer :: nx, ny, i
    nx = size(mm,2) ! no of cols
    ny = size(mm,1) ! no of rows
    do i=1, nx
      rr(:,i) = mm(ny:1:-1,i)
    end do
  end function flipx


  pure function flipy(mm) result(rr)
    character(len=1), intent(in) :: mm(:,:)
    character(len=1) :: rr(size(mm,1), size(mm,2))

    integer :: nx, ny, i
    nx = size(mm,2) ! no of cols
    ny = size(mm,1) ! no of rows
    do i=1, ny
      rr(:,i) = mm(:,ny-i+1)
    end do
  end function flipy


  pure function rotcw(mm) result(rr)
    character(len=1), intent(in) :: mm(:,:)
    character(len=1) :: rr(size(mm,1), size(mm,2))

    integer :: nx, ny, i, j
    nx = size(mm,2) ! no of cols
    ny = size(mm,1) ! no of rows
    do i=1, nx 
    do j=1, ny
      rr(j,i) = mm(nx-i+1,j)
    end do
    end do
  end function rotcw


  pure function rotccw(mm) result(rr)
    character(len=1), intent(in) :: mm(:,:)
    character(len=1) :: rr(size(mm,1), size(mm,2))

    integer :: nx, ny, i, j
    nx = size(mm,2) ! no of cols
    ny = size(mm,1) ! no of rows
    do i=1, nx 
    do j=1, ny
      rr(j,i) = mm(i,ny-j+1)
    end do
    end do
  end function rotccw


  ! ==========================================
  ! Parse patterns from the file into matrices
  ! ==========================================
  pure subroutine line2mat(str, ma, mb)
    character(len=*), intent(in) :: str
    character(len=1), allocatable, intent(out) :: ma(:,:), mb(:,:)

    integer :: p1, p2

    p1 = scan(str,' => ')
    if (p1==0) error stop 'line2mat - invalid format'
    p2 = p1 + 4
    call readmat(strip(str(:p1-1)), ma)
    call readmat(strip(str(p2:)), mb)
  end subroutine line2mat


  pure function strip(str) result(res)
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: res
!
! Remove '/' from the string
!
    character(len=len(str)) :: wrk
    integer :: i, j

    j = 0
    do i=1, len(str)
      if (str(i:i)=='/') cycle
      j = j + 1
      wrk(j:j) = str(i:i)
    end do
    allocate(character(len=j) :: res)
    res = wrk(1:j)
  end function strip


  pure subroutine readmat(str, m)
    character(len=*), intent(in) :: str
    character(len=1), allocatable, intent(out) :: m(:,:)

    integer, parameter :: MAX_N = 5
    integer :: n

    do n=1, MAX_N
     if (n*n == len(str)) exit 
    end do
    if (n==MAX_N+1) error stop 'readmat - length is not n**2'
    allocate(m(n,n))
    read(str,'(*(a1))') m
  end subroutine readmat


  subroutine print_mat(m)
    character(len=1), intent(in) :: m(:,:)

    integer :: i
    do i=1, size(m,1)
      print '(*(a1,1x))', m(i,:)
    end do
  end subroutine print_mat

end module day1721_mod