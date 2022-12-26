module day1701_mod
  use iso_fortran_env, only : iostat_end, iostat_eor
  implicit none
  private
  public day1701

contains

  function read_arr(file) result(arr)
    character(len=*), intent(in) :: file
    integer, allocatable :: arr(:)
!
! Extract single long line of single digit numbers into an array
!
    character(len=1) :: ch
    integer :: ios, n, i, fid

    ! count number of characters on a line
    n = 0
    open(newunit=fid, file=file, status='old')
    do
      read(fid, '(a1)', advance='no', iostat=ios) ch  
      if (ios==iostat_eor .or. ios==iostat_end) exit
      n = n + 1
    end do

    ! fill the array
    allocate(arr(n))
    backspace(fid)
    do i=1,n
      read(fid, '(a1)', advance='no', iostat=ios) ch  
      if (ios==iostat_eor .or. ios==iostat_end) error stop 'inconsistency'
      read(ch,'(i1)',iostat=ios) arr(i)
      if (ios/=0) error stop 'conversion error'
    end do
    close(fid)
  end function read_arr



  subroutine day1701(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: arr(:)
    integer :: i, inext, ans1, ans2

    arr = read_arr(file)
    if (mod(size(arr),2) /= 0) error stop 'odd number of elements'
    ans1 = 0
    ans2 = 0
    do i = 1, size(arr)
      inext = modulo(i, size(arr)) + 1
      if (arr(i) == arr(inext)) ans1 = ans1 + arr(i)
      inext = modulo(i+size(arr)/2-1, size(arr)) + 1
      if (arr(i) == arr(inext)) ans2 = ans2 + arr(i)
    end do
    print '("Answer 1/1 ",i0,l2)', ans1, ans1==1390
    print '("Answer 1/2 ",i0,l2)', ans2, ans2==1232
  end subroutine day1701

end module day1701_mod