module day1702_mod
  use parse_mod, only : read_strings, split_nonempty, string_t
  implicit none
  private
  public day1702

contains

  subroutine day1702(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(string_t), allocatable :: nums(:)
    integer :: i, j, ans1, ans2
    integer, allocatable :: arr(:)

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      call split_nonempty(lines(i)%str, achar(9), nums)
      if (allocated(arr)) deallocate(arr)
      allocate(arr(size(nums)))
      do j=1, size(nums)
        read(nums(j)%str,*) arr(j)
      end do
      ans1 = ans1 + maxval(arr) - minval(arr)
      ans2 = ans2 + part2(arr)
    end do
    print '("Answer 2/1 ",i0,l2)', ans1, ans1==43074
    print '("Answer 2/2 ",i0,l2)', ans2, ans2==280
  end subroutine day1702



  integer function part2(arr) result(res)
    integer, intent(in) :: arr(:)

    integer :: i, j, ij(2)

    ij = -1
    do i=1,size(arr)
    do j=1,size(arr)
      if (i==j) cycle
      if (mod(arr(i),arr(j))/=0) cycle
      if (any(ij/=-1)) error stop 'more than one suitable pair'
      ij = [i, j]
    end do
    end do
    res = arr(ij(1)) / arr(ij(2))
  end function part2

end module day1702_mod