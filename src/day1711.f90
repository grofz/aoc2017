module day1711_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none

  character(len=2), parameter :: CHDIRS(6) = ['nw','n ','ne','sw','s ','se']
  integer, parameter :: DIRS(2,6) = reshape([-1,-1, 0,-2, 1,-1, -1,1, 0,2, 1,1], [2,6])

contains

  subroutine day1711(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: line(:), words(:)
    integer :: i, j, x(2), ans1, ans2

    line = read_strings(file)
    if (size(line)/=1) error stop 'day11 - just single line expected'
    call split(line(1)%str, ',', words)
    x = 0
    ans2 = 0
    do i=1,size(words)
      j = findloc(CHDIRS, words(i)%str, 1)
      if (j<1 .or. j>6) error stop 'unknown movement'
      x = x + DIRS(:,j)
      if (maxval(abs(x))>ans2) ans2 = maxval(abs(x))
    end do
    ans1 = maxval(abs(x))
    print '("Answer 11/1 ",i0,l2)', ans1, ans1==705
    print '("Answer 11/2 ",i0,l2)', ans2, ans2==1469

  end subroutine day1711

end module day1711_mod