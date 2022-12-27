module day1705_mod
  use parse_mod, only : read_numbers
  implicit none

contains

  subroutine day1705(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: aa(:)
    integer :: ans1, ans2
    integer, parameter :: PART1=1, PART2=2

    aa = read_numbers(file)
    !aa = [0, 3, 0, 1, -3]

    call jump(aa, PART1, ans1)
    print '("Answer 5/1 ",i0,l2)', ans1, ans1==339351
    call jump(aa, PART2, ans2)
    print '("Answer 5/2 ",i0,l2)', ans2, ans2==24315397

  end subroutine day1705



  subroutine jump(aa, rule, ans)
    integer, intent(in) :: rule, aa(:)
    integer, intent(out) :: ans

    integer :: ijump, ind
    integer :: aa0(size(aa))

    ans = 0
    aa0 = aa
    ind = 1
    do
      ijump = aa0(ind) 
      select case(rule)
      case(1)
        aa0(ind) = aa0(ind) + 1
      case(2)
        if (ijump >= 3) then
          aa0(ind) = aa0(ind) - 1
        else
          aa0(ind) = aa0(ind) + 1
        end if
      case default
        error stop 'invalid branch'
      end select
      ind = ind + ijump
      ans = ans + 1
      if (ind < 1 .or. ind > size(aa)) exit
    end do

  end subroutine jump

end module day1705_mod