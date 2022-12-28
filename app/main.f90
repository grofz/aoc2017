program main
  use day1701_mod, only: day1701
  use day1702_mod, only: day1702
  use day1703_mod, only: day1703
  use day1704_mod, only: day1704
  use day1705_mod, only: day1705
  use day1706_mod, only: day1706
  use day1707_mod, only: day1707
  use day1708_mod, only: day1708
  implicit none

  goto 01
  01 call day1701('inp/01/input.txt')
  02 call day1702('inp/02/input.txt')
  03 call day1703()
  04 call day1704('inp/04/input.txt')
  05 call day1705('inp/05/input.txt')
  06 call day1706('inp/06/input.txt')
  07 call day1707('inp/07/input.txt')
  08 call day1708('inp/08/input.txt')
end program main
