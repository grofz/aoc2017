program main
  use day1701_mod, only: day1701
  use day1702_mod, only: day1702
  use day1703_mod, only: day1703
  use day1704_mod, only: day1704
  implicit none

  goto 04
  01 call day1701('inp/01/input.txt')
  02 call day1702('inp/02/input.txt')
  03 call day1703()
  04 call day1704('inp/04/input.txt')
end program main
