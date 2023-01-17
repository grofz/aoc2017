module day1719_mod
  use parse_mod, only : read_pattern
  implicit none

! integer, parameter :: MAXPATH = 40
  integer, parameter :: DIR_DOWN=1, DIR_LEFT=2, DIR_UP=3, DIR_RIGHT=4
  integer, parameter :: DIR(2,4) = reshape([1,0, 0,-1, -1,0, 0,1], [2,4])

contains

  subroutine day1719(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: aa(:,:)
    character(len=:), allocatable :: path
    integer :: ans2

    !aa = read_pattern('inp/19/test.txt', .true.)
    aa = read_pattern(file, .true.)
    !call print_aa(aa)
    !print *, size(aa,1), size(aa,2)
    path = travel(aa,ans2)
    print '("Answer 19/1 ",a,l2)', trim(path), trim(path)=='SXPZDFJNRL'
    print '("Answer 19/2 ",i0,l2)', ans2, ans2==18126

  end subroutine day1719


  function travel(aa,counter) result(path)
    character(len=1), intent(in) :: aa(:,:)
    integer, intent(out) :: counter
    character(len=:), allocatable :: path

    integer :: i, j, ni, nj, ij(2), curdir
    integer :: dirleft, dirright, ijleft(2), ijright(2)

    counter = 0
    path = ' '
    ni = size(aa,1)
    nj = size(aa,2)

    ! Find entry point
    do j=1,nj
      if (aa(1,j)=='|') exit
    end do
    curdir = DIR_DOWN
    ij = [1,j]
!print *, 'entry point =', ij

    do
      ij = ij + DIR(:,curdir)
      counter = counter + 1
!print *, ij, curdir
      associate(ch=>aa(ij(1),ij(2)))
      select case(ch)
      case('|','-') ! continue along the current path
        continue

      case('+') ! corner, turn left or right
        dirleft = mod(curdir-1+4-1,4)+1
        dirright = mod(curdir+1-1,4)+1
        ijleft = ij + DIR(:,dirleft)
        ijright = ij + DIR(:,dirright)
        associate(chl=>aa(ijleft(1),ijleft(2)), chr=>aa(ijright(1),ijright(2)))
        if (mod(dirleft,2)==0 .and. mod(dirright,2)==0) then ! left/right
          if (chl=='-' .and. chr==' ') then
            curdir = dirleft
          else if (chr=='-' .and. chl==' ') then
            curdir = dirright
          else
            error stop 'travel - left/right turn undecided'
          end if
        else if (mod(dirleft,2)==1 .and. mod(dirright,2)==1) then ! up/down
          if (chl=='|' .and. chr==' ') then
            curdir = dirleft
          else if (chr=='|' .and. chl==' ') then
            curdir = dirright
          else
            error stop 'travel - up/down turn undecided'
          end if
        else
          error stop 'travel - wrong turn'
        end if
        end associate

      case (' ') ! end of route
        exit

      case default ! letter, mark path
        if (iachar(ch)<iachar('A') .or. iachar(ch)>iachar('Z')) then
          print *, ij, '~'//ch//'~'
          error stop 'invalid character'
        end if
        path = trim(path)//ch
!print *, 'path added ', ch, len_trim(path)
      end select
      end associate
    end do
  end function travel


  subroutine print_aa(aa)
    character(len=1), intent(in) :: aa(:,:)

    integer :: i, j, x, y

    x = size(aa,1)
    y = size(aa,2)
    do i=1,x
      print '(*(a))', aa(i,:)
    end do
  end subroutine print_aa

end module day1719_mod