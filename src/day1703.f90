module day1703_mod
  implicit none

contains

  subroutine day1703()
    integer, parameter :: inp=361527

    integer :: i, j, v, n, s
    integer, allocatable :: aa(:,:)

    ! Part 1
    call botrig(inp, i, j)
    print '("Answer 3/1 ",i0,l2)', abs(i)+abs(j), abs(i)+abs(j)==326

    ! Part 2
    n = max(abs(i),abs(j))
    allocate(aa(-n-1:n+1, -n-1:n+1))
    aa = 0
    aa(0,0)=1
    do v = 2, inp
      call botrig(v, i, j)
      s = sum(aa(i-1:i+1,j-1:j+1))
      aa(i,j) = s
      if (s > inp) exit
    end do
    print '("Answer 3/2 ",i0,l2)', s, s==363010
  end subroutine day1703



  subroutine botrig(v, i, j)
    integer, intent(in) :: v
    integer, intent(out) :: i, j
!
! Co-ordinates of spiral memory value
!
    integer :: v0, n, r, over, tmp
    
    i = 0
    j = 0
    if (v==1) return ! the center is a special case

    ! In circles, monitor the position of the right bottom corner until
    ! the input value is overshot
    v0 = 0
    do
      v0 = v0 + cfence(i) 
      if (v0 >= v) exit
      i = i+1
    end do

    ! "i" is also a halfway of a side distance
    if (i /= cfence(i)/8 .or. mod(cfence(i),8)/=0) error stop 'consistency check'

    ! Back-track from "v0" to the input value, moving clockwise
    over = v0-v
    n = over/i
    r = mod(over,i)
    ! Moving clocwise from bottom-right, decreasing memory
    ! "n" - 0, 2, 4, 6 -> on a way from a corner to the middle of side
    ! "n" - 1, 3, 5, 7 -> on a way from the middle of side to a corner
    if (mod(n,2)==0) then
      ! "J" will equal "i" if "v" is on the corner
      j = i - r
    else
      ! "j" will be zero if "v" is in the middle of side
      j = r
    end if

    ! Now identify the coordinates properly
    ! Positive direction of vertical coordinate is bottom
    ! Positive direction of horizontal coordinate is right
    !
    !  # > 4 > # > 5 > #
    !  ^               v
    !  3               6
    !  ^               v
    !  #               #
    !  ^               v
    !  2               7
    !  ^               v
    !  # < 1 < # < 0 < S
    !
    select case(n)
    case(0)
      i = -i
    case(1)
      i = -i
      j = -j
    case(2)
      tmp = i
      i = -j
      j = -tmp
    case(3)
      tmp = i
      i = j
      j = -tmp
    case(4)
      j = -j
    case(5)
    case(6)
      tmp = i
      i = j
      j = tmp
    case(7)
      tmp = i
      i = -j
      j = tmp
    case default
      error stop
    end select
  end subroutine botrig



  pure integer function cfence(i) result(c)
    integer, intent(in) :: i
!
! Number of elements of a circle with bottom right
! corner at [i, -i]
!
    integer :: c1 
    if (i<0) error stop 'only positive integers'
    if (i==0) then
      c = 1
      return
    end if
    c1 = 2*i+1
    c = 2*c1 + 2*(c1-2)
  end function cfence

end module day1703_mod