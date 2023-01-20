module day1722_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter :: DIRS(2,0:3) = reshape([-1,0, 0,1, 1,0, 0,-1], [2,4])
  integer, parameter :: RIGHT_TURN = 1, LEFT_TURN = 3, REVERSE = 2
  character(len=1), parameter :: CH_INFECTED='#', CH_CLEAN='.', CH_WEAKEN='W', CH_FLAG='F'

  type grid_t
    integer :: pos(2)
    integer :: ori
    character(len=1), allocatable :: map(:,:)
    integer :: count_inf=0
    integer :: mode=1
  contains
    procedure :: turn => grid_turn, expand => grid_expand
    procedure :: burst => grid_burst
  end type

contains

  subroutine day1722(file)
    character(len=*), intent(in) :: file

    type (grid_t) :: grid, grid2
    integer, parameter :: NREP = 10000, NREP2 = 10000000
    integer :: i

    grid = grid_new(file)
    !call grid_print(grid)
    do i=1, NREP
      call grid%burst()
    end do
    print '("Answer 22/1 ",i0,l2)', grid%count_inf, grid%count_inf==5256

    ! Part 2
    grid2 = grid_new(file,2)
    do i=1, NREP2
      call grid2%burst()
    end do
    print '("Answer 22/2 ",i0,l2)', grid2%count_inf, grid2%count_inf==2511345
    
  end subroutine day1722


  subroutine grid_burst(th)
    class(grid_t), intent(inout) :: th

    associate(ch => th%map(th%pos(1),th%pos(2)))
      select case(th%mode)
      case(1) ! Part 1 rules
        if (ch==CH_INFECTED) then
          call th%turn(RIGHT_TURN)
          ch = CH_CLEAN
        else if (ch==CH_CLEAN) then
          call th%turn(LEFT_TURN)
          ch = CH_INFECTED
          th%count_inf = th%count_inf + 1
        else
          error stop 'burst invalid map'
        end if

      case(2) ! Part 2 rules
        select case(ch)
        case(CH_CLEAN)
          call th%turn(LEFT_TURN)
          ch = CH_WEAKEN
        case(CH_WEAKEN)
          ch = CH_INFECTED
          th%count_inf = th%count_inf + 1
        case(CH_INFECTED)
          call th%turn(RIGHT_TURN)
          ch = CH_FLAG
        case(CH_FLAG)
          call th%turn(REVERSE)
          ch = CH_CLEAN
        case default
          error stop 'burst 2 - invalid map'
        end select
      
      case default
        error stop 'burst - invalid mode'
      end select
    end associate
    th%pos = th%pos + DIRS(:,th%ori)
    call th%expand()
  end subroutine grid_burst


  type(grid_t) function grid_new(file, mode) result(new)
    character(len=*), intent(in) :: file
    integer, intent(in), optional :: mode

    integer :: ni, nj

    new%map = read_pattern(file)
    ni = size(new%map, 1)
    nj = size(new%map, 1)
    if (ni /= nj) error stop 'new - must be square'
    if (mod(ni-1,2)/=0) error stop 'new - size must be odd'
    new%pos = (ni-1)/2+1
    new%ori = 0
    if (present(mode)) new%mode = mode
  end function grid_new


  subroutine grid_turn(th, iturn)
    class(grid_t), intent(inout) :: th
    integer, intent(in) :: iturn

    th%ori = mod(th%ori + iturn, 4)
  end subroutine grid_turn


  subroutine grid_expand(th)
    class(grid_t), intent(inout) :: th
!
! Expand if needed, otherwise does nothing
!
    integer :: ni, next
    character(len=1), allocatable :: ext(:,:)

    next = 0
    ni = size(th%map, 1)
    if (ni /= size(th%map,2)) error stop 'expand - must be square'
    if (any(th%pos<=1) .or. any(th%pos>=ni)) then
      next = (ni-1)/2
      allocate(ext(2*next+ni, 2*next+ni), source=CH_CLEAN)
      ext(next+1:next+ni, next+1:next+ni) = th%map
      call move_alloc(ext, th%map)
      th%pos = th%pos + next
    end if
  end subroutine grid_expand


  subroutine grid_print(th)
    class(grid_t), intent(in) :: th

    integer :: x0, x1, y0, y1, i

    x0 = lbound(th%map,1)
    x1 = ubound(th%map,1)
    do i = x0, x1
      print '(*(a1,1x))', th%map(i,:)
    end do
  end subroutine grid_print

end module day1722_mod