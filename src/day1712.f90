module day1712_mod
  use parse_mod, only : split, string_t, read_strings
  implicit none

  type node_t
    integer :: id = -1
    integer, allocatable :: ns(:)
    integer :: ilab = -1
  end type

contains

  subroutine day1712(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(node_t), allocatable :: nodes(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    allocate(nodes(0:size(lines)-1))
    do i=1, size(nodes)
      nodes(i-1) = node_new(lines(i)%str)
    end do

    ! What is size of the group containing program "0"
    call flood_fill(nodes, 0, 1)
    ans1 = count(nodes(:)%ilab==1)
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==306

    ! Label the remaining groups (group 1 already labeled)
    ans2 = 1
    do i=0,size(nodes)-1
      if (nodes(i)%ilab > 0) cycle
      ! the node is a member of new group
      ans2 = ans2 + 1
      call flood_fill(nodes, i, ans2)
    end do
    print '("Answer 12/2 ",i0,l2)', ans2, ans2==200
  end subroutine day1712


  type(node_t) function node_new(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: ngbs(:)
    integer :: ios, i

    read(str,*,iostat=ios) new%id
    if (ios/=0) error stop 'new - id reading error'
    i = scan(str, '<-> ')
    if (i==0) error stop 'new - delimiter not present'
    call split(str(i+4:),',',ngbs)
    allocate(new%ns(size(ngbs)))
    do i=1, size(ngbs)
      read(ngbs(i)%str,*,iostat=ios) new%ns(i)
      if (ios/=0) error stop 'new - ngb reading error'
    end do
!print *, 'id=',new%id, 'ngbs= ', new%ns
  end function node_new


  recursive subroutine flood_fill(nodes, id, ilab)
    type(node_t), intent(inout) :: nodes(0:)
    integer, intent(in) :: id, ilab

    integer :: i

    if (nodes(id)%id /= id) error stop 'flood_fill - array shift'
    if (nodes(id)%ilab == ilab) return
    if (nodes(id)%ilab >= 0) error stop 'flood_fill - already labeled'
    nodes(id)%ilab = ilab
    do i=1, size(nodes(id)%ns)
      call flood_fill(nodes, nodes(id)%ns(i), ilab)
    end do
  end subroutine flood_fill

end module day1712_mod