module day1724_mod
  use parse_mod, only : string_t, read_strings
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none

  type, extends(djikstra_node_at) :: node_t
    integer, pointer :: bridge(:,:)
    logical, allocatable :: used(:)
    integer :: last_port
  contains
    procedure :: nextngb
    procedure :: isequal
    procedure :: istarget
  end type

contains

  subroutine day1724(file)
    character(len=*), intent(in) :: file

    integer, target, allocatable :: bbs(:,:)
    type(string_t), allocatable :: lines(:)
    type(node_t) :: init
    integer :: i, nb, ans(2)

    lines = read_strings(file)
    nb = size(lines)
    allocate(bbs(2,nb))
    do i=1, nb
      bbs(:,i) = parse_line(lines(i)%str)
    end do

    init%bridge => bbs
    allocate(init%used(nb), source=.false.)
    init%last_port = 0

    ! Deep first search is much faster than Djikstra
    !ans = djikstra_way(init)
    call dfs(init%used, 0, bbs, ans, i)

    print '("Answer 24/1 ",i0,l2)' , ans(1), ans(1)==2006
    print '("Answer 24/2 ",i0,l2)' , ans(2), ans(2)==1994
  end subroutine day1724


  recursive subroutine dfs(used, lastport, bbs, ans, maxlen)
    logical, intent(in) :: used(:)
    integer, intent(in) :: lastport
    integer, intent(in) :: bbs(:,:)
    integer, intent(out) :: ans(2), maxlen

    integer :: ans0(2), i, otherport, maxlen0
    logical :: used0(size(used))

    ans = 0
    maxlen = 0
    do i=1, size(used)
      if (used(i)) cycle
      if (bbs(1,i)==lastport) then
        otherport = bbs(2,i)
      else if (bbs(2,i)==lastport) then
        otherport = bbs(1,i)
      else 
        cycle
      end if

      used0 = used
      used0(i) = .true.
      call dfs(used0, otherport, bbs, ans0, maxlen0)

      ! update the best results
      if (ans0(1)>ans(1)) ans(1) = ans0(1)
      if (maxlen0 > maxlen) then
        maxlen = maxlen0
        ans(2) = ans0(2)
      else if (maxlen0 == maxlen .and. ans0(2)>ans(2)) then
        ans(2)=ans0(2)
      end if

    end do

    if (all(ans==0)) then
      ! no more bridges can be added, evaluate result
      maxlen = count(used)
      do i=1,size(used)
        if (used(i)) ans = ans + bbs(1,i) + bbs(2,i)
      end do
    end if
  end subroutine dfs


  function parse_line(str) result(b)
    character(len=*), intent(in) :: str
    integer :: b(2)

    integer :: ip

    ip = scan(str,'/')
    if (ip==0) error stop 'parse_line - / is missing'
    read(str(:ip-1),*) b(1)
    read(str(ip+1:),*) b(2)
  end function parse_line


! Djikstra not needed anymore

  function djikstra_way(init) result(ans)
    type(node_t), intent(in) :: init
    integer :: ans(2)

    integer :: i, w, j, maxlen
    type(djikstra_node_ptr), allocatable :: nodes(:)

    call djikstra_search(nodes, init, w)
    print *, 'nodes =',size(nodes)
    ans(1) = 0
    ans(2) = 0
    maxlen = 1
    do i=1, size(nodes)
      if (nodes(i)%ptr%d < ans(1)) ans(1) = nodes(i)%ptr%d
      select type(nn=>nodes(i)%ptr)
      class is (node_t)
        if (count(nn%used)>=maxlen) then
            ans(2) = nn%d
            maxlen = count(nn%used)
        else if (count(nn%used)==maxlen .and. nn%d<ans(2)) then
            ans(2) = nn%d
        end if
      end select
    end do
    ans = -ans
  end function djikstra_way


  subroutine nextngb(node, flag, node_ngb, distance)
    class(node_t), intent(in) :: node
    integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    integer :: ports_other

    ! next node => find one of unused compatible bridges
    flag = flag + 1
    do flag = flag, size(node%used)
      if (node%used(flag)) cycle
      if (node%bridge(1,flag)==node%last_port) then
        ports_other = node%bridge(2,flag)
      else if (node%bridge(2,flag)==node%last_port) then
        ports_other = node%bridge(1,flag)
      else
        cycle
      end if

      ! bridge at "i" can be used
      allocate(node_ngb, source=node)
      select type(node_ngb)
      class is (node_t)
        node_ngb%last_port = ports_other
        node_ngb%used(flag) = .true.
        distance = -node%bridge(1,flag)-node%bridge(2,flag)
      class default
        error stop 'nextngb - error class'
      end select
      exit
    end do
    ! mark if no more possible bridges exist
    if (flag==size(node%used)+1) flag = 0
  end subroutine nextngb

  logical function isequal(anode, bnode)
    class(node_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode

    select type(bnode)
    class is (node_t)
      isequal = all(anode%used .eqv. bnode%used) .and. anode%last_port==bnode%last_port
    class default
      error stop 'isequal - wrong class'
    end select
  end function isequal

  logical function istarget(node)
    class(node_t), intent(in) :: node
    istarget = .false.
  end function istarget

end module day1724_mod