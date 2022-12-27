! CONTENT
! module rbtr_module
! *function parent(a,n) result (p)
! *function grandparent(a,n) result (g)
! *function sibling(a,n) result (s)
! *function uncle(a,n) result (u)
! *subroutine rotate_left(a,n)
! *subroutine rotate_right(a,n)
! function rbtr_create(dat) result (a)
! function rbtr_free(a) result (ires)
! function rbtr_nodes(a) result(inod)
! function rbtr_inse(a,dat,cfun,id) result (ires)
! *recursive subroutine insert_recurse(a, root, n, cfun)
! *recursive subroutine insert_repair_tree(a,n)
! function rbtr_gofs(a) result(ires)
! function rbtr_gols(a) result(ires)
! function rbtr_next(a) result(ires)
! function rbtr_prev(a) result(ires)
! *function successor(a,n) result(next)
! *function predecessor(a,n) result(prev)
! function rbtr_find(a,key,cfun,inext) result(ires)
! function rbtr_read(a,id) result (dat)
! subroutine rbtr_veri(a,ierr,iheight) 
! *recursive subroutine subtree_check(a, t, isvalid, i)
! function rbtr_dele(a) result(ires)  
! *subroutine delete_case1(a,m)
! *subroutine delete_case2(a,m)
! *subroutine delete_case34(a,m)
! *subroutine delete_case5(a,m)
! *subroutine delete_case6(a,m)
!
! (*) is private routine 
!
!
! PROPERTIES OF RED-BLACK TREE (RB-tree is a self-balancing binary search tree)
! 1. each node is red or black
! 2. root node is black
! 3. all leaves (nil nodes) are black
! 4. if a node is red, then both its children are black
! 5. every path from a given node to any of its descendant NIL nodes contains the
!    same number of black nodes
!
! Insert / Delete / Search operations time complexity is O(log(N))
! Duplicity keys are not allowed in this implementation
!
! BUGS / TODO
! * Insert and search operations use user supplied function CFUN using transform
!   statement to access to node data. This slows the running time. A parameter
!   should be added to RBTR structure indicating whether external function CFUN
!   or direct comparison of integer values of ID is used by insert and search
!   operations
!
! * Add an option to not deallocate the deleted node and its "data" array to rbtr_dele
!   subroutine, it should return pointer to the deleted node instead. 
!   Also add an option to not allocate the new node and its "data" array to rbtr_inse
!   subroutine, it should take an pointer to the "free" node instead
!   This will reduce (I believe) the running time (skiping deallocation/allocation calls)
!   in cases when the key value changes - and the node must be removed from and then
!   reinserted to the tree.
!
! VERSION 1.0 (May/June 2019)
! Algorithms are based on code snippets from Wikipedia "red-black trees" page
!
! -------------------------------------------------------------------------------
  module rbtr_module
! -------------------------------------------------------------------------------
  !use nrtype
  use iso_fortran_env, only : i4b=>int32, i1b=>int8  
  implicit none

  private
  public rbtr, rbtd, rbtr_create, rbtr_free, rbtr_nodes
  public rbtr_inse, rbtr_gofs, rbtr_gols, rbtr_next, rbtr_prev, rbtr_find, rbtr_read
  public rbtr_dele, rbtr_veri

  public rbtr_test, rbtr_test2 

  ! A public variable used as a MOLD for transfer
  integer(i4b), allocatable :: rbtd(:)

  integer(i1b), parameter :: red_color = 1, black_color = 0

  type :: rbtr ! RB-tree
    private
    type(rbtn), pointer :: root => null()
    type(rbtn), pointer :: nil  => null() 
    type(rbtn), pointer :: current => null()
    integer(i4b) :: nodes = 0
  end type rbtr

  type rbtn
    private
    type(rbtn), pointer :: left   => null()
    type(rbtn), pointer :: right  => null()
    type(rbtn), pointer :: parent => null()
    integer(i1b) :: color
    integer(i4b), pointer :: data(:) => null()
    integer(i4b) :: id
  end type rbtn

  abstract interface 
    function cfun_ai(a,b) result (i)          ! return  1 if "a-key" < "b-key"
      import i4b                              ! return -1 if "a-key" > "b-key"
      integer(i4b), intent(in) :: a(:), b(:)  ! return  0 if "a-key" = "b-key"
      integer(i4b) :: i
    end function
  end interface 

  contains

! -------------------------------------------------------------------------------
  function parent(a,n) result (p)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n, p
  
  if (associated(a % nil, n)) then ! "n" is already a NIL node
    p => n                         ! return NIL node 
    print *, 'WARNING parent: not expected this part of code will ever run'
  else
    p => n % parent
  endif
! -------------------------------------------------------------------------------
  end function parent
! -------------------------------------------------------------------------------
  


! -------------------------------------------------------------------------------
  function grandparent(a,n) result (g)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n, g, p

  p => parent(a,n)
  if (associated(a % nil, p)) then ! there is no parent
    g => p                         ! return NIL node
  else
    g => parent(a,p)
  endif
! -------------------------------------------------------------------------------
  end function grandparent
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function sibling(a,n) result (s)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n, s, p

  p => parent(a,n)
  if (associated(a % nil, p)) then ! there is no parent
    s => p                         ! return NIL node
  else
    if (associated(n, p % left)) then
      s => p % right
    elseif (associated(n, p % right)) then  ! TODO this elseif can be else
      s => p % left                         ! after testing is done
    else
      print *, 'sibling: ERROR defensive check failed'
      stop
    endif
  endif
! -------------------------------------------------------------------------------
  end function sibling
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function uncle(a,n) result (u)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n, p, g, u

  p => parent(a,n)
  g => grandparent(a,n)
  if (associated(a % nil, g)) then ! there is no grandparent
    u => g                         ! return NIL node
  else
    u => sibling(a,p)
  endif
! -------------------------------------------------------------------------------
  end function uncle
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine rotate_left(a,n)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n     ! "n" is PIVOT, its right child is ROTATOR
  type(rbtn), pointer :: nnew, p

  p => parent(a,n) ! "p" is PIVOT's parent, it will be relinked to ROTATOR

  if (associated(n % right, a % nil)) then         ! no right child of pivot 
    print *,'rotate_left ERROR - rotator is leaf' 
    stop
  endif

  nnew => n % right ! "nnew" is ROTATOR, its left child is INSIDE subtree

  n % right => nnew % left ! relink INSIDE as the right child of PIVOT

  nnew % left => n     ! relink PIVOT as the left child of ROTATOR
  n % parent => nnew   ! ... and ROTATOR is now PIVOT's parent

  if (.not. associated(n % right, a % nil)) then ! INSIDE subtree is not empty
    n % right % parent => n ! PIVOT is now INSIDE's parent
  endif

  if (associated(p, a % nil)) then ! PIVOT was root node 
    nnew % parent => a % nil
    a % root => nnew ! now ROTATOR is the root node
  else                              
    ! replace parent <-> PIVOT link to parent <-> ROTATOR link
    if (associated(n, p % left)) then ! PIVOT was left child
      p % left => nnew
    else                              ! PIVOT was right child
      p % right => nnew
    endif
    nnew % parent => p
  endif
! -------------------------------------------------------------------------------
  end subroutine rotate_left
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine rotate_right(a,n)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n     ! "n" is PIVOT, its left child is ROTATOR
  type(rbtn), pointer :: nnew, p

  p => parent(a,n) ! "p" is PIVOT's parent, it will be relinked to ROTATOR

  if (associated(n % left, a % nil)) then         ! no left child of pivot 
    print *,'rotate_right ERROR - rotator is leaf' 
    stop
  endif

  nnew => n % left ! "nnew" is ROTATOR, its right child is INSIDE subtree

  n % left => nnew % right ! relink INSIDE as the left child of PIVOT

  nnew % right => n    ! relink PIVOT as the right child of ROTATOR
  n % parent => nnew   ! ... and ROTATOR is now PIVOT's parent

  if (.not. associated(n % left, a % nil)) then ! INSIDE subtree is not empty
    n % left % parent => n ! PIVOT is now INSIDE's parent
  endif

  if (associated(p, a % nil)) then ! PIVOT was root node 
    nnew % parent => a % nil
    a % root => nnew ! now ROTATOR is the root node
  else                              
    ! replace parent <-> PIVOT link to parent <-> ROTATOR link
    if (associated(n, p % left)) then ! PIVOT was left child
      p % left => nnew
    else                              ! PIVOT was right child
      p % right => nnew
    endif
    nnew % parent => p
  endif
! -------------------------------------------------------------------------------
  end subroutine rotate_right
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_create(dat) result (a)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  integer(i4b), intent(in) :: dat(:)
  nullify(a % root)
  nullify(a % current)
  allocate(a % nil)
  allocate(a % nil % data(size(dat)))
  a % nil % id = -1
  a % nil % color = black_color
  a % nodes = 0
! -------------------------------------------------------------------------------
  end function rbtr_create
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_free(a) result (ires)
! -------------------------------------------------------------------------------
! USE: Free tree from the memory. WARNING - data must be deallocated by the user
!      prior to running this procedure. Here only "rbtn" and "rbtn % data" 
!      pointers are deallocated
!
  type(rbtr) :: a
  integer(i4b) :: ires ! 0 = all ok
                       ! 1 = all ok, but tree was not empty
                       !-5 = error, something went wrong
  integer(i4b) :: j

  ires = 0
  if (a % nodes /= 0) then     ! tree is not empty
    ires = 1                     ! delete all nodes from the tree
    do 
      a % current => a % root
      j = rbtr_dele(a)
      if (j==2) exit
      if (j==0) then
        a % current => a % root
        cycle
      endif
      print *, 'rbtr_free - something went wrong j =',j
      ires = -5
      return
    enddo
  endif

  deallocate(a % nil % data) ! tree is now empty, deallocate NIL node
  deallocate(a % nil)
  nullify(a % root)
  nullify(a % current)
  if (a % nodes /= 0) ires = -1
! -------------------------------------------------------------------------------
  end function rbtr_free
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_nodes(a) result(inod)
! -------------------------------------------------------------------------------
  integer(i4b) :: inod
  type(rbtr) :: a
  inod = a % nodes
! -------------------------------------------------------------------------------
  end function rbtr_nodes
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_inse(a,dat,cfun,id) result (ires)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  integer(i4b), intent(in) :: dat(:)
  integer(i4b), optional :: id
  integer(i4b) :: ires
  procedure(cfun_ai) :: cfun

  integer(i4b) :: idi
  type(rbtn), pointer :: new

 !interface 
 !  function cfun(a,b) result (i)
      !use nrtype
 !    import i4b
 !    integer(i4b), intent(in) :: a(:), b(:)
 !    integer(i4b) :: i
 !  end function
 !end interface 

! -
  idi = 0
  if (present(id)) idi = id
  a % nodes = a % nodes + 1

  ! prepare new node to be inserted to a tree
  allocate(new)
  allocate(new % data(size(dat)))
  new % data = dat
  new % id = idi
  new % left => a % nil
  new % right => a % nil

  if (a % nodes == 1) then ! special case if it is the first node
    new % parent => a % nil
    new % color = black_color ! root node is BLACK
    a % root => new
    ires = 1
    return
  else
    new % color = red_color ! inserted node is RED
  endif

  ! insert new node into the current tree
  call insert_recurse(a, a % root, new, cfun)

  ! repair tree 
  call insert_repair_tree(a,new)

  ires = 0
! -------------------------------------------------------------------------------
  end function rbtr_inse
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  recursive subroutine insert_recurse(a, root, n, cfun)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: root, n
  procedure(cfun_ai) :: cfun

 !interface 
 !  function cfun(a,b) result (i) ! return  1 if "a-key" < "b-key"
 !    !use nrtype                  ! return -1 if "a-key" > "b-key"
 !    import i4b
 !    integer(i4b), intent(in) :: a(:), b(:)  ! return  0 if "a-key" = "b-key"
 !    integer(i4b) :: i
 !  end function
 !end interface 

  integer(i4b) :: i

! -
  if (associated(root, a % nil)) then
    print *, 'insert_recurse: ERROR - this should not happen'
    stop
  endif

  i = cfun(n % data, root % data)
  select case(i)
  case(1) ! insert node to the left from the root
    if (associated(a % nil, root % left)) then
      root % left => n
    else
      call insert_recurse(a, root % left, n, cfun)
      return
    endif

  case(-1) ! insert node to the right from the root
    if (associated(a % nil, root % right)) then
      root % right => n
    else
      call insert_recurse(a, root % right, n, cfun)
      return
    endif

  case(0)  ! node with the same key exists in tree
    print *, 'rbtr_insert: SORRY, no duplicit keys allowed'
    stop
    ! TODO better error dealing, function should recover without inserting the
    ! new node
    ! TODO make variant that allows duplicit keys

  case default
    print *, 'rbtr_insert: ERROR - cfun should return 0 or +1/-1 only'
    stop
  end select

  n % parent => root ! insert new node
! -------------------------------------------------------------------------------
  end subroutine insert_recurse
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  recursive subroutine insert_repair_tree(a,n)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: n, p, u, g
  logical :: uncle_exists, uncle_is_red

  p => parent(a,n)
  if (associated(p,a % nil)) then
    n % color = black_color ! case 1 - root node must be black
  elseif (p % color == black_color) then
    continue                ! case2 - nothing must be done
  else ! parent is red
    u => uncle(a,n)
    uncle_exists = .not. associated(u, a % nil)
    if (uncle_exists) uncle_is_red = u % color == red_color
    if (uncle_exists .and. uncle_is_red) then
      ! case3 - repaint parent and uncle black and rerun on the grandparent
      g => grandparent(a,n)
      p % color = black_color
      u % color = black_color
      g % color = red_color
      call insert_repair_tree(a,g)
    else
      ! case 4 - parent is red and uncle is black
      g => grandparent(a,n)

      ! case 4, step 1
      if (associated(n, p % right) .and. associated(p, g % left)) then
        call rotate_left(a,p)
        n => n % left
      elseif (associated(n, p % left) .and. associated(p, g % right)) then
        call rotate_right(a,p)
        n => n % right
      endif

      ! case 4, step 2
      p => parent(a,n)
      g => grandparent(a,n)

      if (associated(n, p % left)) then
        call rotate_right(a,g)
      else
        call rotate_left(a,g)
      endif
      p % color = black_color
      g % color = red_color
    endif
  endif
! -------------------------------------------------------------------------------
  end subroutine insert_repair_tree
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_gofs(a) result(ires)
! -------------------------------------------------------------------------------
! USE: Points the current pointer to the leftmost node in the tree
!      If there is no node, current pointer is nullified
!
  type(rbtr) :: a
  integer(i4b) :: ires ! 0 = ok
                       ! 1 = no leftmost node
! type(rbtn), pointer :: left
  integer(i4b) :: isaf

  ! -
  
  if (.not. associated(a % root)) then
    nullify(a % current)
    ires = 1
  else
    a % current => a % root
    isaf = 0
    do while(.true.)
      ires = 0
      if (associated(a % current % left, a % nil)) exit
      a % current => a % current % left
      isaf = isaf + 1
      if (mod(isaf,1000)==0) then
        print *, 'rbtr_gofs: WARNING - seems trapped in an endless loop'
      endif
    enddo
  endif
! -------------------------------------------------------------------------------
  end function rbtr_gofs
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_gols(a) result(ires)
! -------------------------------------------------------------------------------
! USE: Points the current pointer to the rightmost node in the tree
!      If there is no node, current pointer is nullified
!
  type(rbtr) :: a
  integer(i4b) :: ires ! 0 = ok
                       ! 1 = no rightmost node
  integer(i4b) :: isaf

  ! -
  
  if (.not. associated(a % root)) then
    nullify(a % current)
    ires = 1
  else
    a % current => a % root
    isaf = 0
    do while(.true.)
      ires = 0
      if (associated(a % current % right, a % nil)) exit
      a % current => a % current % right
      isaf = isaf + 1
      if (mod(isaf,1000)==0) then
        print *, 'rbtr_gols: WARNING - seems trapped in an endless loop'
      endif
    enddo
  endif
! -------------------------------------------------------------------------------
  end function rbtr_gols
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_next(a) result(ires)
! -------------------------------------------------------------------------------
! USE: Points the current pointer on the successor node.
!      If there is no successor node the pointer is nullified.

  type(rbtr)   :: a
  integer(i4b) :: ires ! 0 = ok
                       ! 1 = no current pointer
                       ! 2 = current pointer is last pointer

  type(rbtn), pointer :: next

  ! -
  if (.not. associated(a % current)) then
    ires = 1
    return
  endif

  next => successor(a, a % current)
  if (associated(next, a % nil)) then
    ires = 2
    nullify(a % current)
  else
    ires = 0
    a % current => next
  endif
  return

! OLD CODE AS A BACKUP
! if (.not. associated(a % current % right, a % nil)) then
!                                         ! the current node has a right child
!   next => leftmost(a % current % right)
! else
!                                         ! no right child of current node
!   next => parent(a, a % current)
!   if (associated(next,a % nil)) then
!                                         ! current node has no parent
!     ires = 2 ! no right child and no parent -> this is the last node
!   else
!     if (associated(next % left, a % current)) then
!                                         ! current node is the left child
!       continue ! -> next node is the parent
!     else
!                                         ! current node is the right child
!       ! then we go up until finding a node that is the left child or 
!       ! the root node
!       old => next
!       do while(.true.)
!         next => parent(a, next)
!         if (associated(next, a % nil)) then
!                                         ! root node was reached
!           ires = 2
!           exit
!         else
!           if (associated(next % left, old)) then
!                                         ! "next" is parent of a left child
!             exit
!           else
!                                         ! "next is parent of a right child
!             old => next ! keep looking
!           endif
!         endif
!       enddo
!     endif
!   endif
! endif
! if (ires == 2) then
!   nullify(a % current)
! else
!   a % current => next
! endif
! contains
! function leftmost(n)
!   type(rbtn), pointer :: n, leftmost
!   leftmost => n
!   do while(.true.)
!     if (associated(leftmost % left, a % nil)) exit
!     leftmost => leftmost % left 
!   enddo
! end function leftmost
! END OLD CODE BACKUP

! -------------------------------------------------------------------------------
  end function rbtr_next
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_prev(a) result(ires)
! -------------------------------------------------------------------------------
! USE: Points the current pointer on the predecessor node.
!      If there is no predecessor node the pointer is nullified.

  type(rbtr)   :: a
  integer(i4b) :: ires ! 0 = ok
                       ! 1 = no current pointer
                       ! 2 = current pointer is first pointer

  type(rbtn), pointer :: prev

  ! -
  if (.not. associated(a % current)) then
    ires = 1
    return
  endif

  prev => predecessor(a, a % current)
  if (associated(prev, a % nil)) then
    ires = 2
    nullify(a % current)
  else
    ires = 0
    a % current => prev
  endif
! -------------------------------------------------------------------------------
  end function rbtr_prev
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function successor(a,n) result(next)
! -------------------------------------------------------------------------------
! USE: The result points either to the successor node of N or to the NIL node 
!      if there is no successor 

  type(rbtr) :: a
  type(rbtn), pointer :: n, next, old

  if (associated(n, a % nil)) then ! N is NIL -> result is NIL as well
    next => a % nil
    return
  endif

  if (.not. associated(n % right, a % nil)) then
                                ! THE CURRENT NODE HAS A RIGHT CHILD
    next => leftmost(n % right)
  else                          ! NO RIGHT CHILD OF CURRENT NODE
    next => parent(a, n)
    if (associated(next,a % nil)) then  ! current node has no parent
      continue   ! no right child and no parent -> this is the last node
    else
      if (associated(next % left, n)) then ! current node is the left child
        continue                           ! -> next node is the parent
      else                                 ! current node is the right child
        ! then we go up until finding a node that is the left child or 
        ! the root node
        old => next
        do 
          next => parent(a, next)
          if (associated(next, a % nil)) then ! root node was reached
            exit    ! -> this was the last node, "next" points to NIL
          else
            if (associated(next % left, old)) then
              exit                         ! -> "next" is parent of a left child
            else                           ! "next" is parent of a right child
              old => next                  ! -> keep looking
            endif
          endif
        enddo
      endif
    endif
  endif
  
  contains
  function leftmost(n)
    type(rbtn), pointer :: n, leftmost
    leftmost => n
    do 
      if (associated(leftmost % left, a % nil)) exit
      leftmost => leftmost % left 
    enddo
  end function leftmost
! -------------------------------------------------------------------------------
  end function successor
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function predecessor(a,n) result(prev)
! -------------------------------------------------------------------------------
! USE: The result points either to the predecessor node of N or to the NIL node 
!      if there is no predecessor 

  type(rbtr) :: a
  type(rbtn), pointer :: n, prev, old

  if (associated(n, a % nil)) then ! N is NIL -> result is NIL as well
    prev => a % nil
    return
  endif

  if (.not. associated(n % left, a % nil)) then
                                ! THE CURRENT NODE HAS A LEFT CHILD
    prev => rightmost(n % left)
  else                          ! NO LEFT CHILD OF CURRENT NODE
    prev => parent(a, n)
    if (associated(prev,a % nil)) then  ! current node has no parent
      continue   ! no left child and no parent -> this is the first node
    else
      if (associated(prev % right, n)) then ! current node is the right child
        continue                           ! -> prev node is the parent
      else                                 ! current node is the left child
        ! then we go up until finding a node that is the right child or 
        ! the root node
        old => prev
        do 
          prev => parent(a, prev)
          if (associated(prev, a % nil)) then ! root node was reached
            exit    ! -> this was the last node, "prev" points to NIL
          else
            if (associated(prev % right, old)) then
              exit                         ! -> "prev" is parent of a right child
            else                           ! "prev" is parent of a left child
              old => prev                  ! -> keep looking
            endif
          endif
        enddo
      endif
    endif
  endif
  
  contains
  function rightmost(n)
    type(rbtn), pointer :: n, rightmost
    rightmost => n
    do 
      if (associated(rightmost % right, a % nil)) exit
      rightmost => rightmost % right 
    enddo
  end function rightmost
! -------------------------------------------------------------------------------
  end function predecessor
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_find(a,key,cfun,inext) result(ires)
! -------------------------------------------------------------------------------
! USE: Find a node with specified key, set current window to it.
!      If node is not found, window can be set to the next node
  type(rbtr) :: a
  integer(i4b), intent(in) :: key(:)
  integer(i4b), optional :: inext ! specifies what to do if key is not found    
                                  ! 0 - do not jump to next node
                                  ! 1 - jump to next node (default)
  integer(i4b) :: ires !  0 - key found
                       !  1 - not found but window pointed on next
                       ! -1 - empty tree
                       ! -2 - no next node to point
                       ! -3 - not found and pointing next disabled
  type(rbtn), pointer :: n
  procedure(cfun_ai) :: cfun
  integer(i4b) :: i, inext0
 !interface 
 !  function cfun(a,b) result (i)
      !use nrtype
 !    import i4b
 !    integer(i4b), intent(in) :: a(:), b(:)
 !    integer(i4b) :: i
 !  end function
 !end interface

  ! -
  if (a % nodes == 0) then
    ires = -1
    return
  endif

  inext0 = 1                         ! "inext" functionality is ON on default...
  if (present(inext)) inext0 = inext ! ... but can be changed

  n => a % root
  do
    i = cfun(key, n % data)
    select case (i)
    case(1)  ! key must be in the left sub-tree of N
      if (.not. associated(n % left, a % nil)) then ! left child of N exists
        n => n % left
        cycle
      endif
    case(-1) ! key must be in the right sub-tree of N
      if (.not. associated(n % right, a % nil)) then ! right child of N exists
        n => n % right 
        cycle
      endif
    case(0)  ! key was found
      continue
    case default
      print *, 'rbtr_find ERROR: ilegal value returned by cfun'
      stop
    end select
    exit
  enddo

  ! "i" is now "0" if key was found as N, 
  !            "1" if key would be precessor of N or
  !           "-1" if key would be successor of N
  select case (i)
  case(0)  
    ires = 0
  case(1)   
    if (inext0 == 1) then
      ires = 1
    else
      ires = -3
    endif
  case(-1)             ! move N to its sucessor node if "inext" is enabled
    if (inext0 == 1) then
      ires = 1
      n => successor(a,n)
      if (associated(n, a % nil)) ires = -2 ! key would be the last node
    else
      ires = -3
    endif
  end select

  ! set the window
  select case(ires)
  case(0,1)
    a % current => n
  case(-2,-3)
    nullify(a % current)
  case default
    print *, 'wrong wrong TODO test' ! TODO can be deleted for nicer code
    stop
  end select
! -------------------------------------------------------------------------------
  end function rbtr_find
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_read(a,id) result (dat)
! -------------------------------------------------------------------------------
! USE: Read data from node at the current position of list "a"

  type(rbtr) :: a
  integer(i4b), pointer :: dat(:)
  integer(i4b), optional :: id

  ! -

  if (.not. associated(a % current)) then
    print *, 'rbtr_read ERROR - current node not set'
    stop
  endif
  dat => a % current % data
  if (present(id)) id = a % current % id
! -------------------------------------------------------------------------------
  end function rbtr_read
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine rbtr_veri(a,ierr,iheight) 
! -------------------------------------------------------------------------------
! USE: Verify that the RB-tree is valid, Return its height

  type(rbtr) :: a
  integer(i4b), intent(out) :: ierr, iheight
  logical :: isvalid

  if (.not. associated(a % root)) then      ! the tree is empty
    ierr = 2 
    iheight = 0
    return
  endif

  call subtree_check(a, a % root, isvalid, iheight)
  ierr = -1
  if (isvalid) ierr = 0
! -------------------------------------------------------------------------------
  end subroutine rbtr_veri
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  recursive subroutine subtree_check(a, t, isvalid, i)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  type(rbtn), pointer :: t ! "t" is the root of subtree
  logical :: isvalid, isvalid_left, isvalid_right
  integer(i4b) :: i, i_left, i_right

  if (associated(t, a % nil)) then ! "t" is leaf
    isvalid = .true.
    i = 1
    return
  endif

  if (associated(t % parent, a % nil) .and. t % color /= black_color) then
    isvalid = .false. ! t is root and it is not black
    i = 0
    return
  endif

  if (t % color == red_color) then
    i = 0
    if (t % left  % color /= black_color .or. &
        t % right % color /= black_color) then
      isvalid = .false.
      i = -1
      return
    endif
  else
    i = 1
  endif

  call subtree_check(a, t % left, isvalid_left, i_left)
  call subtree_check(a, t % right, isvalid_right, i_right)

  isvalid = isvalid_left .and. isvalid_right .and. i_left==i_right
  i = i_left + i
  return
! -------------------------------------------------------------------------------
  end subroutine subtree_check
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_dele(a) result(ires)  
! -------------------------------------------------------------------------------
! TODO - only node, not data will be deallocated (freed from memory)
!
  type(rbtr) :: a
  integer(i4b) :: ires ! 0 = Ok
                       ! 1 = current node was not pointed (nothing done)
                       ! 2 = current node was last node
                       !-5/-6/-7 = deallocation error
  integer(i4b) :: ierr
  logical :: lfree
  type(rbtn), pointer :: n, ch

  ! -
  
  if (.not. associated(a % current)) then
print *, 'rbtr_dele: node not found '
    ires = 1
    return
  else
    ires = 0
  endif

  n => a % current ! N is node to be deleted


  ! CASE I: N has two non-leaf children
  ! * find the successor node, move content of that node to the current node
  !   to be deleted and then delete the successor node
  ! * this leads to the CASE II (one ore zero non-leaf children) 
  if (.not. (associated(n % left,  a % nil) .or. &
             associated(n % right, a % nil))) then
    ! Find the next largest node in N's right subtree (successor)
    ch => n % right
    do
      if (associated(ch % left, a % nil)) exit
      ch => ch % left
    enddo
    n % id = ch % id       ! copy content of successor's node to N
    deallocate(n % data, stat=ierr)
    if (ierr /= 0) then
      print *, 'rbtr_dele ERROR: deallocation error, stat = ',ierr
      ires = -5
    endif
    lfree = .true.         ! data array already cleared
    n % data => ch % data
    n => ch                ! succesor is new N to be deleted
  else
    lfree = .false.
  endif


  ! CASE II:  N has one non-leaf child or both childrens are leaves
  ! * If N is red, both its children must be leaves. The node can be
  !   removed without violating red-black tree properties.
  ! * If N is black and its child CH red, then N is replaced by CH, 
  !   CH is relabeled as black and we are done
  ! * If both N and CH are black, replacing N by CH will break red-black
  !   tree properties and it must be rebalanced (delete_caseX subroutines)

  if (associated(n % left, a % nil)) then ! left child is leaf
    ch => n % right
  elseif (associated(n % right, a % nil)) then ! right child is leaf
    ch => n % left
  else
print *, 'rbtr_dele: defensive check failed 1'
stop ! TODO this can be removed after debugging
  endif


  if (associated(ch, a % nil)) then ! N has zero non-leaf children
    if (n % color == black_color) then
      call delete_case1(a,n)
    else
      continue
    endif

    if (associated(n % parent, a % nil)) then       ! N is root
      nullify(a % root) 
    elseif (associated(n, n % parent % left)) then  ! N is left child
      n % parent % left => a % nil
    else                                            ! N is right child
      n % parent % right => a % nil
    endif

  else                              ! N has one non-leaf child CH
    ch % parent => n % parent
    if (associated(n % parent, a % nil)) then       ! N is root
      a % root => ch ! CH is new root
    elseif (associated(n, n % parent % left)) then  ! N is left child
      n % parent % left => ch
    else                                            ! N is right child
      n % parent % right => ch
    endif
    if (n % color == black_color) then              
      if (ch % color == red_color) then             ! N is black, CH is red
        ch % color = black_color
      else                                          ! both N and CH are black
        print *, 'rbtr_del: defensive check failed 2A'
        stop 
        ! Black node with one black child means that the number of black nodes in the 
        ! black child subtree is larger than in the other (leaf) subtree. This would
        ! imply that the tree was not balanced.
        !call delete_case1(a,ch)
      endif
    else                                            ! N is red (impossible here)
      print *, 'rbtr_dele: defensive check failed 2'
      stop
      ! it is not possible for the red node to have one black leaf and
      ! other subtree, this would imply property no. 5 violation
    endif
  endif
  
  ! remove the node from the memory and reduce the node number counter
  nullify (a % current)
  a % nodes = a % nodes - 1

  if (.not. lfree) then
    deallocate(n%data,stat = ierr)
    if (ierr /= 0) then
      print *, 'rbtr_dele ERROR: deallocation error, stat = ',ierr
      ires = -6
    endif
  endif
  deallocate(n,stat = ierr)
  if (ierr /= 0) then
    print *, 'rbtr_dele ERROR: deallocation error, stat = ',ierr
    ires = -7
  endif
  if (a % nodes == 0 .and. ires == 0) ires = 2 ! last node was deleted
! -------------------------------------------------------------------------------
  end function rbtr_dele
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine delete_case1(a,m)
! -------------------------------------------------------------------------------
! if M is the new root, nothing needs to be done, otherwise proceed to case 2

  type(rbtr) :: a
  type(rbtn), pointer :: m

  if (.not. associated(m % parent, a % nil)) then
    call delete_case2(a,m)
  endif
! -------------------------------------------------------------------------------
  end subroutine delete_case1
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine delete_case2(a,m)
! -------------------------------------------------------------------------------
! if S is red, then 
!   * make S black, 
!   * make P red and
!   * rotate left/right around P so S will become grandparent of M

  type(rbtr) :: a
  type(rbtn), pointer :: m, s

  s => sibling(a,m)
  if (s % color == red_color) then
    m % parent % color = red_color
    s % color = black_color
    if (associated(m, m % parent % left)) then ! M is left child
      call rotate_left(a,m % parent)
    else
      call rotate_right(a,m % parent)
    endif
  endif
  call delete_case34(a,m)
! -------------------------------------------------------------------------------
  end subroutine delete_case2 
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine delete_case34(a,m)
! -------------------------------------------------------------------------------
! if S, Sleft, Sright and P are black then
!   * repaint S red: this compensates the deleted black node in S' subtree but
!                    the whole P->M and P->S sub-trees are one black node less 
!                    than the remaining branches, therefore ...
!   * rebalance up-level: use delete_case1 on P
!
! if S, Sleft and Sright are black but P is red then
!   * exchange colors of S and P: and we are done
!
! otherwise proceed to delete_case5

  type(rbtr) :: a
  type(rbtn), pointer :: m, s

  s => sibling(a,m)
if (associated(s,a%nil)) then ! sibling is null node -what to do?'
print *, 'defensive check - sibling is leaf'
stop
endif

  if (m % parent % color == black_color .and. &
      s % color == black_color .and. &
      s % left % color == black_color .and. &
      s % right % color == black_color) then
    s % color = red_color
    call delete_case1(a,m % parent)

  elseif (m % parent % color == red_color .and. &
          s % color == black_color .and. &
          s % left % color == black_color .and. &
          s % right % color == black_color) then
    s % color = red_color
    m % parent % color = black_color

  else
    call delete_case5(a,m)
  endif
! -------------------------------------------------------------------------------
  end subroutine delete_case34
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine delete_case5(a,m)
! -------------------------------------------------------------------------------
! S is black, S left is red, S right is black and M is the left child 
! * rotate right at S so S left is new sibling of M
! * echange colors of S and its new parent (it was S left)
!
! Mirrored situation
! S is black, S right is red, S left is black and M is the right child
! * rotate left at S so S right is new sibling of M
! * echchange colors of S and its new parent (it was S right)
!
! At the end M should have black sibling with red children on the outside of
! the tree and this falls into case 6

  type(rbtr) :: a
  type(rbtn), pointer :: m, s

  s => sibling(a,m)
  if (associated(s,a%nil)) then ! sibling is null node - help me
    print *, 'RBTR ERROR sibling is leaf in delete_case5'
    stop
  endif

  if (s % color == black_color) then
  
    if (associated(m,m % parent % left) .and. & ! M is left child
        s % right % color == black_color) then

      if (s % left % color /= red_color) then
        print *, 'RBTR defensive check failed in delete_case5 - Sleft is not red'
        stop
      endif
    
      s % color = red_color
      s % left % color = black_color
      call rotate_right(a,s)

    elseif (associated(m,m % parent % right) .and. & ! M is right child
            s % left % color == black_color) then

      if (s % right % color /= red_color) then
        print *, 'RBTR defensive check failed in delete_case 5 - Sright not red'
        stop
      endif

      s % color = red_color
      s % right % color = black_color
      call rotate_left(a,s)
    endif

    call delete_case6(a,m)

  else
    print *, 'RBTR defensive check failed in delete_case5 - sibling is red'
    stop
  endif
! -------------------------------------------------------------------------------
  end subroutine delete_case5
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  subroutine delete_case6(a,m)
! -------------------------------------------------------------------------------
! If S is black, its right child is red and M is the left child then
! * rotate left at P so S becomes the parent of P and S's right child
! * exchange colors P and S and make S's right child black
!
! Mirrored situation
! If S is black, its left child is red and M is the right child then
! * rotate right at P so S becomes the parent of P and S's left child
! * exchange colors P and S and make S's left child black
!
! The properties of red-black tree are now restored

  type(rbtr) :: a
  type(rbtn), pointer :: m, s

  s => sibling(a,m)
  if (associated(s,a%nil)) then ! sibling is null node - help me
    print *, 'RBTR ERROR sibling is leaf in delete_case6'
    stop
  endif

  s % color = m % parent % color
  m % parent % color = black_color

  if (associated(m, m % parent % left)) then ! M is left child
    s % right % color = black_color
    call rotate_left(a,m % parent)
  else
    s % left % color = black_color
    call rotate_right(a,m % parent)
  endif
! -------------------------------------------------------------------------------
  end subroutine delete_case6
! -------------------------------------------------------------------------------



! -------------------------------------------------------------------------------
  function rbtr_test(a) result(ires)
! -------------------------------------------------------------------------------
  type(rbtr) :: a
  integer(i4b) :: ires

  type(rbtn), pointer :: n


ires = 0

  n => parent(a,a%current)
! print *, 'function exit ok'
  if (associated(n, a % nil)) then
    print '(3(2(i4,1x),a))', &
      a%current%id, a%current%color, &
      'ROOT NODE    L= ',a%current%left%id, a%current%left%color, &
      'R= ',a%current%right%id, a%current%right%color
    !print *, a%current%id, a%current%color,' parent not associated'
  else
    print '(4(2(i4,1x),a))', &
      a%current%id, a%current%color,' P=',n % id, n % color, &
      'L= ',a%current%left%id, a%current%left%color, &
      'R= ',a%current%right%id, a%current%right%color
  endif
  
  end function rbtr_test


  subroutine rbtr_test2(a)
  type(rbtr) :: a
! call rotate_right(a,a % current)
! call rotate_left(a,a % current)
  if (a % current % color == red_color) then
    a % current % color = black_color
  else
    a % current % color = red_color
  endif

  end subroutine rbtr_test2



! -------------------------------------------------------------------------------
  end module rbtr_module
! -------------------------------------------------------------------------------