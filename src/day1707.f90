module day1707_mod
  use parse_mod, only : split, string_t, read_strings
  implicit none

  integer, parameter :: MAXLEN=10

  type :: prog_t
    character(len=MAXLEN) :: name
    integer :: w
    integer :: pid = 0
    integer, allocatable :: cids(:)
    logical :: isbalanced
  end type

contains

  subroutine day1707(file)
    character(len=*), intent(in) :: file

    integer :: i, np, iroot, wid, ans2
    type(string_t), allocatable :: lines(:)
    type(prog_t), allocatable :: pp(:)
    character(len=MAXLEN) :: ans1

    lines = read_strings(file)
    np = size(lines)
    allocate(pp(np))

    ! Read program name and weight
    do i=1,np
      pp(i) = prog_new(lines(i)%str)
    end do

    ! Connect parent and child programs
    do i=1,np
      call prog_addparent(pp, i, lines(i)%str)
    end do

    ! Part 1
    iroot = findloc(pp%pid, 0, dim=1)
    if (iroot==0) error stop 'root not found'
    if (iroot /= findloc(pp%pid, 0, dim=1, back=.true.)) error stop 'more than one root'
    ans1 = pp(iroot)%name
    print '("Answer 7/1 ",a,l2)', trim(ans1), ans1=='cyrupz'

    ! Mark programs that are balanced
    do i=1,np
      pp(i)%isbalanced = prog_isbalanced(i, pp)
    end do
    call correct_weight(iroot, pp, 0, wid, ans2)
!print '(2x,a,": modify weight ",i0," to ",i0)', &
!   &  trim(pp(wid)%name), pp(wid)%w, ans2
    print '("Answer 7/2 ",i0,l2)', ans2, ans2==193
  end subroutine day1707



  recursive subroutine correct_weight(id, pp, wreq, wid, wcor)
    integer, intent(in) :: id ! current id
    type(prog_t), intent(in) :: pp(:)
    integer, intent(in) :: wreq   ! required weight
    integer, intent(out) :: wid   ! id of wrong node
    integer, intent(out) :: wcor  ! correct weight

    integer :: i, ifound, wreq0, icorrect

    if (pp(id)%isbalanced) error stop 'nothing to do in a balanced branch'
    if (size(pp(id)%cids)==1) error stop 'single child not supported by the current algoritm'

    associate(cids=>pp(id)%cids)
      ! Find which subtower is corrupted
      icorrect = 0
      ifound = 0
      do i=1, size(cids)
        if (pp(cids(i))%isbalanced) then
          icorrect = i
          cycle
        end if
        if (ifound /= 0) error stop 'more than one branch are corrupted'
        ifound = i
      end do

      if (ifound==0) then
        ! weight of one of current program childs is wrong
        wreq0 = (wreq-pp(id)%w) / size(cids)
        do i=1,size(cids)
          if (prog_weight(cids(i), pp) == wreq0) cycle
          ifound = i
          exit
        end do
        if (ifound==0) error stop 'current program is wrong'
        wid = cids(ifound)
        wcor = pp(wid)%w + (wreq0 - prog_weight(wid,pp))
      else
        ! use results from the recursive instance
        wreq0 = prog_weight(cids(icorrect), pp)
        call correct_weight(cids(ifound), pp, wreq0, wid, wcor)
      end if
    end associate

  end subroutine correct_weight



  logical function prog_isbalanced(id,pp) result(isbalanced)
    integer, intent(in) :: id
    type(prog_t), intent(in) :: pp(:)

    integer :: i, wsave

    isbalanced = .true.
    if (size(pp(id)%cids)==0) return
    wsave = prog_weight(pp(id)%cids(1), pp)
    do i=2, size(pp(id)%cids)
      if (wsave == prog_weight(pp(id)%cids(i), pp)) cycle
      isbalanced = .false.
      exit
    end do

  end function prog_isbalanced



  recursive function prog_weight(id, pp) result(w)
    integer, intent(in) :: id
    type(prog_t), intent(in) :: pp(:)
    integer :: w

    integer :: i

    w = pp(id)%w
    do i=1, size(pp(id)%cids)
      w = w + prog_weight(pp(id)%cids(i), pp)
    end do

  end function prog_weight



  subroutine prog_addparent(pp, pid, str)
    type(prog_t), intent(inout) :: pp(:)
    integer, intent(in) :: pid
    character(len=*), intent(in) :: str

    integer :: i, j, id

    i = index(str,'-> ')
    allocate(pp(pid)%cids(0))
    if (i==0) return ! no children

    i = i + 3
    do
      j = scan(str(i:),',')
      if (j==0) then
        j = len_trim(str) + 1
      else
        j = i + j - 1
      end if

      id = prog_find(pp, trim(adjustl(str(i:j-1))))
      if (id==0) error stop 'child index not found'
      pp(id)%pid = pid
      pp(pid)%cids = [pp(pid)%cids, id]

      if (j > len_trim(str)) exit
      i = j + 1
    end do
  end subroutine prog_addparent



  integer function prog_find(pp, name) result(ind)
    type(prog_t), intent(in) :: pp(:)
    character(len=*), intent(in) :: name

    integer :: i

    ind = 0
    do i=1,size(pp)
      if (name == trim(pp(i)%name)) exit
    end do
    if (i /= size(pp)+1) ind = i

  end function prog_find



  type(prog_t) function prog_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i, j, k

    i = scan(str, ' ')
    j = scan(str, '(')
    k = scan(str, ')')
    if (i==0 .or. j==0 .or. k==0) error stop 'new - invalid format'
    new%name = str(:i-1)
    read(str(j+1:k-1),*) new%w

  end function prog_new

end module day1707_mod