module day1708_mod
  use rbtr_module, only : rbtr, rbtd, rbtr_create, rbtr_inse, rbtr_find, &
  &   rbtr_free, rbtr_read, rbtr_dele, rbtr_gofs, rbtr_next
  use parse_mod, only : string_t, read_strings, split
  implicit none

  integer, parameter :: MAX_REGNAME_LEN=10
  type register_t
    character(len=MAX_REGNAME_LEN) :: regname
    integer :: val = 0
  end type

contains

  subroutine day1708(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), toks(:)
    type(rbtr) :: root
    type(register_t) :: regtmp
    integer :: i, ierr, ans1, ans2

    lines = read_strings(file)
    root = rbtr_create(transfer(regtmp, rbtd))
    ans2 = -huge(ans2)
    do i=1, size(lines)
      call split(lines(i)%str, ' ', toks)
      if (size(toks)/=7) error stop 'not seven tokens'
      call execute(toks, root, ans2)
    end do
    call regprint(root, ans1)
    print '("Answer 8/1 ",i0,l2)', ans1, ans1==4567
    print '("Answer 8/2 ",i0,l2)', ans2, ans2==5636
    ierr = rbtr_free(root)

  end subroutine day1708



  subroutine regprint(root, maxv)
    type(rbtr), intent(in) :: root
    integer, intent(out) :: maxv

    integer :: ierr
    type(register_t) :: reg

    ierr = rbtr_gofs(root)
    maxv = -huge(maxv)
    do
      if (ierr /= 0) exit
      reg = transfer(rbtr_read(root), reg)
     !print '("{",a,"} v = ",i0)', trim(reg%regname), reg%val
      if (reg%val > maxv) maxv = reg%val
      ierr = rbtr_next(root)
    end do

  end subroutine regprint



  subroutine execute(toks, root, maxv)
    type(string_t), intent(in) :: toks(:)
    type(rbtr), intent(inout) :: root
    integer, intent(inout) :: maxv

    logical :: istrue
    integer :: regval, opval

    istrue = evaluate(toks, root)
    if (.not. istrue) return
    regval = regread(toks(1)%str, root)
    read(toks(3)%str,*) opval
    select case(toks(2)%str)
    case('inc')
      regval = regval + opval
    case('dec')
      regval = regval - opval
    case default
      error stop 'execute - uknonwn action'
    end select
    if (regval > maxv) maxv = regval
    call regwrite(toks(1)%str, root, regval)
!print *, toks(1)%str,' has now value ',regval
  end subroutine execute



  logical function evaluate(toks, root) result(istrue)
    type(string_t), intent(in) :: toks(:)
    type(rbtr), intent(inout) :: root

    integer :: lval, rval

    if (toks(4)%str /= 'if' .or. size(toks) /= 7) error stop 'evaluate - wrong format'
    lval = regread(toks(5)%str, root)
    read(toks(7)%str, *) rval
    select case (toks(6)%str)
    case('>')
      istrue = lval > rval
    case('<')
      istrue = lval < rval
    case('>=')
      istrue = lval >= rval
    case('<=')
      istrue = lval <= rval
    case('==')
      istrue = lval == rval
    case('!=')
      istrue = lval /= rval
    case default
      error stop 'evaluate - unknown operator'
    end select

  end function evaluate



  integer function cmpfun(a, b) result(res)
    integer, intent(in) :: a(:), b(:)
    type(register_t) :: rega, regb

    rega = transfer(a, rega)
    regb = transfer(b, regb)
    if (rega%regname < regb%regname) then
      res = 1
    else if (rega%regname > regb%regname) then
      res = -1
    else
      res = 0
    end if
  end function cmpfun



  integer function regread(regname, root) result(val)
    character(len=*), intent(in) :: regname
    type(rbtr), intent(inout) :: root

    type(register_t) :: regtmp
    integer :: ierr

    regtmp%regname = regname
    ierr = rbtr_find(root, transfer(regtmp,rbtd), cmpfun)
    if (ierr==0) then
      regtmp = transfer(rbtr_read(root), regtmp)
      val = regtmp%val
    else
      val = 0
    end if

  end function regread



  subroutine regwrite(regname, root, val)
    character(len=*), intent(in) :: regname
    type(rbtr), intent(inout) :: root
    integer, intent(in) :: val

    type(register_t) :: regtmp
    integer :: ierr

    regtmp%regname = regname
    ierr = rbtr_find(root, transfer(regtmp,rbtd), cmpfun)
    if (ierr==0) then
      ierr = rbtr_dele(root)
    end if
    regtmp%val = val
    ierr = rbtr_inse(root, transfer(regtmp,rbtd), cmpfun)

  end subroutine regwrite

end module day1708_mod