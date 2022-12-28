module day1709_mod
  use parse_mod, only : read_strings, string_t
  implicit none

  type group_t
    integer :: s = 0 ! score
    type(group_ptr), allocatable :: cs(:)
  end type

  type group_ptr
    type(group_t), pointer :: ptr
  end type group_ptr

contains

  subroutine day1709(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: line(:)
    type(string_t) :: cleaned
    type(group_t), pointer :: root
    integer :: iend, ans1, ans2

    line = read_strings(file)
    cleaned = clean_garbage(line(1)%str, ans2)
    call group_parse(cleaned%str, 1, root, iend)
    if (iend-1 /= len(cleaned%str)) error stop 'unused characters'
    ans1 = group_score(root)
    print '("Answer 9/1 ",i0,l2)', ans1, ans1==10616
    print '("Answer 9/2 ",i0,l2)', ans2, ans2==5101
    call group_free(root)

  end subroutine day1709



  function group_new(score) result(new)
    integer, intent(in) :: score
    type(group_t), pointer :: new
    allocate(new)
    new%s = score
    allocate(new%cs(0))
  end function group_new



  recursive subroutine group_free(this)
    type(group_t), intent(inout), pointer :: this
    integer :: i
    do i=1,size(this%cs)
      call group_free(this%cs(i)%ptr)
    end do
    deallocate(this)
  end subroutine group_free



  recursive function group_score(this) result(score)
    class(group_t), intent(in) :: this
    integer :: score

    integer :: i
    score = this%s
    do i=1, size(this%cs)
      score = score + group_score(this%cs(i)%ptr)
    end do
  end function group_score



  recursive subroutine group_parse(str, lev, new, iend)
    character(len=*), intent(in) :: str
    integer, intent(in) :: lev
    type(group_t), pointer, intent(out) :: new
    integer, intent(out) :: iend ! first unused character in "str"

    integer :: i, j, ncomp 
    type(group_ptr) :: gptr

    if (len(str)==0) error stop 'parse - empty input'

    i = 1
    ncomp = 0
    ! The first character must be { or <
    if (str(i:i)=='{') then
      ! Add group...
      new => group_new(lev)
      ! ...and components of the new group
      do
        i = i + 1
        if (i > len(str)) error stop 'parse - reaching end prematurely'
        if (str(i:i)=='}') exit
        if (str(i:i)==',') cycle
        ncomp = ncomp + 1
        call group_parse(str(i:), lev+1, gptr%ptr, j)
        j = i + j - 1
        new%cs = [new%cs, gptr]
        i = j - 1
      end do
      iend = i + 1

    else if (str(i:i)=='<') then
      ! Group is a garbage item
      if (str(i+1:i+1)/='>') error stop 'parse - garbage not cleaned'
      new => group_new(0)
      iend = i+2
    else
      error stop 'parse - unexpected first character'
    end if

  end subroutine group_parse



  type(string_t) function clean_garbage(str, nrem) result(cleaned)
    character(len=*), intent(in) :: str
    integer, intent(out) :: nrem

    character(len=len(str)) :: ss, tt
    integer :: i, j
    logical :: ignore_next, inside_garbage

    ! Pre-wash: remove characters behind "!"
    ss = ''
    j = 0
    ignore_next = .false.
    do i=1, len(str)
      if (ignore_next) then
        ignore_next = .false.
      else if (str(i:i) == '!') then
        ignore_next = .true.
      else
        j = j + 1
        ss(j:j) = str(i:i)
      end if
    end do

    ! Wash: remove garbage, leave "<>"
    nrem = 0
    tt = ''
    j = 0
    inside_garbage = .false.
    do i=1, len_trim(ss)
      if (inside_garbage) then
        ! look for ">" that marks the end of garbage
        if (ss(i:i) == '>') then
          inside_garbage = .false.
          j = j + 1
          tt(j:j) = ss(i:i)
        else
          ! remove this character
          nrem = nrem + 1
        end if
      else
        ! look for "<" that marks the start of garbage
        if (ss(i:i) == '<') then
          inside_garbage = .true.
          j = j + 1
          tt(j:j) = ss(i:i)
        else
          j = j + 1
          tt(j:j) = ss(i:i)
        end if
      end if
    end do
    if (inside_garbage) error stop 'clean_garbage - stuck in garbage'

    cleaned = string_t(tt(1:j))
  end function clean_garbage

end module day1709_mod