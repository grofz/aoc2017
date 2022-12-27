module day1704_mod
  use parse_mod, only : split, read_strings, string_t
  implicit none

contains

  subroutine day1704(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1,size(lines)
      associate(is=>contains_duplicate(lines(i)%str))
        if (.not. is(1)) ans1 = ans1 + 1
        if (.not. is(2)) ans2 = ans2 + 1
      end associate
    end do
    print '("Answer 4/1 ",i0,l2)', ans1, ans1==383
    print '("Answer 4/2 ",i0,l2)', ans2, ans2==265

  end subroutine day1704



  function contains_duplicate(line) result(is)
    character(len=*), intent(in) :: line
    logical :: is(2)
!
! Return "true" if 
! (1) the line cotains duplicate words
! (2) the line contains words that are anagrams
!
    type(string_t), allocatable :: words(:)
    integer :: i, j, n

    call split(line, ' ', words)
    n = size(words)
    is = .false.
    OUT: do i=1, n-1
      do j=i+1, n
        if (words(i)%str == words(j)%str) then
          is = .true.
          exit OUT
        else if (are_anagrams(words(i)%str, words(j)%str)) then
          is(2) = .true.
          if (all(is)) exit OUT
        end if
      end do
    end do OUT

  end function contains_duplicate



  logical function are_anagrams(w1, w2) 
    character(len=*), intent(in) :: w1, w2
!
! Are two words anagrams?
!
    integer :: n
    character(len=max(len(w1), len(w2))) :: u1, u2

    are_anagrams = .false.
    n = len(w1)
    if (len(w2) /= n) return

    ! Sort characters according their ASCII value
    ! and compare them
    u1 = w1
    call sort(u1)
    u2 = w2
    call sort(u2)
    if (u1 == u2) are_anagrams = .true.

  contains
    subroutine sort(w)
      character(len=*), intent(inout) :: w
      integer :: i, j
      character(len=1) :: ch

!write(*,'(a)',advance='no') w
      ! Insertion sort
      do i = 2, len(w)
        ch = w(i:i)
        do j = i, 2, -1 
          if (iachar(w(j-1:j-1)) <= iachar(ch)) exit
          w(j:j) = w(j-1:j-1)
        end do
        w(j:j) = ch
      end do
!write(*,'(a)') ' --> '//w
    end subroutine sort
  end function are_anagrams

end module day1704_mod