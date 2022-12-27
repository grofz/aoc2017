module day1706_mod
  use rbtr_module, only : rbtr, rbtr_create, rbtr_inse, rbtr_find, rbtr_free
  implicit none

  integer, parameter :: NBANKS = 16
  !integer, parameter :: NBANKS = 4 ! test case
  integer, parameter :: ISFOUND = 0, MAXI = 1000000

contains

  subroutine day1706(file)
    character(len=*), intent(in) :: file

    integer :: mem(NBANKS), memsaved(NBANKS), ierr, i
    type(rbtr) :: root

    ! Read initial configuration
    mem = read_input(file)
    !mem = [0, 2, 7, 0] ! test case

    ! Create map and add the first node
    root = rbtr_create(mem)
    ierr = rbtr_inse(root, mem, cmp)

    ! Defragmentation steps, exit when known state encountered
    do i=1, MAXI
      call redistribute(mem)
      ierr = rbtr_find(root, mem, cmp)
      if (ierr==ISFOUND) exit
      ierr = rbtr_inse(root, mem, cmp)
    end do
    if (i==MAXI+1) error stop 'repetition not found'
    print '("Answer 6/1 ",i0,l2)', i, i==5042

    ! Part 2
    memsaved = mem
    do i=1, MAXI
      call redistribute(mem)
      if (cmp(mem, memsaved) == 0) exit
    end do
    if (i==MAXI+1) error stop 'repetition not found'
    print '("Answer 6/2 ",i0,l2)', i, i==1086

    ! Free map
    ierr = rbtr_free(root)
  end subroutine day1706



  subroutine redistribute(aa)
    integer, intent(inout) :: aa(:)

    integer :: icur, suma
    integer :: nchunk, nrem

    suma = sum(aa)
    icur = maxloc(aa, dim=1)

    ! Every bank receives "nchunk" blocks from current bamk,
    ! "nrem" banks then receive one additional block.
    nchunk = aa(icur)/NBANKS
    nrem = mod(aa(icur), NBANKS)

    ! Remove all blocks from the current bank
    aa(icur) = 0

    ! Redistribute the chunks
    aa = aa + nchunk
    aa(icur+1:min(NBANKS,icur+nrem)) = aa(icur+1:min(NBANKS,icur+nrem)) + 1
    aa(1:nrem-NBANKS+icur) = aa(1:nrem-NBANKS+icur) + 1

    ! Defensive test
    if (suma /= sum(aa)) error stop 'total block number mismatch'
  end subroutine redistribute



  function cmp(a, b) result(res)
    integer :: res
    integer, intent(in) :: a(:), b(:)

    integer :: i

    res = 0
    do i=1, NBANKS
      if (a(i) < b(i)) then
        res = 1
        exit
      else if (a(i) > b(i)) then
        res = -1
        exit
      end if
    end do

  end function cmp



  function read_input(file) result(aa)
    use parse_mod, only : string_t, read_strings, split
    character(len=*), intent(in) :: file
    integer :: aa(NBANKS)

    type(string_t), allocatable :: lines(:), words(:)
    integer :: i

    lines = read_strings(file)
    if (size(lines) /= 1) error stop 'only one line expected'
    call split(lines(1)%str, achar(9), words)
    if (size(words) /= NBANKS) error stop 'read_input - expected NBANKS numbers'
    aa = 0
    do i=1, size(words)
      read(words(i)%str,*) aa(i)
    end do
  end function read_input

end module day1706_mod