module day1714_mod
  use day1710_mod, only : knot_hash
  implicit none
  private
  public day1714

  integer, parameter :: MAXLEN=8, NBITS=128

  type disk_t
    logical :: bb(0:NBITS-1, 0:NBITS-1)
    integer :: ll(0:NBITS-1, 0:NBITS-1)
    character(len=MAXLEN) :: base
  end type

contains

  subroutine day1714()
    type(disk_t) :: main
    integer :: ans1, ans2

    main%base = 'wenycdww' ! real case
    !main%base = 'flqrgnkx' ! test case

    call disk_generate(main)
    ans1 = count(main%bb)
    print '("Answer 14/1 ",i0,l2)', ans1, ans1==8108 .or. ans1==8226

    call label(main%ll, main%bb, ans2)
    print '("Answer 14/2 ",i0,l2)', ans2, ans2==1242 .or. ans2==1128

  end subroutine day1714


  subroutine disk_generate(th)
    class(disk_t), intent(inout) :: th

    integer :: irow, ib
    character(len=3) :: cnum
    character(len=32) :: hash
    
    do irow=0, NBITS-1
      write(cnum,'(i3)') irow
      hash = knot_hash(trim(th%base)//'-'//trim(adjustl(cnum)))
      do ib = 1, 32
        th%bb(irow, (ib-1)*4:4*ib-1) = hexdig_to_bits(hash(ib:ib))
      end do
    end do
  end subroutine disk_generate


  function hexdig_to_bits(hexdig) result(bits)
    character(len=1) :: hexdig
    logical :: bits(4)

    integer :: hex, i, j

    if (hexdig>='a' .and. hexdig<='f') then
      hex = 10 + iachar(hexdig) - iachar('a')
    else if (hexdig>='0' .and. hexdig<='9') then
      hex = iachar(hexdig) - iachar('0')
    else 
      error stop 'hexdig_to_bits - not a hex digit'
    end if
    if (hex>15 .or. hex<0) error stop 'something wrong'

    bits = .false.
    do i=1,4
      j = hex / 2**(4-i)
      hex = hex - j*2**(4-i)
      if (j==1) bits(i)=.true.
    end do
    if (hex/=0) error stop 'something also wrong'
  end function hexdig_to_bits


  subroutine label(aa, mask, ngroups)
    integer, intent(out) :: aa(:,:)
    logical, intent(in) :: mask(:,:)
    integer, intent(out) :: ngroups

    integer :: i, j

    ngroups = 0
    aa = 0
    do i =1, size(mask,1)
    do j =1, size(mask,2)
      if (.not. mask(i,j)) cycle
      if (aa(i,j) > 0) cycle
      ngroups = ngroups + 1
      call flood_fill(aa, mask, i, j, ngroups)
    end do
    end do
  end subroutine label


  recursive subroutine flood_fill(aa, mask, x, y, ilab)
    integer, intent(inout) :: aa(:,:)
    logical, intent(in) :: mask(:,:)
    integer, intent(in) :: x, y, ilab

    integer :: i, j

    if (x<1 .or. y<1 .or. x>size(mask,1) .or. y>size(mask,2)) return
    if (.not. mask(x,y)) return
    if (aa(x,y) == ilab) return
    if (aa(x,y) > 0) error stop 'already labeled as another group'
    aa(x,y) = ilab
    do i=x-1,x+1
    do j=y-1,y+1
      ! consider only first closest neighbors
      if (abs(x-i)+abs(y-j) /= 1) cycle
      call flood_fill(aa, mask, i, j, ilab)
    end do
    end do
  end subroutine flood_fill

end module day1714_mod