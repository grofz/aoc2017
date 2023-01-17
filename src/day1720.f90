module day1720_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type particle_t
    integer :: a(3), v(3), x(3)
    integer :: id
  contains
    procedure :: update => particle_update
    procedure :: dst => particle_dst
  end type
  interface particle_t
    module procedure particle_new
  end interface

contains

  subroutine day1720(file)
    character(len=*), intent(in) :: file

    type(particle_t), allocatable :: p(:)
    type(string_t), allocatable :: lines(:)
    integer :: i, j, np
    integer :: ans1, ans2

    lines = read_strings(file)
    np = size(lines)
    allocate(p(np))
    do i=1,np
      p(i) = particle_t(lines(i)%str, i)
    end do
    !print '("Particles read: ",i0)', np

    do j=1, 400 ! ending condition not implemented TODO
      do i=1, np
        call p(i)%update()
      end do
      call remove_collided(p)
    end do
    ans1 = closest_part(p)
    ans2 = count(p(:)%id/=0)
    print '("Answer 20/1 ",i0,l2)', ans1-1, ans1-1==125
    print '("Answer 20/2 ",i0,l2)', ans2, ans2==461

  end subroutine day1720


  subroutine remove_collided(p)
    type(particle_t), intent(inout), allocatable :: p(:)

    integer :: i, j

    do i=1,size(p)-1
      if (p(i)%id==0) cycle
      do j=i+1,size(p)
        if (p(j)%id==0) cycle
        if (all(p(i)%x==p(j)%x)) then
          p(i)%id = 0
          p(j)%id = 0
        end if
      end do
    end do

   !print *, 'count =', count(p(:)%id/=0)
  end subroutine remove_collided


  integer function closest_part(ps) result(id)
    type(particle_t), intent(in) :: ps(:)

    integer :: dstmin, i

    id = 0
    dstmin = huge(dstmin)
    do i=1, size(ps)
      if (ps(i)%dst() < dstmin) then
        dstmin = ps(i)%dst()
        id = i
      end if
    end do
  end function closest_part


  integer function particle_dst(this) result(dst)
    class(particle_t), intent(in) :: this

    integer :: xabs(3)

    where(this%x > 0)
      xabs = this%x
    else where
      xabs = -this%x
    end where
    dst = sum(xabs)
  end function particle_dst


  subroutine particle_update(this)
    class(particle_t), intent(inout) :: this
    this%v = this%v + this%a
    this%x = this%x + this%v
  end subroutine particle_update


  type(particle_t) function particle_new(str, id) result(new)
    character(len=*), intent(in) :: str
    integer, intent(in) :: id

    new%x = parse_vector(str,'p')
    new%v = parse_vector(str,'v')
    new%a = parse_vector(str,'a')
    new%id = id
  end function particle_new


  function parse_vector(str,template) result(vec)
    character(len=*), intent(in) :: str, template
    integer :: vec(3)

    integer :: i, j
    i = index(str(1:),template//'=<')
    if (i==0) error stop 'new - p not found'
    i = i + len(template)+2 
    j = scan(str(i:),'>')
    if (j==0) error stop 'new - p > not found'
    j = i + j - 1
    read(str(i:j-1),*) vec
  end function parse_vector


end module day1720_mod