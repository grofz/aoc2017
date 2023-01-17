module day1723_mod
  use day1718_mod, only : comp_t

contains

  subroutine day1723(file)
    character(len=*), intent(in) :: file

    type(comp_t) :: comp
    integer :: istat, ans1, breg, creg, ans2, i

    ! Count number of mul instructions
    comp = comp_t(file)
    ans1 = 0
    do 
      call comp%exec_one(istat)
      if (comp%ip < 1 .or. comp%ip > size(comp%prog)) exit
      if (comp%prog(comp%ip)%ch=='mul') ans1 = ans1 + 1
    end do
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==5929

    ! Let the code set registers B and C and then exit
    comp = comp_t(file)
    comp%regs(1)%v = 1 ! set a 1
    do 
      !print *, comp%ip, comp%prog(comp%ip)%ch, comp%prog(comp%ip)%op(1)%id, comp%prog(comp%ip)%op(2)%val(comp%regs)
      call comp%exec_one(istat)
      !print '(*(i0,1x))', comp%regs(1:8)%v
      if (comp%ip==10) exit
      if (comp%ip < 1 .or. comp%ip > size(comp%prog)) exit
    end do
    breg = comp%regs(2)%v
    creg = comp%regs(3)%v

    ! The code counts the number of non-primes between
    ! breg and creg with step 17
    ans2 = 0
    do i=breg, creg, 17
      if (.not. isprime(i)) ans2 = ans2 + 1
    end do
    print '("Answer 23/2 ",i0,l2)', ans2, ans2==907

  end subroutine day1723


  logical function isprime(n)
    integer, intent(in) :: n
!
! https://en.wikipedia.org/wiki/Primality_test
!
    integer :: limit, i

    if (n <= 3) then
      isprime = n > 1
    else if (mod(n,2)==0 .or. mod(n,3)==0) then
      isprime = .false.
    else
      limit = int(sqrt(real(n)))
      isprime = .true.
      do i = 5, limit+1, 6
        if (mod(n,i)/=0 .and. mod(n,i+2)/=0) cycle
        isprime = .false.
        exit
      end do
    end if
  end function isprime
end module day1723_mod