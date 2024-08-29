program muliple3or5
	implicit none
    integer :: total
    integer :: i

    total = 0

    do i = 1, 999
        if ((MOD(i, 3) .eq. 0) .or. (MOD(i, 5) .eq. 0)) then
            total = total + i
        end if
    end do

    print *, total
        
end program
