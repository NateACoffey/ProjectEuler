program evenfibonacci
    implicit none
    integer :: last
    integer :: secondlast
    integer :: current
    integer :: total

    last = 1
    secondlast = 1
    current = 1
    total = 0

    do while(current < 4000000)
        if(MOD(current, 2) .eq. 0) then
            total = total + current
        end if

        secondlast = last
        last = current

        current = last + secondlast
    end do
    
    
    print *, total

end program
