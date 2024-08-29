!There exists exactly one Pythagorean triplet for which a+b+c=1000.
!Find the product abc

program pythagoreantriple
	implicit none
	integer :: i, j, k
	integer :: last
	
	last = 1000
	
	!i < j < k
	do i = 1, last / 3 !max possible: 332 + 333 + 334
		do j = i + 1, last / 2 !max possible: 1 + 499 + 500
			k = last - i - j
			if(i**2 + j**2 .eq. k**2)then
				print *, i*j*k
				return
			end if
			
		end do
	end do
	


end program
