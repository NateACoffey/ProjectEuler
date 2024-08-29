!find the largest prime factor of 600851475143
program largestprime
	implicit none
	integer(kind=8) :: numb
	integer(kind=8) :: largest

	numb = 600851475143

	largest = factor(numb)

    
	print *, largest


contains

	function factor(numb) result(largest)
		implicit none
		integer(kind=8) :: numb
		integer(kind=8) :: largest
		integer :: i
		integer(kind=8) :: sqrtNumb

		!cast numb to real64 for the sqrt then cast result to int
		sqrtNumb = nint(sqrt(real(numb, kind=8)))

		!reduce number until it's no longer divisible by 2
		do while(MOD(numb, 2) .eq. 0)
			largest = 2
			numb = numb / 2
		end do

		!reduce number by every odd number and store the largest i
		!highest can only ever be sqrt
		i = 3
		do while((i .le. sqrtNumb) .and. (numb .gt. 1))
			do while(MOD(numb, i) .eq. 0)
				largest = i
				numb = numb / i
			end do
			!recalculate sqrt to save loop time as numb gets reduced
			sqrtNumb = nint(sqrt(real(numb, kind=8)))
			
			i = i + 2
		end do
		
		!verify resulting numb is not prime instead reduced to 1
		if (numb .gt. 1) then
			largest = numb
		end if
		

	end function factor   

end program
