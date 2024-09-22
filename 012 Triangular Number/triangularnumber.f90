!What is the value of the first triangle number 
!to have over five hundred divisors?

module math
	implicit none
	
contains
	function factor_list(numb) result(factors)
		implicit none
		integer(kind=8) :: numb
		integer(kind=8), allocatable :: factors(:), temp_factors(:)
		integer(kind=8) :: i, factor_count
		integer(kind=8) :: sqrtNumb

		! Initial guess for sqrtNumb
		sqrtNumb = nint(sqrt(real(numb, kind=8)))

		! Initialize an empty array to store factors
		factor_count = 0
		allocate(temp_factors(2 * sqrtNumb))

		! Loop from 1 to sqrt(numb) to find factors
		do i = 1, sqrtNumb
			if (MOD(numb, i) .eq. 0) then
				! i is a factor, add it
				factor_count = factor_count + 1
				temp_factors(factor_count) = i

				! Add the corresponding divisor numb / i if it's not the same as i
				if (i .ne. numb / i) then
				    factor_count = factor_count + 1
				    temp_factors(factor_count) = numb / i
				end if
			end if
		end do
		
		!allocate result array only with used elements
		allocate(factors(factor_count))
		factors = temp_factors(:factor_count)

		!deallocate temp array
		deallocate(temp_factors)

	end function factor_list
	
	
end module math

program triangularnumber
	use math
	implicit none
	integer(kind=8) :: numb, curr
	integer(kind=8), allocatable :: factors(:)
	integer :: factor_amount, min_factors
	
	min_factors = 500
	
	numb = 1
	factor_amount = 0
	
	do while(factor_amount .le. min_factors)
		curr = numb * (numb + 1) / 2
		factors = factor_list(curr)
		factor_amount = SIZE(factors)
		numb = numb + 1
	end do
	
	print *, curr

end program
