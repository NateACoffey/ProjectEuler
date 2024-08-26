!Find the smallest positive number evenly divisible by 1-20
program smallestmultiple
	implicit none
	integer :: small, large
	integer(kind=8) :: i
	integer(kind=8) :: lastLCM
	
	lastLCM = 1
	
	
	small = 1
	large = 20
	
	i = small
	do while(i .le. large)
		lastLCM = LCM(lastLCM, i)
		
		i = i + 1
	end do
	
	print *, lastLCM
	
contains
	function LCM(a, b) result(product)
		implicit none
		integer(kind=8), intent(in) :: a, b
		integer(kind=8) :: gcd_ab
		integer(kind=8) :: product

		gcd_ab = GCD(a, b)
		!Simple LCM formula
		product = (a /gcd_ab) * b
		
	end function LCM
	
	function GCD(a, b) result(product)
		implicit none
		integer(kind=8), intent(in) :: a, b
		integer(kind=8) :: quotient, divisor, remainder
		integer(kind=8) :: product
		
		quotient = a
		divisor = b
		
		!Euclidean algorithm
		remainder = MOD(quotient, divisor)
		do while(remainder .ne. 0)
			quotient = divisor
			divisor = remainder
			remainder = MOD(quotient, divisor)
		end do
		
		product = divisor
		
	end function GCD
	

end program
