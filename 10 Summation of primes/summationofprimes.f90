
module math
	implicit none
contains
	function isPrime(n) result(prime)
		implicit none
		integer(kind = 8), intent(in) :: n
		integer(kind=8) :: sqrtNumb, numb
		integer :: i
		logical :: prime
		
		numb = n
		!adjust factor formula but if we find a factor, we return false
		if(n .le. 1)then
			prime = .false.
			return
		else if(n .le. 3) then
			prime = .true.
			return
		end if
		
		prime = .true.
		
		!cast numb to real64 for the sqrt then cast result to int
		sqrtNumb = nint(sqrt(real(n, kind=8)))

		!divisible by 2
		if(MOD(numb, 2) .eq. 0)then
			prime = .false.
			return
		end if

		!divisible by an odd number
		do i = 3, sqrtNumb, 2
			if(MOD(numb, i) .eq. 0) then
				prime = .false.
				return
			end if
		end do
	
	end function isPrime
	
	function sumOfPrimes(n) result(numb)
		implicit none
		integer, intent(in) :: n
		integer(kind=8) :: i
		integer(kind=8) :: primeSum
		integer(kind=8) :: numb
		
		primeSum = 0
		
		
		do i = 2, n-1
			if(isPrime(i))then
				primeSum = primeSum + i
			end if
		end do	
		
		numb = primeSum
		
	end function sumOfPrimes

end module math


program summationofprime
	use math
	implicit none
	integer :: highest
	integer(kind=8) :: sum

	highest = 2e6

	sum = sumOfPrimes(highest)

	print *, sum

end program
