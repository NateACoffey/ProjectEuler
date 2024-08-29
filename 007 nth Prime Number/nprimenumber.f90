!What is the 10001st prime number
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
	
	function n_th_prime(n) result(numb)
		implicit none
		integer, intent(in) :: n
		integer(kind=8) :: i
		integer(kind=8) :: primeCount, currPrime
		integer(kind=8) :: numb
		
		primeCount = 0
		
		!go through every number and save amount of primes
		i = 2
		do while(primeCount .lt. n)
			if(isPrime(i))then
				primeCount = primeCount + 1
				currPrime = i
			end if
			i = i + 1
		end do	
		
		numb = currPrime
		
	end function n_th_prime

end module math

program n_prime_number
	use math
	implicit none
	integer(kind=8) :: prime
	integer :: n
	
	n = 10001
	
	prime = n_th_prime(n)
	
	
	print *, prime

end program
