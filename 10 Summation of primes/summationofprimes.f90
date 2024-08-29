!Find the sum of all the primes below two million
module math
	implicit none
contains
	
	function sieve_of_eratosthenes(n) result(numb)
		implicit none
		integer(kind=8) :: n
		logical, dimension(2:n) :: isPrime
		integer :: i, j
		integer(kind=8) :: numb
		
		isPrime = .true.
		
		!start from 2, 
		!mark every multiple of i after i^2 as not prime
		do i = 2, int(sqrt(real(n, kind=8)))
			if(isPrime(i))then
				j = i*i
				do while(j <= n)
					isPrime(j) = .false.
					j = j + i
				end do
			end if
		end do
		
		numb = 0
		do i = 2, n
			if(isPrime(i))then
				numb = numb + i
			end if
		end do
		
		
		
	end function sieve_of_eratosthenes

end module math


program summationofprime
	use math
	implicit none
	integer(kind=8) :: highest
	integer(kind=8) :: sum

	highest = 2e6

	sum = sieve_of_eratosthenes(highest)

	print *, sum

end program
