program largestpalindrome
	implicit none
	integer :: digits
	integer :: product
	
	digits = 3
	
	product = findProduct(digits)
	
	print *, product
	
	
contains
	function findProduct(digits) result(product)
		implicit none
		integer :: digits
		integer :: product
		integer :: maxFactor, minFactor
		integer :: i, j, currProduct
		
		!max number to be multplied
		maxFactor = 10**digits - 1
		minFactor = 10**(digits - 1)
		
		product = 0
		
		do i = maxFactor, minFactor, -1
			do j = i - 1, minFactor, -1
				currProduct = i * j
				!early exit
				if(currProduct .lt. product) then
					exit
				end if
				
				if((currProduct .gt. product) .and. isPalindrome(currProduct))then
					product = currProduct
					exit
				end if
			
			end do
			!early exit 2
			if(i * (i - 1) .lt. product) then
				exit
			end if
		end do
	
	end function findProduct
	
	
	function isPalindrome(original) result(palindrome)
		implicit none
		integer, intent(in) :: original
		integer :: numb, reverse
		integer :: remainder
		logical :: palindrome
		
		numb = original
		reverse = 0
		!reverse the original number
		do while(numb .ne. 0)
			remainder = MOD(numb, 10)
			reverse = reverse * 10 + remainder
			numb = numb / 10
		end do
		
		
		palindrome = (original .eq. reverse)
	
	end function isPalindrome
	
end program
