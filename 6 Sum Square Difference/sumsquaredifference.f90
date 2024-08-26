!Find the difference between the sum of the squares of 
!the first one hundred natural numbers and the square of the sum
module math
	implicit none
	
contains
	!calculates 1^2 + 2^2 + . . . + n^2
	function sum_of_n_squares(n) result(sum)
		implicit none
		integer, intent(in) :: n
		integer(kind=8) :: sum
		
		sum = (n * (n + 1) * (2 * n + 1)) / 6
		
	end function sum_of_n_squares
	
	!calucates (1 + 2 + . . . + n)^2
	function square_of_n_sum(n) result(sum)
		implicit none
		integer, intent(in) :: n
		integer(kind=8) :: sum
		
		sum = ((n * (n + 1)) / 2) ** 2
		
	end function square_of_n_sum
end module math

program sum_square_difference
	use math
	implicit none
	integer :: n
	integer(kind=8) :: difference
	
	n = 100
	
	difference = square_of_n_sum(n) - sum_of_n_squares(n)
	
	print *, difference

end program
