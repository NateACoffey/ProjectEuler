!Which starting number, under one million, produces the longest chain
module math
	implicit none
	
contains
	function is_even(numb) result(even)
		implicit none
		integer(kind=8) :: numb
		logical :: even
		
		even = MOD(numb, 2) .eq. 0
		
	end function is_even
	
	function collatz_sequence(start) result(length)
		implicit none
		integer(kind=8), intent(in) :: start
		integer(kind=8) :: i, length
		
		length = 1
		i = start
		
		do while(i .gt. 1)
			if(is_even(i)) then
				i = i / 2
			else
				i = 3 * i + 1
			end if
			length = length + 1
		end do
		
	end function collatz_sequence
	
end module math


program longest_collatz_sequence
	use math
	implicit none
	integer(kind=8) :: i, length, curr, numb
	
	length = 0
	numb = 1
	
	i = 1
	do while(i .lt. 1e6)
		curr = collatz_sequence(i)
		if(curr .gt. length) then
			length = curr
			numb = i
		end if
		i = i + 1
	end do
	
	print *, numb, length
	
end program longest_collatz_sequence
