!Work out the first ten digits of the sum 
!of the following one-hundred 50-digit numbers.
program large_sum
	implicit none
	integer, parameter :: n = 100
	character(len=50) :: numbers(n)
	character(len=53) :: numb
	integer :: i, j, carry, digit
	integer :: sum(54)
	integer :: first_non_zero
	
	
	call read_numbers_from_file('numbers.txt', numbers, n)
	
	sum = 0
	carry = 0
	do i = 1, n
        numb = '000' // trim(numbers(i))

        !add numbers from right to left
        do j = 53, 1, -1
            digit = ichar(numb(j:j)) - ichar('0')
            sum(j+1) = sum(j+1) + digit + carry  !account for the carry
            carry = sum(j+1) / 10
            sum(j+1) = mod(sum(j+1), 10)
        end do
        sum(1) = sum(1) + carry  !add remaining carry
        carry = 0
    end do
	
	first_non_zero = 1
	do j = 1, 53
		if (sum(j+1) /= 0) then
		    first_non_zero = j + 1
		    exit
		end if
	end do

	
	do j = first_non_zero, first_non_zero + 9
		write(*, '(I1)', advance='no') sum(j)
	end do
	print *
	
	
end program


subroutine read_numbers_from_file(filename, numbers, n)
	implicit none
	character(len=*), intent(in) :: filename
	character(len=50), intent(out) :: numbers(n)
	integer, intent(IN) :: n  !number of numbers to read
	integer :: i, ios
	character(len=60) :: line

	!open file
	open(unit=10, file=filename, status='old', action='read')

	!read numbers from file
	i = 1
	do while (.true.)
		read(10, '(A)', iostat=ios) line
		if (ios /= 0) exit  !exit if end of file or error

		numbers(i) = line
		i = i + 1
	end do

	!close file
	close(10)
end subroutine read_numbers_from_file
