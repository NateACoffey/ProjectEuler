!Work out the first ten digits of the sum 
!of the following one-hundred 50-digit numbers.
program large_sum
	implicit none
	integer, parameter :: n = 100
	character(len=50) :: numbers(n)
	integer :: i, ios
	integer(kind=8) :: sum, integer_var, total
	character(len=50) :: char_var
	character(len=10) :: first_ten
	
	
	call read_numbers_from_file('numbers.txt', numbers, n)
	
	sum = 0
	
	do i = 1, SIZE(numbers)
		READ(numbers(i)(1:15), '(I15)') integer_var
		sum = sum + integer_var
	end do
	
	WRITE(char_var, '(I50)') sum
	
	first_ten = trim(adjustl(char_var))
	
	print *, first_ten
	
end program

! Subroutine to read numbers from the file
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
