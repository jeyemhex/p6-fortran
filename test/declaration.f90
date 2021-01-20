program declaration
  implicit none

  integer, parameter  :: dp
  real(kind=dp)       :: x, y
  real(kind=dp), dimension(:), allocatable :: V

  x = 3
  y = 2

  V = x + y

end program declaration
