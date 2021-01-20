! test module
module test
  implicit none

  private

  integer :: i = selected_real_kind(15,300)

contains

  subroutine test_sub()
    i = 1
  end subroutine test_sub
end module test
