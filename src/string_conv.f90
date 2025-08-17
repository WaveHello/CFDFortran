module string_conv
   use stdlib_kinds, only: dp, i32 => int32, sp
   implicit none



   interface num2str
      module procedure real_16_to_str
      module procedure real_32_to_str
      module procedure int_32_to_str
   end interface num2str

contains
   function real_16_to_str(arg) result(str)
      real(sp), intent(in) :: arg
      character(len=:), allocatable :: str

      ! Local variables
      character(len=32) :: tmp_str

      write(tmp_str, "(F0.4)") arg
      str = trim(adjustl(tmp_str))
   end function real_16_to_str

   function real_32_to_str(arg) result(str)
      real(dp), intent(in) :: arg
      character(len=:), allocatable :: str

      ! Local variables
      character(len=32) :: tmp_str

      write(tmp_str, "(F0.8)") arg
      str = trim(adjustl(tmp_str))
   end function real_32_to_str

   function int_32_to_str(arg) result(str)
      integer(i32), intent(in) :: arg
      character(len=:), allocatable :: str

      ! Local variables
      character(len=12) :: tmp_str

      write(tmp_str, "(I0)") arg

      str = trim(adjustl(tmp_str))
   end function int_32_to_str


end module string_conv
