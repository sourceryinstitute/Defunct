module ISO_Fortran_binding_module
  use iso_c_binding, only : c_ptr, c_loc, c_f_pointer, c_int
  implicit none

  type, bind(C) :: CFI_cdesc_t
    type(c_ptr) base_addr
  end type

contains

  type(c_ptr) function integer_CFI_address(dv) bind(C)
    type(CFI_cdesc_t), intent(in), target :: dv
    integer(c_int), pointer :: array(:)
    call c_f_pointer(dv%base_addr,array,[1])
    integer_CFI_address = c_loc(array)
  end function

  type(c_ptr) function generic_CFI_address(dv) bind(C)
    type(CFI_cdesc_t), intent(in), target :: dv
    class(*), pointer :: array(:)
    call c_f_pointer(dv%base_addr,array,[1])
    generic_CFI_address = c_loc(array)
  end function

end module

  use iso_c_binding, only : c_loc, c_int, c_associated
  use  ISO_Fortran_binding_module, only : integer_CFI_address, generic_CFI_address, CFI_cdesc_t
  implicit none
  integer(c_int), target :: fortran_array(1)=[99_c_int]
  type(CFI_cdesc_t), target ::array_descriptor
  array_descriptor%base_addr = c_loc(fortran_array)
  print*, c_associated( array_descriptor%base_addr , integer_CFI_address(array_descriptor) )
  print*, c_associated( array_descriptor%base_addr , generic_CFI_address(array_descriptor) )

end
