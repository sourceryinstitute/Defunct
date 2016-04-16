  use iso_fortran_env
  type(event_type) :: x[*] 
contains
  subroutine exchange
    event post(x[1])
  end subroutine
end 
