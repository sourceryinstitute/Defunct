  use iso_fortran_env
  integer :: nc(1)
  type(event_type) done[*]
  event wait(done,until_count=nc(1))
end
