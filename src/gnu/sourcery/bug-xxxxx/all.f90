module results_interface
  implicit none

  type results_t
    real, allocatable :: output(:)
  end type

contains

  subroutine max_filtered_normalized_distance(rhs)
    type(results_t)rhs
    real, allocatable :: rhs_filtered(:)
    rhs_filtered = rhs%output
  end subroutine

end module
