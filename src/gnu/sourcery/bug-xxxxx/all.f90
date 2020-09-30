module results_interface
  implicit none

  type results_t
    real, allocatable :: output(:, :)
  end type

contains

  pure module subroutine max_filtered_normalized_distance(this, rhs)
    class(results_t), intent(in) :: this
    type(results_t), intent(in) :: rhs
    real, allocatable :: rhs_filtered(:,:)

    rhs_filtered = rhs%output

  end subroutine

end module
