module results_interface
  implicit none

  type results_t
    real, allocatable :: output(:, :)
  contains
    procedure :: max_filtered_normalized_distance
  end type

contains

  pure module function max_filtered_normalized_distance(this, rhs)
    class(results_t), intent(in) :: this
    type(results_t), intent(in) :: rhs
    real :: max_filtered_normalized_distance
    integer, parameter :: mdotos_column=4, thrust_column=5
    real, allocatable :: rhs_filtered(:,:)
    type(results_t) distance

    rhs_filtered = rhs%output

    max_filtered_normalized_distance = maxval(rhs%output)
  end function

end module
