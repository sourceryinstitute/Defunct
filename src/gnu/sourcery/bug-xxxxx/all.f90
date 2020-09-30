module results_interface

    implicit none
    private

    type, public :: results_t
        private
        real, allocatable :: output(:, :)
    contains
        procedure :: max_filtered_normalized_distance
    end type

    interface
        pure module function max_filtered_normalized_distance(this, rhs)
            implicit none
            class(results_t), intent(in) :: this
            type(results_t), intent(in) :: rhs
            real :: max_filtered_normalized_distance
        end function
    end interface

contains

    module procedure max_filtered_normalized_distance
        integer, parameter :: mdotos_column=4, thrust_column=5
        real, allocatable :: rhs_filtered(:,:)
        type(results_t) distance

        rhs_filtered = rhs%output

        max_filtered_normalized_distance = maxval(rhs%output)
    end procedure

end module
