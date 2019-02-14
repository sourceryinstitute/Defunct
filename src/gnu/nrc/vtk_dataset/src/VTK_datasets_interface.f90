MODULE vtk_datasets
    USE Precision, only : i4k, r8k
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: rectlnr_grid

    TYPE :: coordinates
        CHARACTER(LEN=:),        ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: name
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        INTEGER(i4k), DIMENSION(3)    :: dimensions
        LOGICAL, PUBLIC               :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: init
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: rectlnr_grid
        PRIVATE
        TYPE (coordinates) :: x
        TYPE (coordinates) :: y
        TYPE (coordinates) :: z
    CONTAINS
        PROCEDURE :: read  => rectlnr_grid_read
        PROCEDURE :: write => rectlnr_grid_write
        PROCEDURE, PRIVATE :: setup => rectlnr_grid_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_rectlnr_grid
    END TYPE rectlnr_grid

    INTERFACE

        MODULE SUBROUTINE abs_read (me, unit)
        CLASS(dataset), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit
        END SUBROUTINE abs_read

        MODULE SUBROUTINE abs_write (me, unit)
        CLASS(dataset), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit
        END SUBROUTINE abs_write

        MODULE SUBROUTINE init (me, dims, x_coords, y_coords, z_coords)
        CLASS(dataset), INTENT(OUT) :: me
        INTEGER(i4k),        DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords,  y_coords, z_coords
        END SUBROUTINE init

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        CLASS(dataset), INTENT(IN) :: me, you
        LOGICAL :: diffs
        END FUNCTION check_for_diffs

        MODULE SUBROUTINE rectlnr_grid_read (me, unit)
        CLASS(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit
        END SUBROUTINE rectlnr_grid_read

        MODULE SUBROUTINE rectlnr_grid_write (me, unit)
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: unit
        END SUBROUTINE rectlnr_grid_write

        MODULE SUBROUTINE rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords)
        CLASS (rectlnr_grid),       INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: x_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: y_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: z_coords
        END SUBROUTINE rectlnr_grid_setup

        MODULE FUNCTION check_for_diffs_rectlnr_grid (me, you) RESULT (diffs)
        CLASS(rectlnr_grid), INTENT(IN) :: me
        CLASS(dataset),      INTENT(IN) :: you
        LOGICAL                         :: diffs
        END FUNCTION check_for_diffs_rectlnr_grid

    END INTERFACE

END MODULE vtk_datasets
