MODULE vtk_datasets
    USE Precision, only : i4k, r8k
    IMPLICIT NONE

    TYPE coordinates
        CHARACTER(LEN=:),        ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord
    END TYPE

    TYPE rectlnr_grid
        CHARACTER(LEN=:), ALLOCATABLE :: name
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        INTEGER(i4k), DIMENSION(3)    :: dimensions
        LOGICAL, PUBLIC               :: firstcall = .TRUE.
        TYPE (coordinates) :: x, y, z
    CONTAINS
        PROCEDURE :: rectlnr_grid_setup
        PROCEDURE :: rectlnr_grid_read
    END TYPE

    CONTAINS

        MODULE SUBROUTINE rectlnr_grid_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        CLASS(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit

        !>@brief
        !> Reads the rectilinear grid dataset information from the .vtk file

        INTEGER(i4k)                     :: i, j, iostat
        INTEGER(i4k),        PARAMETER   :: dim = 3
        LOGICAL                          :: end_of_File
        CHARACTER(LEN=def_len)           :: line
        INTEGER(i4k),      DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),         DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:),  DIMENSION(:),   ALLOCATABLE :: chars
        CHARACTER(LEN=13), DIMENSION(3),   PARAMETER   :: descr_coord = &
          & [ 'X_COORDINATES', 'Y_COORDINATES', 'Z_COORDINATES' ]

        READ(unit,"(a)",iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,"(a)",iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I','I' /), ignore='DIMENSIONS ',  separator=' ', ints=ints)
        me%dimensions = ints(1:3); ALLOCATE(me%x%coord(1:ints(1)), me%y%coord(1:ints(2)), me%z%coord(1:ints(3)))

        end_of_file = .FALSE.; i = 0

        get_coords: DO
            i = i + 1
            READ(unit,"(a)",iostat=iostat) line
            CALL interpret_string (line=line, datatype=(/ 'I','C' /), ignore=descr_coord(i), separator=' ', ints=ints, chars=chars)
            SELECT CASE (i)
            CASE (1)
                me%x%datatype = TRIM(chars(1))
            CASE (2)
                me%y%datatype = TRIM(chars(1))
            CASE (3)
                me%z%datatype = TRIM(chars(1))
            END SELECT

            READ(unit,"(a)",iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_coords
            ELSE
                j = 0
                get_vals: DO
                    j = j + 1
                    CALL interpret_string (line=line, datatype=(/ 'R' /), separator=' ', reals=reals)
                    SELECT CASE (i)
                    CASE (1)
                        me%x%coord(j) = reals(1)
                    CASE (2)
                        me%y%coord(j) = reals(1)
                    CASE (3)
                        me%z%coord(j) = reals(1)
                    END SELECT
                    IF (line == '') EXIT get_vals
                END DO get_vals
            END IF
            IF (i == dim) EXIT get_coords  !! Filled up array points
        END DO get_coords

        END SUBROUTINE rectlnr_grid_read

        MODULE SUBROUTINE rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords)
        CLASS (rectlnr_grid),       INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: x_coords, y_coords, z_coords

        me%name       = 'RECTILINEAR_GRID'
        me%dimensions = dims
        me%y%datatype = me%x%datatype; me%z%datatype = me%x%datatype
        me%x%coord    = x_coords
        me%y%coord    = y_coords
        me%z%coord    = z_coords
        me%firstcall  = .FALSE.

        END SUBROUTINE rectlnr_grid_setup

        LOGICAL MODULE FUNCTION diff(me, you)
        TYPE(rectlnr_grid), INTENT(IN) :: me, you
        diff = merge(.TRUE.,.FALSE., &
          ANY([me%x%datatype /= you%x%datatype, me%y%datatype /= you%y%datatype, me%z%datatype /= you%z%datatype]) &
        )
        END FUNCTION
END MODULE vtk_datasets
