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
    END TYPE

    CONTAINS

        MODULE SUBROUTINE rectlnr_grid_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        TYPE(rectlnr_grid), INTENT(OUT) :: me
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

END MODULE
