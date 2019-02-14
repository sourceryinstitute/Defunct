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
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: rectlnr_grid
        PRIVATE
        TYPE (coordinates) :: x
        TYPE (coordinates) :: y
        TYPE (coordinates) :: z
    CONTAINS
        PROCEDURE :: rectlnr_grid_setup
        PROCEDURE :: read  => rectlnr_grid_read
        PROCEDURE :: write => rectlnr_grid_write
        PROCEDURE :: check_for_diffs_rectlnr_grid
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs_rectlnr_grid
    END TYPE rectlnr_grid

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

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I','I' /), ignore='DIMENSIONS ',  separator=' ', ints=ints)
        me%dimensions = ints(1:3); ALLOCATE(me%x%coord(1:ints(1)), me%y%coord(1:ints(2)), me%z%coord(1:ints(3)))

        end_of_file = .FALSE.; i = 0

        get_coords: DO
            i = i + 1
            READ(unit,100,iostat=iostat) line
            CALL interpret_string (line=line, datatype=(/ 'I','C' /), ignore=descr_coord(i), separator=' ', ints=ints, chars=chars)
            SELECT CASE (i)
            CASE (1)
                me%x%datatype = TRIM(chars(1))
            CASE (2)
                me%y%datatype = TRIM(chars(1))
            CASE (3)
                me%z%datatype = TRIM(chars(1))
            END SELECT

            READ(unit,100,iostat=iostat) line
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

100     FORMAT((a))
        END SUBROUTINE rectlnr_grid_read

        MODULE SUBROUTINE rectlnr_grid_write (me, unit)
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: unit

        !>@brief
        !> Writes the rectilinear grid dataset information to the .vtk file

        WRITE(unit,100) me%name
        WRITE(unit,101) me%dimensions
        WRITE(unit,102) me%dimensions(1), me%x%datatype
        WRITE(unit,110) me%x%coord
        WRITE(unit,103) me%dimensions(2), me%y%datatype
        WRITE(unit,110) me%y%coord
        WRITE(unit,104) me%dimensions(3), me%z%datatype
        WRITE(unit,110) me%z%coord

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('X_COORDINATES ',i0,' ',(a))
103     FORMAT ('Y_COORDINATES ',i0,' ',(a))
104     FORMAT ('Z_COORDINATES ',i0,' ',(a))
110     FORMAT (*(es13.6))

        END SUBROUTINE rectlnr_grid_write

        MODULE SUBROUTINE rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords)
        CLASS (rectlnr_grid),       INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: x_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: y_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: z_coords

        !>@brief
        !> Sets up the rectilinear grid dataset with information

        IF (dims(1) /= SIZE(x_coords) .OR. dims(2) /= SIZE(y_coords) .OR. dims(3) /= SIZE(z_coords)) THEN
            ERROR STOP 'Bad inputs for rectlnr_grid_setup. Dims is not equal to size of coords.'
        END IF

        me%name       = 'RECTILINEAR_GRID'
        me%dimensions = dims
        me%y%datatype = me%x%datatype; me%z%datatype = me%x%datatype
        me%x%coord    = x_coords
        me%y%coord    = y_coords
        me%z%coord    = z_coords
        me%firstcall  = .FALSE.

        END SUBROUTINE rectlnr_grid_setup

        MODULE FUNCTION check_for_diffs_rectlnr_grid (me, you) RESULT (diffs)
        CLASS(rectlnr_grid), INTENT(IN) :: me
        CLASS(dataset),      INTENT(IN) :: you
        LOGICAL                         :: diffs

        !>@brief
        !> Function checks for differences in a rectilinear grid dataset
        INTEGER(i4k) :: i

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%name          /= you%name) THEN
            diffs = .TRUE.
        ELSE IF (me%dimensions(1) /= you%dimensions(1) .OR. &
          &      me%dimensions(2) /= you%dimensions(2) .OR. &
          &      me%dimensions(3) /= you%dimensions(3)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (rectlnr_grid)
                IF      (me%x%datatype /= you%x%datatype .OR. &
                  &      me%y%datatype /= you%y%datatype .OR. &
                  &      me%z%datatype /= you%z%datatype) THEN
                    diffs = .TRUE.
                ELSE IF (SIZE(me%x%coord) /= SIZE(you%x%coord) .OR. &
                  &      SIZE(me%y%coord) /= SIZE(you%y%coord) .OR. &
                  &      SIZE(me%z%coord) /= SIZE(you%z%coord)) THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, SIZE(me%x%coord)
                        IF (me%x%coord(i) /= you%x%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                    DO i = 1, SIZE(me%y%coord)
                        IF (me%y%coord(i) /= you%y%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                    DO i = 1, SIZE(me%z%coord)
                        IF (me%z%coord(i) /= you%z%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_rectlnr_grid
END MODULE vtk_datasets
