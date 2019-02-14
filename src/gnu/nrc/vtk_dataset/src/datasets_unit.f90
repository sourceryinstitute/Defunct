  USE ISO_FORTRAN_ENV, ONLY : i4k => INT32, i8k => INT64, r4k => REAL32, r8k =>REAL64
  IMPLICIT NONE
  TYPE coordinates
      CHARACTER(LEN=:),        ALLOCATABLE :: datatype
      REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord
  END TYPE
  TYPE rectlnr_grid
      CHARACTER(LEN=:), ALLOCATABLE :: name
      CHARACTER(LEN=:), ALLOCATABLE :: datatype
      INTEGER(i4k), DIMENSION(3)    :: dimensions
      LOGICAL :: firstcall = .TRUE.
      TYPE (coordinates) :: x, y, z
  END TYPE
  TYPE(rectlnr_grid) u, v
  REAL(r8k), PARAMETER ::  &
    x(*) = [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ], &
    y(*) = [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ], &
    z(*) = [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
  u%name       = 'RECTILINEAR_GRID'
  u%dimensions = [size(x),size(y),size(z)]
  u%y%datatype = u%x%datatype; u%z%datatype = u%x%datatype
  u%x%coord    = x
  u%y%coord    = y
  u%z%coord    = z
  u%firstcall  = .FALSE.
  OPEN (20, file='rectlnr_grid.vtk', form='formatted')
  WRITE(20,"('DATASET ',(a))") u%name
  WRITE(20,"('DIMENSIONS ',*(i0,' '))") u%dimensions
  WRITE(20,"('X_COORDINATES ',i0,' ',(a))") u%dimensions(1), u%x%datatype
  WRITE(20,"(*(es13.6))") u%x%coord
  WRITE(20,"('Y_COORDINATES ',i0,' ',(a))") u%dimensions(2), u%y%datatype
  WRITE(20,"(*(es13.6))") u%y%coord
  WRITE(20,"('Z_COORDINATES ',i0,' ',(a))") u%dimensions(3), u%z%datatype
  WRITE(20,"(*(es13.6))") u%z%coord
  CLOSE(20)
  OPEN (20, file='rectlnr_grid.vtk', form='formatted', status='old')
  CALL rectlnr_grid_read(v,20)
  print*,v%x%datatype /= u%x%datatype
CONTAINS
        SUBROUTINE rectlnr_grid_read (me, unit)
        USE Misc, ONLY : interpret_string
        TYPE(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit
        INTEGER(i4k)                     :: i, j, iostat
        INTEGER(i4k),        PARAMETER   :: dim = 3
        LOGICAL                          :: end_of_File
        CHARACTER(LEN=1000)           :: line
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
END
