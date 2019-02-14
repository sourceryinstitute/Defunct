MODULE Misc
    USE ISO_FORTRAN_ENV, ONLY : i4k => INT32, i8k => INT64, r4k => REAL32, r8k =>REAL64
    IMPLICIT NONE
CONTAINS
        MODULE SUBROUTINE interpret_string (line, datatype, ignore, separator, reals, ints, chars)
        CHARACTER(LEN=*), INTENT(INOUT) :: line
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: ignore, separator
        CHARACTER(LEN=1), DIMENSION(:), INTENT(IN) :: datatype
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE, OPTIONAL :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE, OPTIONAL :: reals
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE, OPTIONAL :: chars
        INTEGER(i4k) :: i
        CHARACTER(LEN=:), ALLOCATABLE :: string, sep, char
        TYPE :: counter
            INTEGER(i4k) :: t = 0, i = 0, r = 0, c = 0
        END TYPE counter
        TYPE (counter) :: cnt

        IF (PRESENT(ignore)) THEN
            string = TRIM(ADJUSTL(line(INDEX(line,ignore)+LEN(ignore):)))
        ELSE
            string = TRIM(ADJUSTL(line))
        END IF
        IF (PRESENT(separator)) THEN
            sep = separator
        ELSE
            sep = ' '
        END IF
        IF (PRESENT(ints)) THEN
            IF (ALLOCATED(ints)) DEALLOCATE(ints)
            ALLOCATE(ints(1:SIZE(datatype)),source=0_i4k)
        END IF
        IF (PRESENT(reals)) THEN
            IF (ALLOCATED(reals)) DEALLOCATE(reals)
            ALLOCATE(reals(1:SIZE(datatype)),source=0.0_r8k)
        END IF
        IF (PRESENT(chars)) THEN
            IF (ALLOCATED(chars)) DEALLOCATE(chars)
            ALLOCATE(chars(1:SIZE(datatype)),source=string)
        END IF
        DO i = 1, SIZE(datatype)
            SELECT CASE (datatype(i))
            CASE ('I', 'i')
                !! Integer
                cnt%i = cnt%i + 1
                BLOCK
                  CHARACTER(LEN=:), ALLOCATABLE :: text
                  IF (INDEX(string,sep) == 0) THEN
                      text = string(1:)                    !! Read to end of string
                  ELSE
                      text = string(1:INDEX(string,sep)-1) !! Read until sep is found
                  END IF
                  READ(text,'(i8)') ints(cnt%i)           !! Store value
                END BLOCK
            CASE ('R', 'r')
                !! Real
                cnt%r = cnt%r + 1
                BLOCK
                  CHARACTER(LEN=:), ALLOCATABLE :: text
                  IF (INDEX(string,sep) == 0) THEN
                      text = string(1:)                    !! Read to end of string
                  ELSE
                      text = string(1:INDEX(string,sep)-1) !! Read until sep is found
                  END IF
                  READ(text,'(es13.6)') reals(cnt%r) !! Store value
                END BLOCK
            CASE ('C', 'c')
                !! Character
                cnt%c = cnt%c + 1
                IF (INDEX(string,sep) == 0) THEN
                    chars(cnt%c) = string(1:)                    !! Read to end of string
                ELSE
                    chars(cnt%c) = string(1:INDEX(string,sep)-1) !! Read until sep is found
                END IF
            END SELECT
            IF (INDEX(string,sep) == 0) THEN
                string = ''
            ELSE
                string = ADJUSTL(string(INDEX(string,sep)+LEN(sep):))
            END IF
            cnt%t = cnt%t + 1
         END DO
        line = string
        END SUBROUTINE interpret_string
END MODULE Misc
