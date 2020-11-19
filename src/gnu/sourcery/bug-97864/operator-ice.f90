    implicit none

    type VARYING_STRING
        character(len=1), allocatable :: characters(:)
    end type

    interface operator(==)
        procedure character_EQ_String
    end interface

    print *, stringToChar(var_str("Hello")) == var_str("World") ! causes ice

contains
    logical function character_EQ_String(lhs, rhs)
        character(len=*), intent(in) :: lhs
        type(VARYING_STRING), intent(in) :: rhs
        character_EQ_String = lhs == stringToChar(rhs)
    end function

    function stringToChar(string)
        type(VARYING_STRING) string
        character(len=size(string%characters)) :: stringToChar
        stringToChar = ""
    end function

    type(VARYING_STRING) function VAR_STR(char)
        character(len=*) char
        integer i
        VAR_STR%characters = [(char(i:i), i = 1, len(char))]
    end function
end
