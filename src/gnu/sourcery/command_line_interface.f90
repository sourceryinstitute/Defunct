module command_line_interface
  implicit none
  type command_line
  contains
    procedure, nopass :: argument_present
  end type
  interface
    module function argument_present(acceptable_argument) result(found)
      character(len=*), intent(in) :: acceptable_argument(:)
      logical found
    end function
  end interface
contains
  module procedure argument_present
    integer :: i, argnum, arglen
    character(len=32) arg
    associate(acceptable_length => [(len(trim(acceptable_argument(i))), i = 1, size(acceptable_argument))])
      found = .false.
      do argnum = 1,command_argument_count()
        call get_command_argument(argnum, arg, arglen)
        if (any( &
          [(arg==acceptable_argument(i) .and. arglen==acceptable_length(i), i = 1, size(acceptable_argument))] &
        )) then
          found = .true.
        end if
      end do
    end associate
  end procedure
end module
