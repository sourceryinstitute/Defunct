module assertions_interface
  implicit none
contains
  elemental module subroutine assert(assertion,description,success)
    use iso_fortran_env, only : error_unit
    implicit none
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    logical, intent(out), optional :: success
    character(len=:), allocatable :: message
    integer, parameter :: max_appended_characters=24
    if (present(success)) success=assertion
    if (.not.assertion) then
      message = repeat(" ",ncopies=len(description)+max_appended_characters)
      write(message,*) '(',description,') on image',this_image()
      if (.not. present(success)) error stop "Assertion failed" // message
    end if
  end subroutine
end module

program main
  use emulated_collectives_interface, only : co_sum_integer, co_broadcast_integer
  use assertions_interface, only : assert
  implicit none

  associate( me=>this_image(), ni=>num_images() )

   !call assert(ni>3,"test-emulated-collectives: at least 4 images required")

    test_collective_broadcast: block
      integer ::i, messenger, message

      message=333
      messenger=1
      if (me==messenger) i=message
      call co_broadcast_integer(i,source_image=messenger)
      call assert(i==message,"integer message broadcast")

      stop "stopped inside main"

      message=666
      messenger=4
      if (me==messenger) i=message
      call co_broadcast_integer(i,source_image=messenger)
      call assert(i==message,"integer message broadcast")

    end block test_collective_broadcast

    test_collective_sum: block
      integer ::i, image

      image = me
      call co_sum_integer(image,result_image=1)
      if (me==1) call assert(image==sum([(i,i=1,ni)]),"collective integer sum reduction accumulated on image 1")

      sync all

      image = me
      call co_sum_integer(image)
      call assert(image==sum([(i,i=1,ni)]),"collective integer sum accumulated on all images")

      sync all

      image = me
      call co_sum(image,result_image=2)
      if (me==2) call assert(image==sum([(i,i=1,ni)]),"correct integer collective sum reduction")

    end block test_collective_sum

    sync all
    if (me==1) print *,"Test passed."

  end associate
end program

