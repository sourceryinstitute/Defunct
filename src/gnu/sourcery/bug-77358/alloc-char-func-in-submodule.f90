module hello_interface
  character(len=13) :: string="Hello, world!"
  interface
    module function get() result(result_string)
      character(:), allocatable :: result_string
    end function
  end interface
end module

submodule(hello_interface) hello_implementation
contains
  module function get() result(result_string)
    character(:), allocatable :: result_string
    result_string = string
  end function
end submodule

  use hello_interface
  print *,len(get())
end
