module hole_interface

  type hole_t
  contains
    procedure set_user_defined
  end type

  interface
    module subroutine set_diameter (this)
      class(hole_t) this
    end subroutine

    module subroutine set_user_defined(this)
      class(hole_t) this
    end subroutine
  end interface

contains
    module procedure set_user_defined
    end procedure

    module procedure set_diameter
      call this%set_user_defined
    end procedure
end module

  use hole_interface
end
