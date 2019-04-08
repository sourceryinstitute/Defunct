module link_error
contains
  subroutine post_event_in_associate
    use iso_fortran_env
    type(event_type), save :: e[*]
    associate( i => 1 )
      event post(e[1])
    end associate
  end
end module
end
