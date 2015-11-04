module Exception_mod
  type Exception
  end type 
  type ExceptionList
  contains
     generic :: throw => throwException
     procedure :: throwException
  end type 
contains
  subroutine throwException(this, anException)
     class(ExceptionList) :: this
     type (Exception) :: anException
  end subroutine 
  subroutine throwMessage(this)
     class(ExceptionList) :: this
     call this%throw(Exception()) ! this should resolve into a call to throwException
  end subroutine 
end module 
