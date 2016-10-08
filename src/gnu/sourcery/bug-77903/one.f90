module one_module
  implicit none
  interface
    module function one()
    end function
  end interface
end module

submodule(one_module) one_submodule
  implicit none
contains
  integer module function one()
    one = 1
  end function
end submodule  

use one_module
print *,one()," is not ",1
end 
