  type AA
    procedure(foo), pointer :: funct
  end type 
contains
   function foo(A)
     class(AA) A
     type(AA) foo
   end function 
end 
