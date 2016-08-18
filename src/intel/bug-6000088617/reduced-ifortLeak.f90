  type adata

   endtype

   type RealRef

     class(adata),allocatable :: d ! To eliminate leak, remove this line or switch to type(adata) or pointer 

   end type

   type RealVec

     type(RealRef),allocatable :: vec(:) ! Switch to pointer to eliminate leak

   end type

   type(RealVec) :: rv

   rv=NewRealVec()

contains

  function newRealVec() 

    type(RealVec) :: newRealVec

    allocate(newRealVec%vec(1)) 

  end function

end 
