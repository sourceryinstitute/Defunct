contains
  subroutine array_to_vector(array)
    class(*), allocatable :: vector(:),array(:,:)
    allocate(vector,source=pack(array,.true.))
  end subroutine
end
