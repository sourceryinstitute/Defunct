module foo
contains
  subroutine bar()
    integer, save :: i[*]=0
    associate(n=>1)
      print*,i[1]
    end associate
  end subroutine
end module

use foo
end
