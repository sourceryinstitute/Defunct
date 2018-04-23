module foo
contains
  subroutine bar()
    integer, save :: i[*]=0
    block
      integer n
      n=1
      print*,i[1]
    end block
  end subroutine
end module

use foo
end
