program testit
  use iso_fortran_env, only : output_unit
  ! Program illustrating problem in gcc61 copying elements of an array of strings between positions.
  ! Problem only occurs when the code below is embedded within a large and nontrivial context
  ! and is therefore difficult to reproduce.
  implicit none
  character(len=256), dimension(:), allocatable :: linelist
  integer :: n

  ! assign a sequence of blank and non-blank lines (normally read from a file)
  linelist = ['     ','line2','     ','line4']

  ! filter out blank lines
  linelist = pack(linelist,len_trim(linelist)>0)

  do n=1,size(linelist)
    write(output_unit,*) trim(linelist(n))
  end do
end program testit
