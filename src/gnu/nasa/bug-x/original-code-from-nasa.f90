program testit
  ! Program illustrating problem in gcc61 copying elements of an array of strings between positions.
  ! Problem only occurs when the code below is embedded within a large and nontrivial context
  ! and is therefore difficult to reproduce.
  implicit none
  character(len=256), dimension(:), allocatable :: linelist
  character(len=256) :: thisline
  integer :: n,nn
  ! assign a sequence of blank and non-blank lines (normally read from a file)
  allocate(linelist(4))
  linelist(1) = ''
  linelist(2) = 'line2'
  linelist(3) = ''
  linelist(4) = 'line4'
  ! filter out blank lines
  n = 0
  do nn=1,size(linelist)
    if(len_trim(linelist(nn)).eq.0) cycle
    n = n + 1
    if(n.ne.nn) then
      linelist(n) = linelist(nn)
      !workaround! thisline = linelist(nn); linelist(n) = thisline
    endif
  enddo
  ! write out result
  do nn=1,n
    write(6,*) trim(linelist(nn))
  enddo

end program testit
