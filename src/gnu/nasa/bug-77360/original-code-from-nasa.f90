program gcc61bug
  implicit none
  integer, parameter :: strlen=8
  integer :: n,nn,linecount
  character(len=strlen), dimension(:), allocatable :: linelist
  character(len=strlen) :: thisline
  !character(len=strlen), dimension(1) :: skiplist
  integer :: nskip,nkeep

  linecount = 4
  allocate(linelist(linecount))

  linelist(1) = 'line1'
  linelist(2) = 'skip'
  linelist(3) = 'line3'
  linelist(4) = 'line4'

  if(.false.) then ! note: NOT EXECUTED, but presence affects results
    nn = 0
    do n=1,linecount
      if(len_trim(linelist(n)) == 0) cycle
      nn = nn + 1
      if(nn /= n) linelist(nn) = linelist(n)
    enddo
  endif

  nskip = 0
  nkeep = 0
  do n=1,linecount
    thisline = linelist(n)
    if(trim(thisline) == 'skip') then
      nskip = nskip + 1
      !skiplist(nskip) = thisline
    else
      nkeep = nkeep + 1
      if(nkeep /= n) linelist(nkeep) = linelist(n)
    endif
  enddo

  linecount = nkeep
  do n=1,linecount
    write(6,*) n,trim(linelist(n))
  enddo

  ! above printout should be, but is often different:
  ! 1 line1
  ! 2 line3
  ! 3 line4

end program gcc61bug
