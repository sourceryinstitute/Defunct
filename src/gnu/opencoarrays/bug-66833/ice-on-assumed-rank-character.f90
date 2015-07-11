subroutine gfc_descriptor_c_char(a) 
  character a(..)
  print *,rank(a) ! ICE (also for lbound, ubound, and c_loc)
end subroutine
