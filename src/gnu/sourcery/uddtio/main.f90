program main
  use results_interface, only : results_t
  type(results_t) actual, expected

  write(*,*) actual - expected

end program
