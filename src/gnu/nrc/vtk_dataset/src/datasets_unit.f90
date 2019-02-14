  USE vtk_datasets, ONLY : rectlnr_grid
  USE Precision, ONLY : r8k
  IMPLICIT NONE
  TYPE(rectlnr_grid) u, v
  REAL(r8k), PARAMETER ::  &
    x(*) = [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ], &
    y(*) = [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ], &
    z(*) = [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
  CALL u%rectlnr_grid_setup([size(x),size(y),size(z)], x, y, z)
  OPEN (unit=20, file='rectlnr_grid.vtk', form='formatted')
  CALL u%rectlnr_grid_write(20)
  CLOSE(unit=20)
  OPEN (unit=20, file='rectlnr_grid.vtk', form='formatted', status='old')
  CALL v%rectlnr_grid_read(20)
  PRINT*, u%check_for_diffs_rectlnr_grid(v)," <-- should be F"
END
