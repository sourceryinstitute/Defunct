  USE vtk_datasets, ONLY : rectlnr_grid, diff
  USE Precision, ONLY : r8k
  IMPLICIT NONE
  TYPE(rectlnr_grid) u, v
  REAL(r8k), PARAMETER ::  &
    x(*) = [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ], &
    y(*) = [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ], &
    z(*) = [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]

  u%name       = 'RECTILINEAR_GRID'
  u%dimensions = [size(x),size(y),size(z)]
  u%y%datatype = u%x%datatype; u%z%datatype = u%x%datatype
  u%x%coord    = x
  u%y%coord    = y
  u%z%coord    = z
  u%firstcall  = .FALSE.

  OPEN (20, file='rectlnr_grid.vtk', form='formatted')
  WRITE(20,"('DATASET ',(a))") u%name
  WRITE(20,"('DIMENSIONS ',*(i0,' '))") u%dimensions
  WRITE(20,"('X_COORDINATES ',i0,' ',(a))") u%dimensions(1), u%x%datatype
  WRITE(20,"(*(es13.6))") u%x%coord
  WRITE(20,"('Y_COORDINATES ',i0,' ',(a))") u%dimensions(2), u%y%datatype
  WRITE(20,"(*(es13.6))") u%y%coord
  WRITE(20,"('Z_COORDINATES ',i0,' ',(a))") u%dimensions(3), u%z%datatype
  WRITE(20,"(*(es13.6))") u%z%coord

  CLOSE(20)
  OPEN (20, file='rectlnr_grid.vtk', form='formatted', status='old')
  CALL v%rectlnr_grid_read(20)
  PRINT*, diff(u,v)," <-- should be F"
END
