MODULE Grid_Index
    USE Kinds
    PUBLIC
    
    TYPE :: Meters
        REAL(r8k) :: Distance
    END TYPE Meters
    
    ! Finite-Difference Grid
    TYPE :: FD_Grid
        INTEGER(ipk) :: ID = 0          ! Identification #
        INTEGER(ipk) :: MatID = 0      ! Material type identifier
        REAL(r8k) :: q_rel = 0.0_r8k    ! Relative Power
        LOGICAL :: IsBoundary = .FALSE. ! Boundary identifier
    CONTAINS
        PROCEDURE, PUBLIC :: Setup
    END TYPE
    
    ! 2-D x,y Grid
    TYPE, EXTENDS(FD_Grid) :: Grid_2D_Cartesian
        REAL(r8k) :: x = 0.0_r8k
        REAL(r8k) :: y = 0.0_r8k
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_Grid_xy
    END TYPE Grid_2D_Cartesian
    
    ! 3-D x,y,z Grid
    TYPE, EXTENDS(Grid_2D_Cartesian) :: Grid_3D_Cartesian
        REAL(r8k) :: z = 0.0_r8k
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_Grid_xyz
    END TYPE Grid_3D_Cartesian
    
    ! 2-D r, z cylinder
    TYPE, EXTENDS(FD_Grid) :: Grid_2D_Cylindrical
        REAL(r8k) :: r = 0.0_r8k
        REAL(r8k) :: z = 0.0_r8k
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_2D_Cylinder
    END TYPE Grid_2D_Cylindrical
    
    ! 3-D r, theta, z cylinder
    TYPE, EXTENDS(Grid_2D_Cylindrical) :: Grid_3D_Cylindrical
        REAL(r8k) :: theta = 0.0_r8k
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_3D_Cylinder
    END TYPE Grid_3D_Cylindrical
    
    CONTAINS
        
        SUBROUTINE Setup (Node, Prev_ID, MatID, Dim1, Dim2, Dim3)
        CLASS (FD_Grid), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: MatID
        INTEGER(ipk), INTENT(INOUT) :: Prev_ID
        REAL(r8k), INTENT(IN), OPTIONAL :: Dim1, Dim2, Dim3
        
        Node%ID = Prev_ID + 1
        Prev_ID = Prev_ID + 1
        Node%MatID = MatID
        
        ERROR STOP 'This is not generically defined for a mesh'
        
        END SUBROUTINE Setup
        
        SUBROUTINE Setup_Grid_xy (Node, Prev_ID, MatID, Dim1, Dim2, Dim3)
        CLASS (Grid_2D_Cartesian), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: MatID
        INTEGER(ipk), INTENT(INOUT) :: Prev_ID
        REAL(r8k), INTENT(IN), OPTIONAL :: Dim1, Dim2, Dim3
        ! Check to ensure proper variables were sent
        IF (.NOT. (PRESENT(Dim1) .AND. PRESENT(Dim2))) ERROR STOP 'Not enough inputs for Setup_Grid_xy'
        
        Node%ID = Prev_ID + 1
        Prev_ID = Prev_ID + 1
        Node%MatID = MatID
        Node%x = Dim1
        Node%y = Dim2
        
        END SUBROUTINE Setup_Grid_xy
        
        SUBROUTINE Setup_Grid_xyz (Node, Prev_ID, MatID, Dim1, Dim2, Dim3)
        CLASS (Grid_3D_Cartesian), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: MatID
        INTEGER(ipk), INTENT(INOUT) :: Prev_ID
        REAL(r8k), INTENT(IN), OPTIONAL :: Dim1, Dim2, Dim3
        ! Check to ensure proper variables were sent
        IF (.NOT. (PRESENT(Dim1) .AND. PRESENT(Dim2) .AND. PRESENT(Dim3))) ERROR STOP 'Not enough inputs for Setup_Grid_xyz'
        
        Node%ID = Prev_ID + 1
        Prev_ID = Prev_ID + 1
        Node%MatID = MatID
        Node%x = Dim1
        Node%y = Dim2
        Node%z = Dim3
        
        END SUBROUTINE Setup_Grid_xyz
        
        SUBROUTINE Setup_2D_Cylinder (Node, Prev_ID, MatID, Dim1, Dim2, Dim3)
        CLASS (Grid_2D_Cylindrical), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: MatID
        INTEGER(ipk), INTENT(INOUT) :: Prev_ID
        REAL(r8k), INTENT(IN), OPTIONAL :: Dim1, Dim2, Dim3
        
        ! Check to ensure proper variables were sent
        IF (.NOT. (PRESENT(Dim1) .AND. PRESENT(Dim2))) ERROR STOP 'Not enough inputs for Setup_2D_Cylinder'
        
        Node%ID = Prev_ID + 1
        Prev_ID = Prev_ID + 1
        Node%MatID = MatID
        Node%Z = Dim1
        Node%r = Dim2
        
!        PRINT *, Node%ID
!        PRINT *, Node%MatID
!        PRINT *, Node%q_rel
!        PRINT *, Node%z
!        PRINT *, Node%r
        
        END SUBROUTINE Setup_2D_Cylinder
        
        SUBROUTINE Setup_3D_Cylinder (Node, Prev_ID, MatID, Dim1, Dim2, Dim3)
        CLASS (Grid_3D_Cylindrical), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: MatID
        INTEGER(ipk), INTENT(INOUT) :: Prev_ID
        REAL(r8k), INTENT(IN), OPTIONAL :: Dim1, Dim2, Dim3
        
        ! Check to ensure proper variables were sent
        IF (.NOT. (PRESENT(Dim1) .AND. PRESENT(Dim2) .AND. PRESENT(Dim3))) ERROR STOP 'Not enough inputs for Setup_3D_Cylinder'
        
        Node%ID = Prev_ID + 1
        Prev_ID = Prev_ID + 1
        Node%MatID = MatID
        Node%Z = Dim1
        Node%r = Dim2
        Node%theta = Dim3
        
        END SUBROUTINE Setup_3D_Cylinder
    
END MODULE Grid_Index