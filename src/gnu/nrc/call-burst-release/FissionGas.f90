MODULE FGR_Mesh
    USE Kinds, ONLY : ipk, r8k
    IMPLICIT NONE
    TYPE :: FGR_Node
    END TYPE FGR_Node
    TYPE, EXTENDS (FGR_Node) :: FRAPFGR_Node
    END TYPE
END MODULE FGR_Mesh
MODULE FissionGas
    USE Kinds
    USE FGR_Mesh, only : FRAPFGR_Node
    IMPLICIT NONE

    
CONTAINS
    
    SUBROUTINE fgasre
      USE FGR_Mesh, ONLY : FGR_Node
      CLASS (FGR_Node), DIMENSION(:,:), ALLOCATABLE, SAVE :: FGR_Elements
            SELECT TYPE (FGR_Elements)
            CLASS IS (FRAPFGR_Node)
                CALL Burst_Release (FGR_Elements(1,:))
            END SELECT
    END SUBROUTINE fgasre

    SUBROUTINE Burst_Release (x)
    CLASS (FRAPFGR_Node), DIMENSION(:), INTENT(INOUT) :: x
    END SUBROUTINE Burst_Release

END MODULE FissionGas
