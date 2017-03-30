MODULE FissionGas
    USE Kinds
    USE FGR_Mesh, only : FRAPFGR_Node
    IMPLICIT NONE
    
CONTAINS
    
    SUBROUTINE fgasre
    USE Mesh, ONLY : FGR_Elements
            SELECT TYPE (FGR_Elements)
            CLASS IS (FRAPFGR_Node)
                CALL Burst_Release (FGR_Elements(1,:))
            END SELECT
    END SUBROUTINE fgasre

    SUBROUTINE Burst_Release (x)
    CLASS (FRAPFGR_Node), DIMENSION(:), INTENT(INOUT) :: x
    END SUBROUTINE Burst_Release

END MODULE FissionGas
