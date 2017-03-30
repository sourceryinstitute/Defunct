    IMPLICIT NONE
    TYPE :: FGR_Node
    END TYPE FGR_Node

    TYPE, EXTENDS (FGR_Node) :: FRAPFGR_Node
    END TYPE
CONTAINS
    SUBROUTINE fgasre
      CLASS (FGR_Node), DIMENSION(:,:), ALLOCATABLE, SAVE :: FGR_Elements
            SELECT TYPE (FGR_Elements)
            CLASS IS (FRAPFGR_Node)
                CALL Burst_Release (FGR_Elements(1,:))
            END SELECT
    END SUBROUTINE fgasre
    SUBROUTINE Burst_Release (x)
    CLASS (FRAPFGR_Node), DIMENSION(:), INTENT(INOUT) :: x
    END SUBROUTINE Burst_Release
END 
