MODULE Mesh
    USE Kinds
    USE Conversions
    USE FGR_Mesh, ONLY : FGR_Node, ANS54_Node, FRAPFGR_Node, Massih_Node, IFBA_Coating
    USE Variables, ONLY : nmesh
    USE CoolantVars, ONLY : Coolant_Channel
    USE Grid_Index
    USE MatLib
    IMPLICIT NONE
    !>@brief
    !> Module Mesh contains the subroutines that define the mesh used in FAST for the conduction and FGR solutions
    !> This module also contains the master list of node-dependent derived types, such as Thermal_Mesh, FGR_Elements, ZrB2
    !> and FuelRod
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/15/2016
    
    PRIVATE
    PUBLIC :: Create_Mesh, Create_Void_Mesh, Thermal_Mesh, FGR_Elements, ZrB2, Coolant_Mesh, Coolant_Mesh_0, Material_Mesh
    ! Available geometries
    ENUM, BIND (C)
        ENUMERATOR :: Cylinder, Plate, Sphere, XYZ_Mesh, ZRTheta_Mesh
    END ENUM
    ! Dimensions of numerical solution
    ENUM, BIND (C)
        ENUMERATOR :: Stacked_One_D, Two_D, Three_D
    END ENUM
    ! Number of dimensions used in solution
    INTEGER(ipk) :: Dimensions = Stacked_One_D
    ! Fuelrod geometry
    INTEGER(ipk) :: Geometry = Cylinder
    ! Number of thermal elements
    INTEGER(ipk) :: n_TH_elements = 0_ipk
    ! Number of fission gas release elements
    INTEGER(ipk) :: n_FGR_elements = 0_ipk
    
    CLASS (FD_Grid), DIMENSION(:,:), ALLOCATABLE, SAVE :: Thermal_Mesh, Thermal_Mesh_0
    CLASS (FGR_Node), DIMENSION(:,:), ALLOCATABLE, SAVE :: FGR_Elements, FGR_Elements_0
    CLASS (IFBA_Coating), DIMENSION(:), ALLOCATABLE, SAVE :: ZrB2, ZrB2_0
    CLASS (Coolant_Channel), DIMENSION(:), ALLOCATABLE, SAVE :: Coolant_Mesh, Coolant_Mesh_0
    CLASS (Material), DIMENSION(:,:), ALLOCATABLE, SAVE :: Material_Mesh, Material_Mesh_0
    
    TYPE Fuel_Element
        CLASS (Material), DIMENSION(:,:,:), ALLOCATABLE :: Material_Mesh
        CLASS (FD_Grid), DIMENSION(:,:,:), ALLOCATABLE :: Thermal_Mesh
        CLASS (FGR_Node), DIMENSION(:,:,:), ALLOCATABLE :: FGR_Elements
        CLASS (IFBA_Coating), DIMENSION(:,:), ALLOCATABLE :: ZrB2
    END TYPE Fuel_Element
    
    TYPE Cool_Element
        CLASS (Coolant_Channel), DIMENSION(:,:,:), ALLOCATABLE :: Coolant_Mesh
    END TYPE Cool_Element
    
    TYPE (Fuel_Element), DIMENSION(:), ALLOCATABLE :: FuelRod, FuelRod_0
    
    CONTAINS
    
        SUBROUTINE Create_Mesh ()
        USE Kinds
        USE Variables, ONLY : na, nr, ncmesh, dp, rc, AxNodElevat, crad, rrev, deltaz, cfv, cfva, ngasr, &
          &                   ZrB2thick, b10, zrb2den, deltaz, dco
        USE CoolantVars, ONLY : nchn
        
        ! Thermal mesh
        CALL Create_Thermal_Mesh (nr, ncmesh, na-1, dp, rc, deltaz, AxNodElevat, crad, rrev)
        ! Void mesh
!        CALL Create_Void_Mesh (nr, nt, dp, rc, crad, deltaz, cfv, cfva)
        ! FGR mesh
        CALL Create_FGR_Mesh (na, nr, ngasr, dp, rc, deltaz, ZrB2thick, b10, zrb2den)
        ! Coolant mesh
        CALL Create_Coolant_Mesh (nr, ncmesh, na, nchn, dco, deltaz, AxNodElevat)
        ! De-allocate variables no longer needed by code
        IF (ALLOCATED(ZrB2thick)) DEALLOCATE (ZrB2thick)
        
        END SUBROUTINE Create_Mesh
        
        SUBROUTINE Create_Thermal_Mesh (nr, ncmesh, nt, dp, rc, deltaz, AxNodElevat, crad, rrev)
        USE Kinds
        USE Variables, ONLY : dci, dco, imaterials
        IMPLICIT NONE
        !>@brief
        !>
        !>
        
        INTEGER(ipk) :: r, z
        INTEGER(ipk), INTENT(IN) :: nr, ncmesh, nt
        REAL(r8k), DIMENSION(:), INTENT(IN) :: dp, rc, deltaz
        REAL(r8k), DIMENSION(0:), INTENT(OUT) :: AxNodElevat
        REAL(r8k), DIMENSION(:,:), INTENT(OUT) :: crad, rrev
        
        SELECT CASE (Geometry)
        CASE (Cylinder)
            CALL Create_Cylindrical_Mesh (nr, nt, dp, rc, deltaz, AxNodElevat, crad, rrev)
            SELECT CASE (Dimensions)
            CASE (Stacked_One_D)
                ALLOCATE (Grid_2D_Cylindrical::Thermal_Mesh(1:nt,1:(nr+1+ncmesh)))
            CASE (Two_D)
                ALLOCATE (Grid_2D_Cylindrical::Thermal_Mesh(1:nt,1:(nr+1+ncmesh)))
            CASE (Three_D)
                ALLOCATE (Grid_3D_Cylindrical::Thermal_Mesh(1:nt,1:(nr+1+ncmesh)))
            END SELECT
            
            SELECT TYPE (Thermal_Mesh)
            TYPE IS (FD_Grid)
            CLASS IS (Grid_2D_Cylindrical)
                DO z = 1, nt
                    DO r = 1, nr + 1 + ncmesh
                        ASSOCIATE (Node => Thermal_Mesh(z,r))
                            ! *** NOTE ***
                            ! This is NOT complete. Much more work needed here.
                            IF ((r == 1) .OR. r == (nr + 1 + ncmesh)) Node%IsBoundary = .TRUE.
                            IF (r <= nr) THEN
                                ! Fuel material
                                CALL Node%Setup (n_TH_elements, imaterials(r,z), AxNodElevat(z), crad(r,z))
                            ELSE IF (r == nr+1) THEN
                                ! Clad material
                                CALL Node%Setup (n_TH_elements, imaterials(r,z), AxNodElevat(z), dci(z)/2.0_r8k)
                            ELSE
                                ! Clad material
                                CALL Node%Setup (n_TH_elements, imaterials(r,z), AxNodElevat(z), dco(z)/2.0_r8k)
                            END IF
                                
                        END ASSOCIATE
                    END DO
                END DO
            CLASS IS (Grid_3D_Cylindrical)
                
            CLASS DEFAULT
                ERROR STOP
            END SELECT
        CASE (Plate)
            
        CASE (Sphere)
            
        CASE (XYZ_Mesh)
            ERROR STOP 'Mesh not yet defined'
        CASE DEFAULT
            ERROR STOP 'Mesh not yet defined'
        END SELECT
        
        END SUBROUTINE Create_Thermal_Mesh
        
        
        SUBROUTINE Create_Cylindrical_Mesh (nr, nt, dp, rc, deltaz, AxNodElevat, crad, rrev)
        USE Kinds
        IMPLICIT NONE
        !>@brief
        !> Subroutine Create_Thermal_Mesh creates the mesh used in the thermal solution
        !> using cartesian coordiantes (Assumes axi-symmetric)
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/15/2016
        !
        ! Mesh layout:
        ! ----------------------------------------                                 -------- Top of rod
        ! :              :           :           :                                     :
        ! :   Region 1   :  Region 2 :  Region n :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                 Axial Node
        ! .  .  .  .  .  . . . . . . .   .   .   . <-- Mesh Points                    NT
        ! :              :           :           :                                     :
        ! 0  1  2  3  4  5 6 7 8 9               NR <-- Mesh point numbering           :
        ! :              :           :           :                                     :
        ! ----------------------------------------                                 --------
        ! :              :           :           :                                     :
        ! :   Region 1   :  Region 2 :  Region n :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                 Axial Node
        ! .  .  .  .  .  . . . . . . .   .   .   . <-- Mesh Points                     2
        ! :              :           :           :                                     :
        ! 0  1  2  3  4  5 6 7 8 9               NR <-- Mesh point numbering           :
        ! :              :           :           :                                     :
        ! ----------------------------------------                                 --------
        ! :              :           :           :                                     :
        ! :   Region 1   :  Region 2 :  Region n :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                 Axial Node
        ! .  .  .  .  .  . . . . . . .   .   .   . <-- Mesh Points                     1
        ! :              :           :           :                                     :
        ! 0  1  2  3  4  5 6 7 8 9               NR <-- Mesh point numbering           :
        ! :              :           :           :                                     :
        ! ----------------------------------------                                 -------- Bottom of rod
        ! crad(0) -->                       crad(nr) <-- Distance
        !
        ! Where:
        ! 0, NR = Radial Boundaries
        ! crad  = Space variable (radial distance from centerline)
        ! 0, NT = Axial Boundaries
        !
        ! Input
        !
        ! nr - # of radial nodes
        ! nt - # of axial nodes
        ! dp(AxNode)  - Diameter of pellet at axial node AxNode
        ! rc(AxNode)  - Radius of central hole in pellet at axial node AxNode
        !
        ! Output
        !
        ! crad(RadNode, AxNode) - Radial locations used for thermal solution (in)
        ! rrev(RadNode, AxNode) - Radial locations used for burnup. Uses reversed nodalization relative to crad (in)
        ! AxNodElevat(AxNode)   - Axial node midpoint elevation (ft)
        !
        INTEGER(ipk) :: AxNode, RadNode
        INTEGER(ipk), INTENT(IN) :: nr, nt
        REAL(r8k), DIMENSION(:), INTENT(IN) :: dp, rc, deltaz
        REAL(r8k), DIMENSION(0:), INTENT(OUT) :: AxNodElevat
        REAL(r8k), DIMENSION(:,:), INTENT(OUT) :: crad, rrev
        ! Calculation of the radial node placement in the fuel region. This function will place more nodes
        ! near the surface of the pellet to account for rim effects. node 1 is on the fuel surface.
        
        IF (nt < 1) ERROR STOP 'Bad value for na. na < 1. Execution terminated in Subroutine: Create_Cylindrical_Mesh.'
        AxNodElevat(0) = 0.0_r8k
        
        DO AxNode = 1, nt
            
            ! Axial node midpoint elevations (ft)
            IF (AxNode == 1) THEN
                AxNodElevat(AxNode) = deltaz(AxNode) / 2.0_r8k
            ELSE
                AxNodElevat(AxNode) = AxNodElevat(AxNode-1) + (deltaz(AxNode-1) + deltaz(AxNode)) / 2.0_r8k
            END IF
            
            ! Radial distance (in)
            DO RadNode = 1, nr
                crad(RadNode,AxNode) = (1.0_r8k - (REAL(RadNode-1) / REAL(nr-1)) ** 3) * &
                  &                    (dp(AxNode) / 2.0_r8k - rc(AxNode)) + rc(AxNode)
                ! Subroutines fueltp and tubrnp use a reversed nodalization
                rrev(nr-(RadNode-1),AxNode) = crad(RadNode,AxNode)
            END DO
        END DO
        
        ! Include plenum region
        AxNodElevat(nt+1) = AxNodElevat(nt) + (deltaz(nt) + deltaz(nt+1)) / 2.0_r8k
        
        !ALLOCATE (FuelRod(1:nt+1,1:nmesh))
        ! Define the material indexes
        CALL Set_Materials! (FuelRod)
    
        END SUBROUTINE Create_Cylindrical_Mesh
        !
        !
        !
        SUBROUTINE Create_Void_Mesh (nr, nt, dp, rc, crad, deltaz, cfv, cfva)
        USE Kinds
        USE Conversions, ONLY : fttoin
        USE Variables, ONLY : hdish, chmfrh, totl, ringvol, coldringl, dishsd, rdish, chmfrw, vplt, hplt
        IMPLICIT NONE
        !>@brief
        !> Subroutine Create_Void_Mesh creates the mesh used in calculating the void volume and temperatures
        !> using cartesian coordiantes (Assumes axi-symmetric)
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/15/2016
        !
        ! Mesh layout:
        ! ----------------------------------------                                 --------
        ! :DDDD:         :           :         :C:                                     :
        ! :DDD:          :           :          ::                                     :
        ! ::::           :           :           :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! :   Region 1   :  Region 2 :  Region n :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! .  .  .  .  .  . . . . . . .   .   .   . <-- Mesh Points                 Axial Node
        ! :              :           :           :                                     :
        ! 0  1  2  3  4  5 6 7 8 9               NR <-- Mesh point numbering           :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! :              :           :           :                                     :
        ! ::::           :           :           :                                     :
        ! :DDD:          :           :          ::                                     :
        ! :DDDD:         :           :         :C:                                     :
        ! ----------------------------------------                                 -------- Bottom of rod
        ! crad(0) -->                       crad(nr) <-- Distance
        !
        ! Where:
        ! 0, NR = Radial Boundaries
        ! crad  = Space variable (radial distance from centerline)
        ! 0, NT = Axial Boundaries
        !
        ! Region 1 = Fuel
        ! Region 2 = Gas
        ! Region 3 = Clad
        ! Region D = Dish Volume (Assumed filled with Region 2 - Gas)
        ! Region C = Chamfer Volume (Assumed filled with Region 2 - Gas)
        !
        ! Input
        !
        ! nr - # of radial nodes
        ! nt - # of axial nodes
        ! dp(AxNode)  - Diameter of pellet at axial node AxNode
        ! rc(AxNode)  - Radius of central hole in pellet at axial node AxNode
        !
        ! Output
        !
        ! crad(RadNode, AxNode) - Radial locations used for thermal solution.
        !
        INTEGER(ipk) :: AxNode, RadNode
        INTEGER(ipk), INTENT(IN) :: nr, nt
        REAL(r8k) :: dishsg, rchmfr, hi
        REAL(r8k), INTENT(OUT) :: cfv, cfva
        REAL(r8k), DIMENSION(:), INTENT(IN) :: dp, rc, deltaz
        REAL(r8k), DIMENSION(:,:), INTENT(IN) :: crad
        
        ! cfva - total cold fuel volume assuming cylindrical pellets
        cfva = 0.0_r8k
        DO AxNode = 1, nt
            cfva = cfva + pi * dp(AxNode) ** 2 * deltaz(AxNode) * 3.0_r8k
        END DO
        cfv = 0.0_r8k
        ! Determine the cold free volume (cfv)
        DO AxNode = 1, nt
            IF (hdish <= 0.0_r8k .AND. chmfrh <= 0.0_r8k) THEN !No dish or chamfers are modeled
                DO RadNode = 1, nr-1
                    ! Assume circles (Volume = Pi * (r2^2 - r1^2) * Height
                    ringvol(RadNode,AxNode) = Pi * (crad(RadNode,AxNode) ** 2 - crad(RadNode+1,AxNode) ** 2) * &
                      &                       deltaz(AxNode) * fttoin
                    ! Axial length of ring at STP
                    coldringl(RadNode,AxNode) = deltaz(AxNode) * fttoin
                END DO
                ! Subtract out central hole (rc)
                cfv = cfva - pi * rc(AxNode) ** 2 * totl * fttoin
            ELSE
                ! dish radius calculation
                dishsg = dp(AxNode) / 2.0_r8k - dishsd
                ! dish radius of curvature
                IF (hdish <= 0.0_r8k) THEN
                    rdish = 0.0_r8k
                ELSE
                    rdish = (hdish ** 2 + dishsg ** 2) / (2.0_r8k * hdish)
                END IF
                ! chamfer inner radius
                rchmfr = dp(AxNode) / 2.0_r8k - chmfrw
                ! volume of pellet annulus
                vplt = pi * rc(AxNode) ** 2 * deltaz(AxNode) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
                ! radial pellet ring height calculation
                DO RadNode = nr-1, 1, -1
                    IF (rdish < crad(RadNode,AxNode)) THEN
                        hi = hdish
                    ELSE
                        hi = rdish - MAX(SQRT(rdish ** 2 - crad(RadNode,AxNode) ** 2), (rdish - hdish))
                    END IF
                    ringvol(RadNode,AxNode) = (deltaz(AxNode) / hplt) * pi * (crad(RadNode,AxNode) ** 2 * &
                      &                       (hplt + 2.0_r8k * (hi - hdish)) - 2.0_r8k * hi ** 2 * &
                      &                       (rdish - hi / 3.0_r8k)) * fttoin - vplt
                    IF (chmfrh > 0.0_r8k .AND. chmfrw > 0.0_r8k) THEN
                        IF (crad(RadNode,AxNode) > rchmfr) ringvol(RadNode,AxNode) = &
                          & (deltaz(AxNode) / hplt) * (pi * (crad(RadNode,AxNode) ** 2 - crad(RadNode+1,AxNode) ** 2) * hplt - &
                          &  2.0_r8k * pi * crad(RadNode,AxNode) ** 2 * chmfrh * (crad(RadNode,AxNode) - rchmfr) / chmfrw + &
                          &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(RadNode,AxNode) - rchmfr) / chmfrw * &
                          &  (crad(RadNode,AxNode) ** 2 + (rchmfr) ** 2 + crad(RadNode,AxNode) * (rchmfr))) * fttoin
                        IF (crad(RadNode+1,AxNode) > rchmfr) ringvol(RadNode,AxNode) = &
                          & (deltaz(AxNode) / hplt) * (pi * (crad(RadNode,AxNode) ** 2 - crad(RadNode+1,AxNode) ** 2) * hplt - &
                          &  2.0_r8k * pi * crad(RadNode,AxNode) ** 2 * chmfrh * (crad(RadNode,AxNode) - rchmfr) / chmfrw + &
                          &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(RadNode,AxNode) - rchmfr) / chmfrw * &
                          &  (crad(RadNode,AxNode) ** 2 + (rchmfr) ** 2 + crad(RadNode,AxNode) * (rchmfr)) + &
                          &  2.0_r8k * pi * crad(RadNode+1,AxNode) ** 2 * chmfrh * (crad(RadNode+1,AxNode) - rchmfr) / chmfrw - &
                          &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(RadNode+1,AxNode) - rchmfr) / chmfrw * &
                          &  (crad(RadNode+1,AxNode) ** 2 + (rchmfr) ** 2 + crad(RadNode+1,AxNode) * (rchmfr))) * fttoin
                    END IF
                    coldringl(RadNode,AxNode) = ringvol(RadNode,AxNode) / &
                      &                       ((crad(RadNode,AxNode) ** 2 - crad(RadNode+1,AxNode) ** 2) * pi)
                    vplt = vplt + ringvol(RadNode,AxNode)
                END DO
                cfv = cfv + (vplt - pi * rc(AxNode) ** 2 * deltaz(AxNode) * fttoin / hplt * (hplt - 2.0_r8k * hdish))
                IF (rc(AxNode) > 0.0_r8k) coldringl(nr-1,AxNode) = deltaz(AxNode) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
            END IF
        END DO
        
        END SUBROUTINE Create_Void_Mesh
        !
        !
        !
        SUBROUTINE Create_FGR_Mesh (na, nr, ngasr, dp, rc, deltaz, ZrB2thick, b10, zrb2den)
        USE Kinds
        USE Functions, ONLY : terp
        USE Variables, ONLY : ngasmod, ANS54_1982, Massih, FRAPFGR, ANS54_2011, crad, rapow
        IMPLICIT NONE
        !>@brief
        !> Subroutine Create_FGR_Mesh creates the mesh used in the FGR solution
        !> Each FGR solution has a different nodalization. However, each way assumes equal radial areas
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 11/15/2016
        !
        ! Mesh layout:
        ! ----------------------------                           --------
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :       Fuel Region        :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! .  .  .  .  .  . . . . . . . <-- Mesh Points           Axial Node
        ! :                          :                               :
        ! 0  1  2  3  4  5 6 7 8 9  NGASR <-- Mesh point numbering   :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! :                          :                               :
        ! ----------------------------                           -------- Bottom of rod
        ! ansr(0) -->           ansr(nr) <-- Distance
        !
        ! Direction of diffusion:
        ! --> --> --> --> --> --> -->
        !
        ! Where:
        ! 0, NR = Radial Boundaries
        ! crad  = Space variable (radial distance from centerline)
        ! 0, NT = Axial Boundaries
        !
        ! Region 1 = Fuel
        ! Region 2 = Gas
        ! Region 3 = Clad
        ! Region D = Dish Volume (Assumed filled with Region 2 - Gas)
        ! Region C = Chamfer Volume (Assumed filled with Region 2 - Gas)
        !
        ! Input
        !
        ! AxNode - Current axial node being modeled
        ! dp     - Diameter of pellet
        ! ngasr  - # of radial rings in FGR solution
        ! nr     - # of radial rings in thermal solution
        ! rc     - Radius of central hole in pellet
        !
        ! Output
        !
        ! ansr(RadNode, AxNode)  - Radial location
        ! flxfc(RadNode, AxNode) - Flux factor
        !
!        CLASS (FGR_Node), ALLOCATABLE :: Node, Node_Last
        
        INTEGER(ipk) :: z, r_thermal, r_fgr
        INTEGER(ipk), INTENT(IN) :: na, nr, ngasr
        REAL(r8k) :: dvoid, ansda, routr, routr2
        REAL(r8k), INTENT(IN) :: b10, zrb2den
        REAL(r8k), DIMENSION(:), INTENT(IN) :: dp, rc, deltaz
        REAL(r8k), DIMENSION(:), INTENT(INOUT) :: ZrB2thick
        !REAL(r8k), DIMENSION(nr) :: rv, dv
        !REAL(r8k), DIMENSION(:) :: ansr
        
        SELECT CASE (ngasmod)
        CASE (ANS54_1982, ANS54_2011)
            ALLOCATE (ANS54_Node::FGR_Elements(1:na,1:ngasr+1))
        CASE (FRAPFGR)
            ALLOCATE (FRAPFGR_Node::FGR_Elements(1:na,1:ngasr+1))
        CASE (Massih)
            ALLOCATE (Massih_Node::FGR_Elements(1:na,1:ngasr+1))
        CASE DEFAULT
        
            ERROR STOP 'Mesh is not defined for FGR model chosen. Execution terminated in Subroutine: Create_FGR_Mesh'
        END SELECT
        
        SELECT TYPE (Thermal_Mesh)
        TYPE IS (FD_Grid)
        CLASS IS (Grid_2D_Cylindrical)
            
            
            
            
            
!            DO z = 1, na-1
!                DO r = 1, nr + 1 + ncmesh
!                    Thermal_Mesh(z,r)%Mesh => 
!                    Node_Last => Thermal_Mesh(z,r)%Mesh
!                    CALL Node%Setup (Node_Last%ID)
!                    Node_Last => Node
!                END DO
!            END DO
            
            ! Assumes equal area rings
            ! all the radial regions formed here have equal areas, not equal widths.
            ! ansr = radius term
            ! ansdia = diameter term
            ! ansda = area term
            ! routr = radius term
            ! routr2 = area term
            ! ring volume calculation (equal-volume rings)
            DO z = 1, na-1
                ! calculate flux depression array
                ! Use rapow from Subroutine radar
!                DO r_thermal = 1, nr
                    ! Cold node diameter
                    ! NOTE: dv was replaced with %r
!                    Thermal_Mesh(z,r_thermal)%Mesh%r = crad(r_thermal,z) * 2.0_r8k
                    !dv(r_thermal) = crad(r_thermal,z) * 2.0_r8k
                    
                    ! Radial power fraction
                    ! NOTE: rv was replaced with q_rel
                    !Thermal_Mesh(z,r_thermal)%Mesh%q_rel = rapow(r_thermal,z)
!                    rv(r_thermal) = rapow(r_thermal,z)
!                END DO
                dvoid = 2.0_r8k * rc(z)
                ! Area of each radial ring within axial segment AxNode
                ansda = ((dp(z) ** 2 - dvoid ** 2) / 4.0_r8k) / ngasr
                ! Radius of fuel pellet
                routr = dp(z) / 2.0_r8k
                DO r_fgr = 1, ngasr
                    ASSOCIATE (Node => FGR_Elements(z,r_fgr))
                        !FGR_Elements(z,r_fgr)%Mesh => Node
                    
                        routr2 = routr ** 2
                        !ansr(r_fgr) = SQRT(routr2 - ansda / 2.0_r8k)
                        Node%ansr = SQRT(routr2 - ansda / 2.0_r8k)
                        IF (r_fgr < ngasr) routr = SQRT(routr2 - ansda)
                    
                        !Node_Last => FGR_Elements(z,r_fgr)%Mesh
                        !CALL Node%Setup (Node_Last%ID)
                        CALL Node%Setup ((z-1)*ngasr+r_fgr)
                        !Node_Last => Node
                        n_FGR_elements = node%ID
                    END ASSOCIATE
                END DO
                
                ! IFBA coating
                IF (.NOT. ALLOCATED(ZrB2)) ALLOCATE(ZrB2(1:na-1))
                
                CALL ZrB2(z)%Setup(dp(z), deltaz(z), ZrB2thick(z), b10, zrb2den)
                
            END DO
            
            
            
        CLASS IS (Grid_3D_Cylindrical)
                
        CLASS DEFAULT
            ERROR STOP
        END SELECT
        
        END SUBROUTINE Create_FGR_Mesh
        !
        !
        !
        SUBROUTINE Set_Materials
        USE Kinds
        USE Variables, ONLY : na, nr, ngasr, nfmesh, ngmesh, ncmesh, noxide, ncrud, nmesh, imaterials, rc, gadoln, comp, &
          &                   imox, icm, ioxm, cwkf, cwnf, fnck, fncn, coldwk, catexf, chorg, deloxy, roughc, roughf
        
        !>@brief
        !> This subroutine sets the material ID for each mesh
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 6/7/2016
        !
        ! ---------------------------------------------                              --------
        ! :                         :   : :       : : :                                 :
        ! :          Fuel           :Gas:O:  Clad :O:C:                                 :
        ! :                         :   :x:       :x:r:                                 :
        ! :                         :   :i:       :i:u:                             Axial Node
        ! .  .  .  .  .   . . . . . .   .d.   .   .d.d:  <-- Mesh Points              na-1
        ! :                         :   :e:       :e: :                                 :
        ! 0  1  2  3  4  5 6 7 8 9  NR  : :       : : :<-- Mesh point numbering         :
        ! :                         :   : :       : : :                                 :
        ! ---------------------------------------------                              --------
        ! :                         :   : :       : : :                                 :
        ! :          Fuel           :Gas:O:  Clad :O:C:                                 :
        ! :                         :   :x:       :x:r:                                 :
        ! :                         :   :i:       :i:u:                             Axial Node
        ! .  .  .  .  .   . . . . . .   .d.   .   .d.d:  <-- Mesh Points                2
        ! :                         :   :e:       :e: :                                 :
        ! 0  1  2  3  4  5 6 7 8 9  NR  : :       : : :<-- Mesh point numbering         :
        ! :                         :   : :       : : :                                 :
        ! ---------------------------------------------                              --------
        ! :                         :   : :       : : :                                 :
        ! :          Fuel           :Gas:O:  Clad :O:C:                                 :
        ! :                         :   :x:       :x:r:                                 :
        ! :                         :   :i:       :i:u:                             Axial Node
        ! .  .  .  .  .   . . . . . .   .d.   .   .d.d:  <-- Mesh Points                1
        ! :                         :   :e:       :e: :                                 :
        ! 0  1  2  3  4  5 6 7 8 9  NR  : :       : : :<-- Mesh point numbering         :
        ! :                         :   : :       : : :                                 :
        ! -------------------------------------------                              -------- Bottom of rod
        !
        ! Where:
        ! 
        ! Fuel  - imox (user-input. 3 built-in options)
        ! Gas   - idxgas (user-input. 7 built-in options)
        ! Oxide - ioxm, (user-input. 2 built-in options)
        ! Clad  - icm (user-input. 5 built-in options)
        ! Crud  - Hardwired Variable (only 1 built-in option)
        ! 
        CLASS(Material), ALLOCATABLE :: Mat
        INTEGER(ipk) :: RadNode, AxNode, Clad_Start, Oxide_Start, Crud_Start, mat_id
        LOGICAL :: IsEdge
        
        ENUM, BIND(C)
            ENUMERATOR :: Fuel=1, Clad, Gap, Oxide, Crud
        END ENUM
        
        Clad_Start = nfmesh + ngmesh
        Oxide_Start = Clad_Start + ncmesh
        Crud_Start = Oxide_Start + noxide
        
        DO AxNode = 1, na-1
            DO RadNode = 1, (nmesh-1)
                ASSOCIATE (Mat => Material_Mesh(AxNode,RadNode))
                    IF (RadNode == 1 .AND. rc(AxNode) > 0.0_r8k) THEN
                        ! Central void
                        imaterials(RadNode,AxNode) = Gap
                    ELSE IF (RadNode < nfmesh) THEN
                        ! Fuel region
                        imaterials(RadNode,AxNode) = Fuel
                    ELSE IF (RadNode == nfmesh) THEN
                        ! Gap region
                        imaterials(RadNode,AxNode) = Gap
                    ELSE IF (RadNode > nfmesh .AND. RadNode < Oxide_Start) THEN
                        ! Clad region
                        imaterials(RadNode,AxNode) = Clad
                    ELSE IF (RadNode >= Oxide_Start .AND. RadNode < Crud_Start) THEN
                        ! Oxide region
                        imaterials(RadNode,AxNode) = Oxide
                        ! Currently forced to Clad
                        imaterials(RadNode,AxNode) = Clad
                    ELSE IF (RadNode >= Crud_Start .AND. RadNode <= nmesh) THEN
                        ! Crud region
                        imaterials(RadNode,AxNode) = Crud
                        ! Currently forced to Clad
                        imaterials(RadNode,AxNode) = Clad
                    ELSE
                        ! Material not defined
                        ERROR STOP 'Material not defined. Execution terminated in Subroutine: Set_Materials'
                    END IF
                END ASSOCIATE
                ! Set the default values from the input file
    !            CALL Mat%Define(AxNode)
            END DO
        END DO
        mat_id = 0_ipk
        IF (.NOT. ALLOCATED(Material_Element)) ALLOCATE (Material_Element(1:(na*nmesh)))
        DO AxNode = 1, na-1
            DO RadNode = 1, (nmesh-1)
                    mat_id = mat_id + 1
                    ! Define the material edges
                    IsEdge = (RadNode == 1 .OR. RadNode == nfmesh .OR. RadNode == nfmesh+1 .OR. RadNode == nmesh .OR. &
                      &       RadNOde == Oxide_Start .OR. RadNode == Crud_Start)
                    IF (RadNode == 1 .AND. rc(AxNode) > 0.0_r8k) THEN
                        ! Central void
                        ALLOCATE (GasMix::Material_Element(mat_id)%Mat)
                        CALL Set_Initial_Values (mat_id, Edge=IsEdge)
                    ELSE IF (RadNode < nfmesh) THEN
                        ! Fuel region
                        SELECT CASE (imox)
                        CASE (0)
                            ! UO2
                            ALLOCATE (UO2::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id, Edge=IsEdge, Gad=gadoln(AxNode), Roughness=roughf)
                        CASE (1)
                            ! MOX
                            ALLOCATE (MOX_NFI::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id, Edge=IsEdge, Gad=gadoln(AxNode), comp=comp(AxNode), Roughness=roughf)
                        CASE (2)
                            ! MOX
                            ALLOCATE (MOX_Halden::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id, Edge=IsEdge, Gad=gadoln(AxNode), comp=comp(AxNode), Roughness=roughf)
                        CASE (3)
                            ! U3Si2
                            ALLOCATE (U3Si2::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id, Edge=IsEdge, Gad=gadoln(AxNode), Roughness=roughf)
                        END SELECT
                    ELSE IF (RadNode == nfmesh) THEN
                        ! Gap region
                        ALLOCATE (GasMix::Material_Element(mat_id)%Mat)
                        CALL Set_Initial_Values (mat_id, Edge=IsEdge)
                    ELSE IF (RadNode > nfmesh .AND. RadNode < Oxide_Start) THEN
                        ! Clad region
                        SELECT CASE (icm)
                        CASE (2)
                            ALLOCATE (Zirc2::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (4)
                            ALLOCATE (Zirc4::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (5)
                            ALLOCATE (M5::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (6)
                            ALLOCATE (ZIRLO::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (7)
                            ALLOCATE (OptZIRLO::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (9)
                            ALLOCATE (Zr1Nb::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (10)
                            ALLOCATE (E110::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (11)
                            ALLOCATE (SiC::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE (15)
                            ALLOCATE (FeCrAl::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, cwkf=cwkf, cwnf=cwnf, fnck=fnck, fncn=fncn, &
                              &                      coldwk=coldwk, catexf=catexf, chorg=chorg, Oxygen=deloxy, Roughness=roughc)
                        CASE DEFAULT
                            ERROR STOP 'Cladding material not defined. Execution terminated in Subroutine: Set_Materials'
                        END SELECT
                        
                    ELSE IF (RadNode >= Oxide_Start .AND. RadNode < Crud_Start) THEN
                        ! Oxide region
                        !imaterials(RadNode,AxNode) = Oxide
                        SELECT CASE (ioxm)
                        CASE (0)
                            ! ZrO2
                            ALLOCATE (ZrO2::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, Roughness=roughc)
                        CASE (1)
                            ! SiO2
                            ALLOCATE (SiO2::Material_Element(mat_id)%Mat)
                            CALL Set_Initial_Values (mat_id=mat_id, Edge=IsEdge, Roughness=roughc)
                        END SELECT
                        
                        ! Currently forced to Clad
                        !imaterials(RadNode,AxNode) = Clad
                    ELSE IF (RadNode >= Crud_Start .AND. RadNode <= nmesh) THEN
                        ! Crud region
                        !imaterials(RadNode,AxNode) = Crud
                        ! Currently forced to Clad
                        !imaterials(RadNode,AxNode) = Clad
                    ELSE
                        ! Material not defined
                        ERROR STOP 'Material not defined. Execution terminated in Subroutine: Set_Materials'
                    END IF
                ! Set the default values from the input file
    !            CALL Mat%Define(AxNode)
            END DO
        END DO
        
        END SUBROUTINE Set_Materials
        
        
        SUBROUTINE Create_Coolant_Mesh (nr, ncmesh, na, nchn, dco, deltaz, AxNodElevat)
        USE Kinds
        IMPLICIT NONE
        !>@brief
        !>
        !>
        
        INTEGER(ipk) :: z
        INTEGER(ipk), INTENT(IN) :: nr, ncmesh, na, nchn
        REAL(r8k), DIMENSION(:), INTENT(IN) :: dco, deltaz
        REAL(r8k), DIMENSION(0:), INTENT(IN) :: AxNodElevat
        
        SELECT CASE (Geometry)
        CASE (Cylinder)
            SELECT CASE (Dimensions)
            CASE (Stacked_One_D)
                ALLOCATE (Coolant_Mesh(0:na), Coolant_Mesh_0(0:na))
            CASE (Two_D)
                ALLOCATE (Coolant_Mesh(0:na), Coolant_Mesh_0(0:na))
            CASE (Three_D)
                ALLOCATE (Coolant_Mesh(0:(na*nchn)), Coolant_Mesh_0(0:(na*nchn)))
            END SELECT
            
            SELECT TYPE (Thermal_Mesh)
            TYPE IS (FD_Grid)
            CLASS IS (Grid_2D_Cylindrical)
                DO z = LBOUND(Coolant_Mesh,DIM=1), UBOUND(Coolant_Mesh,DIM=1)
                    ASSOCIATE (Node => Coolant_Mesh(z))
                        ! Set the mid-point elevation (m)
                        !Node%Elevation = (AxNodElevat(z-1) + AxNodElevat(z)) / 2.0_r8k
                        Node%Elevation = AxNodElevat(z)
                        CALL Node%Update_Geometry (z)
                    END ASSOCIATE
                END DO
            CLASS IS (Grid_3D_Cylindrical)
                
            CLASS DEFAULT
                ERROR STOP 'Mesh is not yet defined'
            END SELECT
        CASE (Plate)
            
        CASE (Sphere)
            
        CASE (XYZ_Mesh)
            ERROR STOP 'Mesh not yet defined'
        CASE DEFAULT
            ERROR STOP 'Mesh not yet defined'
        END SELECT
        
        END SUBROUTINE Create_Coolant_Mesh
        !
END MODULE Mesh