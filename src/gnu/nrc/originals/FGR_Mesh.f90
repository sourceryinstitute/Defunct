MODULE FGR_Mesh
    USE Kinds, ONLY : ipk, r8k
    IMPLICIT NONE
    !>@brief
    !> Contains the types used for FGR analysis.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/3/2017
    
    PRIVATE
    PUBLIC :: FGR_Node, FRAPFGR_Node, Massih_Node, ANS54_Node, scangas, IFBA_Coating
    
    TYPE OldNew
        ! Old value
        REAL(r8k) :: Old = 0.0_r8k
        ! New value
        REAL(r8k) :: New = 0.0_r8k
    END TYPE OldNew
    
    TYPE GrainType
        ! Standard Grains
        TYPE(OldNew) :: StdGrain
        ! Restructured Grains
        TYPE(OldNew) :: RestrGrain
        ! Sum of both standard and restructured grains
        TYPE(OldNew) :: Net
    END TYPE GrainType
    
    TYPE :: FGR_Node
        ! Node Identification #
        INTEGER(ipk) :: ID = 0
        ! Radial distance from center of rod ()
        REAL(r8k) :: ansr = 0.0_r8k
        ! Concentration of gas remaining in fuel matrix (produced - released)
        REAL(r8k) :: conGas = 0.0_r8k
        ! Radial location of gas used for plotting with scanair information
        REAL(r8k) :: rdGas = 0.0_r8k
        ! Ring specific burnup (MWd/mtU)
        REAL(r8k) :: brnrefab = 0.0_r8k
        ! Burst release occurred (1 = Grain boundary; 2 = Restructured grains; 3 = Unrestructed grains)
        LOGICAL, DIMENSION(3) :: BurstRelease = .FALSE.
        ! Burnup - Nodal burnup
        ! gp     - Concentration of gas produced
        ! rls    - Concentration of gas released
        ! grn    - Grain size (radius) in meters
        ! Temp   - Nodal temperature (K)
        TYPE(OldNew) :: Burnup, gp, rls, grn, Temp
        ! gb  - Concentration of gas at grain boundaries
        ! grs - Concentration of gas re-solved back into the matrix
        ! gg  - Concentration of gas within grains
        TYPE(GrainType) :: gb, grs, gg
        ! g   - Gas concentration in fuel grain for each term of the 4-term approximation to the integration kernel
        TYPE(GrainType), DIMENSION(4) :: g
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_FGR_Node
        PROCEDURE, PUBLIC :: Store => Save_Converged_Vals
        PROCEDURE, PUBLIC :: Write => Write_to_file
        PROCEDURE, PUBLIC :: Read => Read_from_file
    END TYPE FGR_Node
    
    TYPE, EXTENDS (FGR_Node) :: FRAPFGR_Node
        ! Open porosity flag
        LOGICAL, DIMENSION(2) :: openp = .FALSE.
        ! HBU rim thickness (in)
        REAL(r8k) :: rimthick = 0.0_r8k
        ! Fraction of node that contains restructured grains (0 to 1)
        REAL(r8k) :: restructure = 0.0_r8k
        ! Fuel matrix porosity
        REAL(r8k) :: porosity = 0.0_r8k
    END TYPE
    ! Type used for Massih FGR calculations
    TYPE, EXTENDS (FGR_Node) :: Massih_Node
        ! No additional parameters are currently needed for the Massih correlation
    END TYPE
    
    ! Type used for ANS 5.4 FGR calculations
    TYPE, EXTENDS (FGR_Node) :: ANS54_Node
        ! Weighting factor for the radioactive gas release fraction to compensate for diff. production rates in diff. regions
        REAL(r8k) :: pf = 0.0_r8k
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: decay
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: half
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ansd
    CONTAINS
        PROCEDURE, PUBLIC :: pf_norm
    END TYPE ANS54_Node
    
    ! IFBA Variables
    ! Theoretical density of ZrB2 (g/cm^3)
    REAL(r8k), PARAMETER :: ZrB2_TDen = 6.08_r8k
    TYPE atoms
        ! # of atoms per unit volume (cm3) of material
        REAL(r8k) :: B, B10, B11
    END TYPE
    
    TYPE IFBA_Coating
        PRIVATE
        ! Theoretical density of ZrB2 coating (%)
        REAL(r8k) :: TDen = 90.0_r8k
        ! Volume of ZrB2 coating (cm3)
        REAL(r8k) :: Volume = 0.0_r8k
        ! Thickness (in)
        REAL(r8k), PUBLIC :: thickness
        ! Incrimental Helium produced (atoms He/cm^3)
        REAL(r8k) :: HeProd = 0.0_r8k
        ! Boron-10 concentration
        REAL(r8k), PUBLIC :: Boron10 = 0.0_r8k
        ! Moles of Helium gas released to the rod free volume
        REAL(r8k), PUBLIC :: IFBArel = 0.0_r8k
        ! # of atoms per unit volume (cm3) of material
        TYPE(atoms) :: atom
        ! Total Helium produced (atoms He/cm^3)
        TYPE (OldNew) :: He
    CONTAINS
        PROCEDURE, PUBLIC :: Setup => Setup_IFBA
        PROCEDURE :: Calc_He_Prod => Helium_Production
        PROCEDURE, PUBLIC :: Read => Read_IFBA
        PROCEDURE, PUBLIC :: Write => Write_IFBA
        PROCEDURE :: SaveIFBArel => Convert_IFBA_Release
    END TYPE IFBA_Coating
    
    CONTAINS
    
        SUBROUTINE Setup_FGR_Node (Node, Prev_ID)
        
        CLASS (FGR_Node), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: Prev_ID
        
        Node%ID = Prev_ID + 1
        
        END SUBROUTINE Setup_FGR_Node
        
        SUBROUTINE Save_Converged_Vals (Node, grnfuel, Set_Release_to_zero, Temperature)
        
        CLASS (FGR_Node), INTENT(INOUT) :: Node
        
        INTEGER(ipk) :: i
        REAL(r8k), INTENT(IN) :: grnfuel, Temperature
        LOGICAL, INTENT(IN) :: Set_Release_to_zero
        
        Node%gp%Old = Node%gp%New
        Node%gb%StdGrain%Old = Node%gb%StdGrain%New
        Node%gb%RestrGrain%Old = Node%gb%RestrGrain%New
        Node%gg%StdGrain%Old = Node%gg%StdGrain%New
        Node%gg%RestrGrain%Old = Node%gg%RestrGrain%New
        Node%grs%StdGrain%Old = Node%grs%StdGrain%New
        Node%grs%RestrGrain%Old = Node%grs%RestrGrain%New
        IF (Set_Release_to_zero) THEN
            Node%rls%Old = 0.0_r8k
            Node%brnrefab = Node%burnup%New
        ELSE
            Node%rls%Old = Node%rls%New
        END IF
        Node%g%StdGrain%Old = Node%g%StdGrain%New
        Node%g%RestrGrain%Old = Node%g%RestrGrain%New
        
        Node%burnup%old = Node%burnup%new
        Node%grn%old = grnfuel
        Node%Temp%Old = Temperature
        DO i = LBOUND(Node%g,1), UBOUND(Node%g,1)
            Node%g(i)%StdGrain%old = Node%g(i)%StdGrain%New
            Node%g(i)%RestrGrain%old = Node%g(i)%RestrGrain%New
        END DO
        
        END SUBROUTINE Save_Converged_Vals
    
        SUBROUTINE Write_to_file (Node, Unit)
        
        CLASS (FGR_Node), INTENT(IN) :: Node
        
        INTEGER(ipk), INTENT(IN) :: unit
        
        WRITE (unit,*) Node%ID
        WRITE (unit,*) Node%ansr
        WRITE (unit,*) Node%conGas
        WRITE (unit,*) Node%rdGas
        WRITE (unit,*) Node%brnrefab
        WRITE (unit,*) Node%Burnup
        WRITE (unit,*) Node%gp
        WRITE (unit,*) Node%rls
        WRITE (unit,*) Node%grn
        WRITE (unit,*) Node%Temp
        WRITE (unit,*) Node%gb
        WRITE (unit,*) Node%grs
        WRITE (unit,*) Node%gg
        WRITE (unit,*) Node%g
        
        END SUBROUTINE Write_to_file
        
        SUBROUTINE Read_from_file (Node, Unit)
        CLASS (FGR_Node), INTENT(INOUT) :: Node
        
        INTEGER(ipk), INTENT(IN) :: unit
        
        READ (unit,*) Node%ID
        READ (unit,*) Node%ansr
        READ (unit,*) Node%conGas
        READ (unit,*) Node%rdGas
        READ (unit,*) Node%brnrefab
        READ (unit,*) Node%Burnup
        READ (unit,*) Node%gp
        READ (unit,*) Node%rls
        READ (unit,*) Node%grn
        READ (unit,*) Node%Temp
        READ (unit,*) Node%gb
        READ (unit,*) Node%grs
        READ (unit,*) Node%gg
        READ (unit,*) Node%g
        
        END SUBROUTINE Read_from_file
        
        ELEMENTAL SUBROUTINE pf_norm (Node, pfave)
        CLASS (ANS54_Node), INTENT(INOUT) :: Node
        REAL(r8k), INTENT(IN) :: pfave
        
        Node%pf = Node%pf / pfave
        
        END SUBROUTINE pf_norm
        
        !ELEMENTAL SUBROUTINE scangas (Node, AxNode)
        SUBROUTINE scangas (Node, AxNode)
        USE Variables, ONLY : ngasr, rc
        CLASS (FGR_Node), DIMENSION(:), INTENT(INOUT) :: Node
        !>@brief
        !> This subroutine saves gas concentrations used in the plot file.
        !>@author
        !> IRSN
        !> Updated by Ian Porter, NRC, 1/11/2017
        !>@date
        !> 2014
        !
        ! Input
        !
        ! AxNode - Axial node indicator
        !
        INTEGER(ipk) :: irad
        INTEGER(ipk), INTENT(IN) :: AxNode
        REAL(r8k), DIMENSION(ngasr) :: tmpgas
        
        DO irad = 1, ngasr
            Node(irad)%conGas = Node(irad)%gp%new - Node(irad)%rls%new
            ! This sets dimensions
            Node(irad)%rdGas = Node(ngasr-irad+1)%ansr
            tmpgas(irad) = Node(ngasr-irad+1)%ansr
        END DO
        ! This over-rides the dimensions already set.
        Node(1)%rdGas = rc(AxNode)
        DO irad = 2, (ngasr + 1)
            Node(irad)%rdGas = SQRT(2.0_r8k * tmpgas(irad-1) ** 2 - Node(irad-1)%rdGas ** 2)
        END DO
        
        END SUBROUTINE scangas
        
        ELEMENTAL SUBROUTINE Setup_IFBA (Node, dp, deltaz, ZrB2thick, b10, zrb2den)
        USE Conversions, ONLY : Pi, Avogadro, in2tocm2, ftocm
        IMPLICIT NONE
        !>@brief
        !> Sets up the ZrB2 coating information
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/6/2017
        !
        ! Input
        !
        ! dp        - Pellet diameter (in)
        ! deltaz    - Axial node length (ft)
        ! ZrB2thick - Thickness of ZrB2 coating on outside of fuel pellet (in)
        !
        ! Internal
        !
        ! MolarMass_ZrB2 - Grams ZrB2 per mol
        !
        CLASS (IFBA_Coating), INTENT(INOUT) :: Node
        REAL(r8k) :: MolarMass_ZrB2
        REAL(r8k), INTENT(IN) :: dp, deltaz, ZrB2thick, b10, zrb2den
        
        ! Theoretical density (g/cm^3)
        Node%TDen = ZrB2_TDen
        ! ZrB2 coating thickness (in)
        Node%thickness = ZrB2thick
        IF (Node%thickness == 0.0_r8k) THEN
            ! If no thickness, then no B10 concentration
            Node%Boron10 = 0.0_r8k
        ELSE
            ! Assume constant axial B10 concentration
            Node%Boron10 = B10
        END IF
        ! As-fabricated Boron concentration (5.840e22 atoms B/cm^3 assuming 90% TD)
        ! Molar mass (grams ZrB2 per mol) 91.224 g Zr/mol, b10 atom% (10 g/mol) + (1-b10) atom% (11g/mol)
        MolarMass_ZrB2 = 91.224 + 2.0_r8k * ((1.0_r8k - b10 / 100.0_r8k) * 11.0_r8k + (b10 / 100.0_r8k) * 10.0_r8k)
        ! atoms Boron / cm3 ZrB2
        ! = ZrB2 Theoretical Density * As-Fabricated %TD * Avogadro's # / ZrB2 Molar Mass * 2 atoms Boron per atom Zr
        Node%atom%B = Node%TDen * zrb2den / 100.0_r8k * Avogadro / MolarMass_ZrB2 * 2.0_r8k
        ! Volume of ZrB2 at each axial node (cm3)
        Node%Volume = Pi * ((dp / 2.0_r8k + Node%thickness) ** 2 - (dp / 2.0_r8k) ** 2) * in2tocm2 * deltaz * ftocm
        
        END SUBROUTINE Setup_IFBA
        
        PURE ELEMENTAL SUBROUTINE Helium_Production (Node, Power, gasflg)
        USE Variables, ONLY : it, ifba, zrb2den, delh
        USE Refabrication, ONLY : irefab
        USE Conversions, ONLY : hrtosec
        IMPLICIT NONE
        !>@brief
        !> Calculates the production of Helium from ZrB2 coating
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/6/2017
        !
        ! Input
        !
        ! Power - Rod average surface heat flux (* NEED TO VERIFY *)
        !
        ! Internal
        !
        ! B10Remaining - B-10 remaining (B-10/cm3)
        !
        CLASS (IFBA_Coating), INTENT(INOUT) :: Node
        REAL(r8k) :: B10Remaining
        REAL(r8k), INTENT(IN) :: Power
        LOGICAL, INTENT(IN) :: gasflg
        
        ! Helium production in Atoms He/cm^3/s
        IF (Node%Boron10 > 0.0_r8k) THEN
            ! factor of 2.46 because Richard Pagh used this for Density.
            ! 5.472 is 90% TD (6.08 is TD)
            Node%HeProd = (-(9.66127e8_r8k * ifba + 1.088109e11_r8k) * (Node%Boron10 ** 2) + (-2.10296e10_r8k * ifba + &
              &              4.88343e13_r8k) * Node%Boron10) * Power / 5.64_r8k * 6.08_r8k * &
              &             (ZrB2den / 100.0_r8k) / 4.53_r8k
            ! Convert from rate to He/cm^3
            Node%HeProd = Node%HeProd * delh * hrtosec
            ! Calculate remaining B-10
            B10Remaining = Node%Boron10 * Node%atom%B / 100.0_r8k
            ! Ensure does not produce more He than the B-10 that's available
            Node%HeProd = MIN(Node%HeProd, B10Remaining)
        ELSE
            Node%HeProd = 0.0_r8k
        END IF
        
        ! He in atoms He/cm^3
        Node%He%New = Node%He%Old + Node%HeProd
        IF (gasflg) THEN
            Node%He%Old = Node%He%New
            IF (it == irefab) Node%He%Old = 0.0_r8k
            ! Deplete B-10 concentration(%) = Last conc. - He atoms produced (1 He atom produced by 1 B-10 atom)
            Node%Boron10 = MAX(0.0_r8k, (Node%Boron10 - Node%HeProd / Node%atom%B * 100.0_r8k))
        END IF
        
        END SUBROUTINE Helium_Production
        
        PURE SUBROUTINE Convert_IFBA_Release (Node)
        USE Conversions, ONLY : Avogadro
        IMPLICIT NONE
        !>@brief
        !> Converts ZrB2 IFBA Helium release from He/cm^3 to moles of gas
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/23/2017
        
        CLASS (IFBA_Coating), INTENT(INOUT) :: Node
        
        Node%IFBArel = Node%He%New * Node%Volume / Avogadro
        
        END SUBROUTINE Convert_IFBA_Release
        
        IMPURE ELEMENTAL SUBROUTINE Read_IFBA (Node, unit)
        IMPLICIT NONE
        !>@brief
        !> Reads the ZrB2 coating information for restart
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/6/2017
        
        CLASS (IFBA_Coating), INTENT(INOUT) :: Node
        INTEGER(ipk), INTENT(IN) :: unit
        
        READ (unit,*) Node%TDen
        READ (unit,*) Node%Volume
        READ (unit,*) Node%HeProd
        READ (unit,*) Node%Boron10
        READ (unit,*) Node%thickness
        READ (unit,*) Node%IFBArel
        READ (unit,*) Node%atom%B
        READ (unit,*) Node%atom%B10
        READ (unit,*) Node%atom%B11
        READ (unit,*) Node%He%New
        READ (unit,*) Node%He%Old
        
        END SUBROUTINE Read_IFBA
        
        IMPURE ELEMENTAL SUBROUTINE Write_IFBA (Node, unit)
        IMPLICIT NONE
        !>@brief
        !> Writes the ZrB2 coating information for restart
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/17/2017
        
        CLASS (IFBA_Coating), INTENT(IN) :: Node
        INTEGER(ipk), INTENT(IN) :: unit
        
        WRITE (unit,*) Node%TDen
        WRITE (unit,*) Node%Volume
        WRITE (unit,*) Node%HeProd
        WRITE (unit,*) Node%Boron10
        WRITE (unit,*) Node%thickness
        WRITE (unit,*) Node%IFBArel
        WRITE (unit,*) Node%atom%B
        WRITE (unit,*) Node%atom%B10
        WRITE (unit,*) Node%atom%B11
        WRITE (unit,*) Node%He%New
        WRITE (unit,*) Node%He%Old
        
        END SUBROUTINE Write_IFBA
END MODULE FGR_Mesh