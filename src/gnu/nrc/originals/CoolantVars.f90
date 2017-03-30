MODULE CoolantVars
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module was taken from FRAPTRAN and contains coolant information
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 7/15/2016
    
    PUBLIC
    
    ! Available Coolant Types
    INTEGER(ipk), PARAMETER :: Supplied = 0_ipk, Water = 1_ipk, Helium = 2_ipk, Sodium = 3_ipk
    ! Available Coolant Supply Options
    INTEGER(ipk), PARAMETER :: Input_File = 0_ipk, Coolant_File = 1_ipk, TH_Link = 2_ipk
    ! Methods of interpolating user-supplied values for tblka & htca
    INTEGER(ipk), PARAMETER :: Lars_Method = 0_ipk, Ian_Method = 1_ipk, SuppliedNodalVals = 2_ipk
    ! Available coolant temperature models
    INTEGER(ipk), PARAMETER :: Inlet = 1_ipk, Average = 2_ipk, Outlet = 3_ipk, FRAPCON_Model = 7_ipk
    ! Available saturated boiling HTC correlations
    INTEGER(ipk), PARAMETER :: Thom = 0_ipk, Chen = 1_ipk, Jens_Lottes = 2_ipk
    ! Available film boiling HTC correlations
    INTEGER(ipk), PARAMETER :: Groeneveld5_9 = 0_ipk, Groeneveld5_7 = 1_ipk, Bishop_Sandberg_Tong = 2_ipk, &
      &                        Groeneveld_Delorme = 3_ipk
    ! Available transition boiling HTC correlations
    INTEGER(ipk), PARAMETER :: Tong_Young = 0_ipk, Condie_Bengston = 1_ipk, Bjornard_Griffith = 2_ipk
    ! Available Post-CHF correlations
    INTEGER, PARAMETER :: EPRI_1 = 0_ipk, Bowring = 1_ipk, MacBeth = 2_ipk, Zuber = 3_ipk, Biasi = 4_ipk, Stagnant = 5_ipk
    ! Coolant Fluid
    INTEGER(ipk) :: CoolantType = Water
    ! Coolant supply option
    INTEGER(ipk) :: CoolantSupply = Input_File
    ! User specified forcing Heat Transfer Coefficient
    LOGICAL :: Forced_HTC_Mode = .FALSE.
    ! User-defined forced HTC mode
    INTEGER(ipk) :: ForcedMode = 0_ipk
    ! Coolant interpolation method
    INTEGER(ipk) :: InterpMethod = Ian_Method
    ! Coolant calculation type switch
    INTEGER(ipk) :: nqchn = Inlet
    ! # of coolant channels
    INTEGER(ipk) :: nchn = 1_ipk
    ! CHF correlation indicator
    INTEGER(ipk) :: jchf = EPRI_1
    ! Nucleat boiling heat transfer correlation
    INTEGER(ipk) :: nbhtc = Jens_Lottes
    ! Film boiling correlation
    INTEGER(ipk) :: jfb = Groeneveld5_9
    ! Transition boiling heat transfer correlation
    INTEGER(ipk) :: jtr = Tong_Young
    ! Suboption to specify the FRAP-T4 FLECHT correlation instead of the generalized FLECHT correlation
    LOGICAL :: frapt4 = .FALSE.
    ! Flag to print coolant conditions to output file
    INTEGER(ipk) :: ncall = 0_ipk            !
    INTEGER(ipk) :: nvol = 0_ipk             !
    INTEGER(ipk) :: ithymx = 0_ipk           !
    INTEGER(ipk) :: ixazim = 0_ipk           !
    INTEGER(ipk) :: nbrfht = 0_ipk           !
    INTEGER(ipk) :: kaxnlo = 0_ipk           !
    INTEGER(ipk) :: liqnod = 0_ipk           !
    INTEGER(ipk) :: izadfg = 0_ipk           !
    INTEGER(ipk) :: irup = 0_ipk             !
    INTEGER(ipk) :: nbrpst = 0_ipk           !
    INTEGER(ipk) :: nflec = 0_ipk            !
    INTEGER(ipk) :: npaxpf = 0_ipk           !
    INTEGER(ipk) :: mzq1 = 0_ipk             !
    INTEGER(ipk) :: mbdl = 0_ipk             !
    INTEGER(ipk) :: ntempf = 0_ipk           !
    INTEGER(ipk) :: nsrad3 = 0_ipk           !
    INTEGER(ipk) :: nelrad = 0_ipk           !
    INTEGER(ipk) :: nbundl = 15_ipk          ! Bundle size (15 = 15x15 bundle) to use for FLECHT correlation
    ! Flow cross sectional area (m^2)
    REAL(r8k) :: achn = 0.0_r8k
    ! Hydraulic diameter of flow channel (m)
    REAL(r8k) :: dhy = 0.0_r8k
    ! Heat equivalent diameter of flow channel (m)
    REAL(r8k) :: dhe = 0.0_r8k
    ! Saturation temperature, (F) - Carried over from Variables
    REAL(r8k) :: tsat = 0.0_r8k
    REAL(r8k) :: tc1 = 0.0_r8k                 ! 
    REAL(r8k) :: tc2 = 0.0_r8k                 ! 
    REAL(r8k) :: hliq = 0.0_r8k                ! 
    REAL(r8k) :: empytm = 1.0e20_r8k           ! User specified problem time for start of adiabatic heatup, s
    REAL(r8k) :: reflpr = 1.0e20_r8k           ! User specified time at which flooding of reactor core begins, s
    REAL(r8k) :: RadHTC = 0.0_r8k              ! Radiation heat transfer coefficient, (W/m^2*K)
    REAL(r8k) :: fldrte = 0.0_r8k              ! flood rate, in/s
    REAL(r8k) :: fldrpr = 0.0_r8k              ! Flooding rate (This may be equivalent to fldrte - IP)
    REAL(r8k) :: crfpr = 0.0_r8k               ! Carry out fraction (Flooding)
    REAL(r8k) :: zqch = 0.0_r8k                ! 
    REAL(r8k) :: oldtim = 0.0_r8k              ! 
    REAL(r8k) :: tflood = 0.0_r8k              ! Time since start of reflood, s
    REAL(r8k) :: crf = 0.0_r8k                 ! Carry out rate fraction (reflood)
    REAL(r8k) :: templo = 0.0_r8k              ! coolant temp of next lower axial node, F
    REAL(r8k) :: tsatt = 0.0_r8k               ! Saturation temperature
    REAL(r8k) :: pressi = 0.0_r8k              ! system pressure, SI units
    REAL(r8k) :: cpmult = 0.0_r8k              ! heat capacity multiplier, unitless
    REAL(r8k) :: gflow = 0.0_r8k               ! outlet mass flow rate
    REAL(r8k) :: temphi = 0.0_r8k              ! new coolant bulk temperature
    REAL(r8k) :: ruplev = 0.0_r8k              ! rupture elevation, ft
    REAL(r8k) :: refdtm = 1.0E20_r8k           ! Problem time for initiation of reflood, (s)
    REAL(r8k) :: emptm = 0.0_r8k               ! Time at which reactor core is empty and adiabatic heatup begins, (s)
    REAL(r8k) :: hydiam = 0.0_r8k              ! Channel hydraulic diameter, (m)
    REAL(r8k) :: flxsec = 0.0_r8k              ! flow channel cross sectional area, (m^2)
    REAL(r8k) :: tsub = 0.0_r8k                ! coolant subcooling (tsat - tcoolant), F
    REAL(r8k) :: pdeint = 0.0_r8k              ! 
    REAL(r8k) :: flowbk = 0.0_r8k              ! Flow blockage, %
    REAL(r8k) :: pfflec = 0.0_r8k              ! flect axial power peaking factor
    REAL(r8k) :: tfoldf = 0.0_r8k              ! Time (maybe for flooding), s
    REAL(r8k) :: pdecy = 0.0_r8k               ! 
    REAL(r8k) :: pfnuc = 0.0_r8k               ! 
    REAL(r8k) :: toldfc = 0.0_r8k              ! 
    REAL(r8k) :: zqflt = 0.0_r8k               ! 
    REAL(r8k) :: qaxpk = 0.0_r8k               ! 
    REAL(r8k) :: zpkfc = 0.0_r8k               ! 
    REAL(r8k) :: fltgap = 0.0_r8k              ! 
    REAL(r8k) :: pavgft = 0.0_r8k              ! 
    REAL(r8k) :: gum = 0.0_r8k                 ! 
    REAL(r8k) :: zad = 0.0_r8k                 ! 
    REAL(r8k) :: zs = 0.0_r8k                  ! 
    REAL(r8k) :: nu1 = 0.0_r8k                 ! 
    REAL(r8k) :: nu2 = 0.0_r8k                 ! 
    REAL(r8k) :: nu3 = 0.0_r8k                 ! 
    LOGICAL :: ShroudInput = .FALSE.           ! Identifies whether shroud temperature vs time pairs were input by user
    CHARACTER(LEN=8) :: rupflg
    CHARACTER(LEN=8) :: lodmrk
    ! Moderator heating model
    CHARACTER(LEN=15) :: ModHeatModel = 'DEFAULT'
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nhprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ntprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nvprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: npprs
    ! Cold wall and axial factor control for chf
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ichf
    
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity             ! Density for all axial nodes, (kg/m^3)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity0            ! Previous value of coolant density, (kg/m^3)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy            ! Enthalpy for all axial nodes, (J/kg)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy0           ! Previous value of coolant enthalpy, (J/kg)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: coolqual                ! Coolant quality
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs              ! Previous value of coolant pressure, (Pa)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs0             ! Previous value of coolant pressure, (Pa)
!    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BulkCoolTemp            ! Bulk coolant temperature, (K)
    
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: z1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: z2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: flthit
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: faxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qaxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: aflcht
    ! Collapsed liquid level and time data pairs, specified from beginning of reflood
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hlqcl
    ! Inlet temperature and time pairs during reflood, specified from beginning of reflood
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: temptm
    ! Reflood rate and time pairs during reflood, specified from beginning of reflood
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fldrat
    ! Reactor vessel pressure and time data pairs, specified from beginning of reflood
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: prestm
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vfrad3
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: elvrad
    ! Array of elevations of each heat transfer coefficient zone
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: htclev
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tshrda
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rcpar
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad2
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: trad3
    !
    TYPE Oxidation_var
        INTEGER(ipk) :: modmw = 1_ipk
        INTEGER(ipk) :: nIDoxide = 0_ipk
        INTEGER(ipk) :: npair = 0_ipk
        INTEGER(ipk) :: ProtectiveOxide = 0_ipk
        REAL(r8k) :: cexh2l = 0.0_r8k
        REAL(r8k) :: explenumv = 0.0_r8k
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: cexh2a
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltox
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: explenumt
    END TYPE Oxidation_var
    ! Available oxidation reactions
    INTEGER(ipk), PARAMETER :: No_Oxidation = 0_ipk, Cathcart_Pawel = 1_ipk, Baker_Just = 2_ipk
    ! Oxidation reaction model
    INTEGER(ipk) :: modmw = Cathcart_Pawel
    INTEGER(ipk), PARAMETER :: Post_Burst = 0_ipk, BurnupLimit = 1_ipk
    ! User option for something. Not described in FRAPTRAN-1.4 manual
    INTEGER(ipk) :: nIDoxide = 0_ipk
    ! # of pairs for user supplied plenum temperature
    INTEGER(ipk) :: npair = 0_ipk
    ! User option to model initial oxide as protective(0) or non-protective(1)
    INTEGER(ipk) :: ProtectiveOxide = 0_ipk
    ! Cladding hydrogen concentration for axial node k (ppm)
    REAL(r8k) :: cexh2l = 0.0_r8k
    ! External plenum volume
    REAL(r8k) :: explenumv = 0.0_r8k
    ! User specified cladding hydrogen concentration for each axial node (ppm)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cexh2a
    ! Temperature drop across the oxide layer
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltox
    ! User option for exterior plenum temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: explenumt
    ! Variables for clad-to-coolant heat transfer option (Supplied)
    ! # of heat transfer coefficient axial zones
    INTEGER(ipk) :: nhtcz = 0_ipk
    ! Heat transfer coefficient of the coolant at (time, axial node)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: htca
    ! Temperature of the coolant at (time, axial node)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tblka
    ! Variables for clad-to-coolant heat transfer options (Water, Helium, Sodium)
    ! Core average enthalpy vs. time pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hbh
    ! Inlet enthalpy vs. time pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hinta
    ! Mass flux vs. time pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gbh
    ! Pressure vs. time pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh
    ! Outlet enthalpy vs. time pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hupta
    ! Inlet temperature vs. time
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Tbulk
    ! Inlet density vs. time
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RhoBulk
    
    ! Option to model heat transfer by radiation from cladding surface to surrounding flow shroud
    CHARACTER(LEN=3) :: radiation = 'off'
    ! Suboption to specify the inner radius of flow shroud. Set to 1 to turn on
    INTEGER(ipk) :: geom = 0_ipk
    ! Inner radius of flow shroud (ft)
    REAL(r8k) :: rshrd = 0.0_r8k
    ! Suboption to specify temperature history of flow shroud. Enter a value of > 0 to turn on. temp = # of pairs in tshroud
    INTEGER(ipk) :: ntshroud = 1_ipk ! Formerly temp
    ! Flow shroud temperature and time data pairs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tshroud ! Formerly ts
    
    TYPE State
        REAL(r8k) :: Temperature = 0.0_r8k             ! Temperature, (K)
        REAL(r8k) :: thexp = 0.0_r8k                   ! Coefficient of thermal expansion, (1/degK)
        REAL(r8k) :: IsoComp = 0.0_r8k                 ! Coefficient of isothermal compressability, (1/Pa)
        REAL(r8k) :: Enthalpy = 0.0_r8k                ! Enthaly, (J/kg)
        REAL(r8k) :: Entropy = 0.0_r8k                 ! Entropy, (J/kg*K)
        REAL(r8k) :: IntEnergy = 0.0_r8k               ! Internal energy, (J/kg)
        REAL(r8k) :: Vol = 0.0_r8k                     ! Specific volume, (m^3/kg)
        REAL(r8k) :: Cp = 0.0_r8k                      ! Specific heat capacity at constant pressure, (J/kg*K)
        REAL(r8k) :: Density = 0.0_r8k                 ! Density, (kg/m^3)
    END TYPE State
    
    TYPE Coolant_Channel
        REAL(r8k) :: Pressure = 0.0_r8k                ! Pressure, (Pa)
        REAL(r8k) :: P_Sat = 0.0_r8k                   ! Saturation pressure, (Pa)
        REAL(r8k) :: T_Sat = 0.0_r8k                   ! Saturation Temperature, (K)
        REAL(r8k) :: Quality = 0.0_r8k                 ! Quality, (unitless)
        REAL(r8k) :: MassFlux = 0.0_r8k                ! Mass flux, (kg/s*m^2)
        REAL(r8k) :: ModHeat = 0.0_r8k                 ! Coolant direct moderator heating, (fraction)
        REAL(r8k) :: FlowArea = 0.0_r8k                ! Flow area, (m^2)
        REAL(r8k) :: dhe = 0.0_r8k                     ! Heated equivalent diameter, (m)
        REAL(r8k) :: dhy = 0.0_r8k                     ! Channel hydraulic diameter, (m)
        REAL(r8k) :: Elevation  = 0.0_r8k              ! Channel height, (m)
        REAL(r8k) :: Enthalpy_Inlet = 0.0_r8k          ! Enthalpy at normal inlet, (J/kg)
        REAL(r8k) :: Enthalpy_Outlet = 0.0_r8k         ! Enthalpy at normal outlet/plenum region, (J/kg)
        REAL(r8k) :: HTC = 0.0_r8k                     ! Surface-to-coolant heat transfer coefficient, (W/m^2*K)
        REAL(r8k) :: Quality_ROC = 0.0_r8k             ! Quality rate of change with respect to elevation, (1/m)
        TYPE (State) :: Bulk                           ! Bulk coolant properties
        TYPE (State) :: Liquid                         ! Liquid phase properties
        TYPE (State) :: Vapor                          ! Vapor phase properties
    CONTAINS
        PROCEDURE :: Update_Geometry                   ! Updates the coolant channel geometry based on cladding deformation
        PROCEDURE :: Velocity                          ! Coolant velocity, kg/s
    END TYPE Coolant_Channel
    
!    INTERFACE
!        GENERIC, PROCEDURE :: Avg_Nodes! (Bottom, Top)
!    END INTERFACE
    
    CONTAINS
    
    SUBROUTINE Allocate_CoolantVars (N_axial, N_Radial, N_Mesh, N_Timesteps)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine allocates variables associated with CoolantVars
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/6/2016
    !
    ! Input
    !
    ! N_Axial   - Number of axial nodes
    ! N_Radial  - Number of fuel radial mesh points
    ! N_Mesh    - Total # of radial mesh points
    !
    
    INTEGER(ipk), INTENT(IN) :: N_Axial, N_Radial, N_Mesh, N_Timesteps
    
    ALLOCATE (deltox(1:N_Axial), rcpar(1:N_Axial), htclev(1:N_Axial), CoolDensity(1:N_Axial), CoolDensity0(1:N_Axial), &
      &       CoolEnthalpy(1:N_Axial), CoolEnthalpy0(1:N_Axial), &
      &       coolqual(1:N_Axial), OldCoolPrs(1:N_Axial), OldCoolPrs0(1:N_Axial))
    
    ALLOCATE (hbh(1:2*(N_Timesteps+1)), hinta(1:2*(N_Timesteps+1)), hupta(1:2*(N_Timesteps+1)), &
      &       gbh(1:2*(N_Timesteps+1)), pbh(1:2*(N_Timesteps+1)), prestm(1:2*(N_Timesteps+1)), &
      &       temptm(1:2*(N_Timesteps+1)), fldrat(1:2*(N_Timesteps+1)), RhoBulk(1:2*(N_Timesteps+1)), &
      &       hlqcl(1:2*(N_Timesteps+1)), tshroud(1:2*(N_Timesteps+1)), Tbulk(1:2*(N_Timesteps+1)))
    
    ALLOCATE (htca(1:2*(N_Timesteps+1), 1:N_Axial), tblka(1:2*(N_Timesteps+1), 1:N_Axial))
    
    deltox = 0.0_r8k
    rcpar = 0.0_r8k
    htclev = 0.0_r8k
    CoolDensity = 0.0_r8k
    CoolDensity0 = 0.0_r8k
    CoolEnthalpy = 0.0_r8k
    CoolEnthalpy0 = 0.0_r8k
    coolqual = 0.0_r8k
    OldCoolPrs = 0.0_r8k
    OldCoolPrs0 = 0.0_r8k
!    BulkCoolTemp = 0.0_r8k
    
    nhtcz = 0
    hbh = 0.0_r8k
    hinta = 0.0_r8k
    hupta = 0.0_r8k
    gbh = 0.0_r8k
    pbh = 0.0_r8k
    prestm = 0.0_r8k
    temptm = 0.0_r8k
    fldrat = 0.0_r8k
    prestm = 0.0_r8k
    hlqcl = 0.0_r8k
    tshroud = 0.0_r8k
    RhoBulk = 0.0_r8k
    Tbulk = 0.0_r8k
    
    htca = 0.0_r8k
    tblka = 0.0_r8k
    
    END SUBROUTINE Allocate_CoolantVars
    
    SUBROUTINE Update_Geometry (Node, AxNode)
    USE Kinds
    USE Conversions, ONLY : pi, in2tom2, intom, psitoPa, lbhrft2toksm2
    USE Variables, ONLY : Pitch, dco, de, p2, it, modheat
    USE GammaHeating
    IMPLICIT NONE
    !>@brief
    !>
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/6/2016
    
    CLASS (Coolant_Channel), INTENT(INOUT) :: Node
    INTEGER(ipk), INTENT(IN) :: AxNode
    INTEGER(ipk) :: z
    
    z = MAX(AxNode, 1)
    
    ! Coolant Geometry:
    IF (radiation == 'off' .OR. geom == 0) THEN
        ! Case 1: Fuel rod in middle of cluster of fuel rods
        
        ! Flow area (m^2) ! Pitch is in inches, dco is in inches
        IF (achn == 0.0_r8k) THEN
            Node%FlowArea = Pitch ** 2 - (Pi * (dco(z) / 2.0_r8k) ** 2)
        ELSE
            Node%FlowArea = achn
        END IF
        ! Convert from in2 to m^2
        Node%FlowArea = Node%FlowArea * in2tom2
        
        ! Heated equivalent diameter (m)
        Node%dhe = de(z) * intom
        
        ! Channel hydraulic diameter (m)
        Node%dhy = 4.0_r8k * Node%FlowArea / (Pi * dco(z) * intom)
    ELSE
        ! Case 2: Single rod surrounded by unheated flow shroud
        
        ! Flow area (m^2)
        IF (achn == 0.0_r8k) THEN
            Node%FlowArea = (Pi * rshrd ** 2) - (Pi * (dco(z) / 2.0_r8k) ** 2)
        ELSE
            Node%FlowArea = achn
        END IF
        ! Convert from in2 to m2
        Node%FlowArea = Node%FlowArea * in2tom2
        
        ! Heated equivalent diameter (m)
        Node%dhe = de(z) * intom
        
        ! Channel hydraulic diameter (m)
        Node%dhy = 4.0_r8k * Node%FlowArea / (Pi * dco(z) * intom + Pi * rshrd * 2.0_r8k)
    END IF
    
    ! Pressure (psi)
    Node%Pressure = p2(MAX(it,1)) * psitoPa
    
    ! Determine the moderator heating fraction. (NOT CURRENTLY USED) It will come from either (by rank):
    ! (1) the user, (2) Using the built-in coolant density correlation, (3) the original default value of 0.02.
    SELECT CASE (ModHeatModel)
    CASE ('CoolantDensity')
        Node%ModHeat = GammaCoolant(Node%Bulk%Density)
    CASE ('UserValue')
        Node%ModHeat = modheat
    CASE DEFAULT
        Node%ModHeat = 0.02_r8k
    END SELECT
    
    END SUBROUTINE Update_Geometry
    
    FUNCTION Velocity (Node)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Computes the coolant velocity based on mass flux and flow area
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/10/2017
    !
    ! Input
    !
    ! %Mass Flux - Coolant mass flux, (kg/m^2*s)
    ! %FLowArea  - Coolant flow area, (m^2)
    !
    ! Output
    !
    ! Velocity   - Coolant velocity, (kg/s)
    !
    
    CLASS (Coolant_Channel), INTENT(IN) :: Node
    REAL(r8k) :: Velocity
    
    Velocity = Node%MassFlux * Node%FlowArea
    
    END FUNCTION Velocity
    
    FUNCTION Avg_Nodes (Bottom, Top) RESULT (Average)
    
    TYPE (Coolant_Channel), INTENT(IN) :: Bottom, Top
    TYPE (Coolant_Channel) :: Average
    
    Average%Bulk%Temperature = (Bottom%Bulk%Temperature + Top%Bulk%Temperature) / 2.0_r8k
    Average%Bulk%Density = (Bottom%Bulk%Density + Top%Bulk%Density) / 2.0_r8k
    
    END FUNCTION Avg_Nodes
    
END MODULE CoolantVars