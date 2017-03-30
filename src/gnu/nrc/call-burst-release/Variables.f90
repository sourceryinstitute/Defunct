MODULE Variables
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains all of the common variables that are set by the input file and used within the code.
    !>@author
    !> Ian Porter, NRC
    
    PUBLIC
    
    !
    ! Scalars
    !
    ! Failure node
    INTEGER(ipk) :: IFaila = 0_ipk
    ! Node of non-uniform elongation
    INTEGER(ipk) :: knonue = 0_ipk
    ! Indicator to terminate program
    INTEGER(ipk) :: iquit = 0_ipk
    ! Number of axial nodes + 1
    INTEGER(ipk) :: ir1 = 0_ipk
    ! Time-step index
    INTEGER(ipk) :: it = 0_ipk
    ! Gas loop iteration index
    INTEGER(ipk) :: iter = 0_ipk
    ! Minimum (lowest) axial node being modeled
    INTEGER(ipk) :: jmin = 1_ipk
    ! Maximum (highest) axial node being modeled
    INTEGER(ipk) :: jmax = 1_ipk
    ! Axial node index currently being modeled
    INTEGER(ipk) :: j = 1_ipk
    ! Total number of axial power shapes
    INTEGER(ipk) :: jjc = 0_ipk
    ! Peak node used for output printing
    INTEGER(ipk) :: jfix = 0_ipk
    ! Gap iteration indicator
    INTEGER(ipk) :: k = 0_ipk
    ! Maximum # of iterations
    INTEGER(ipk), PARAMETER :: MaximumIterations = 120_ipk
    ! Steady-state indicator flag
    LOGICAL :: SteadyState = .TRUE.
    ! # of time step reductions without time step increase
    INTEGER(ipk) :: ndtred = 0_ipk
    ! Axial power shape index
    INTEGER(ipk) :: m = 0_ipk
    ! Indicator for first tiem step to declare variables in fracas
    INTEGER(ipk) :: nab = 0_ipk
    ! Elastic/Plastic flag (0 = Elastic calculation performed, 1 = Plastic calculation performed)
    INTEGER(ipk) :: nplast = 0_ipk
    ! Number of radial nodes (nr) minus 1
    INTEGER(ipk) :: nrm1 = 0_ipk
    ! Crud model (0 or 1 = constant layer, 2= time dependent)
    INTEGER(ipk) :: icor = 0_ipk
    ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
    INTEGER(ipk) :: idxgas = 0_ipk
    ! Option to specify variable axial node length (1 = ON, 0 = OFF(Default))
    INTEGER(ipk) :: ivardm = 0_ipk
    ! Creep flag (0 = no creep, 1 = creep)
    INTEGER(ipk) :: ncreep = 0_ipk
    ! New problem indicator
    INTEGER(ipk) :: newprb = 0_ipk
    ! Flag to turn on permanent fuel crack healing (0 = Off,  1 = On)
    INTEGER(ipk) :: nheal = 0_ipk
    ! Number of axial nodes minus 1
    INTEGER(ipk) :: nt = 0_ipk
    ! Central Void Index. (0 = No central void, 1 = Central void exists)
    INTEGER(ipk) :: nvoid = 0_ipk
    ! Output options
    ! Specifies whether to print plot information for use with Excel package FRAPlot or APT Plot
    ! (0 = no, 1 = limited, 2 = detailed)
    INTEGER(ipk) :: nplot = 0_ipk
    ! Specifies if coupled to T/H code  (0 = no, 1 = yes) [Not used]
    INTEGER(ipk) :: coupled = 0_ipk
    ! Specifies to write a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER(ipk) :: nrestr = 0_ipk
    ! Specifies the output file printing option (-1 = axial summary, 0 = peak-power node, 1 = all axial nodes)
    INTEGER(ipk) :: jdlpr = 0_ipk
    ! Specifies the output file print control (0 = each timestep, 1 = input & summary only)
    INTEGER(ipk) :: nopt = 0_ipk
    ! Specifies to start from a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER(ipk) :: nread = 0_ipk
    ! Specifies to write a FRAPCON-to-FRAPTRAN restart tape  (0 = no, 1 = yes)
    INTEGER(ipk) :: ntape = 0_ipk
    ! File unit #'s
    ! Output File
    INTEGER(ipk) :: ounit = 6_ipk
    ! Input File
    INTEGER(ipk) :: iunit = 55_ipk
    ! Scratch file
    INTEGER(ipk) :: scrunit = 5_ipk
    ! .frttr file
    INTEGER(ipk) :: funit = 50_ipk
    ! Plot File
    INTEGER(ipk) :: punit = 66_ipk
    ! FRAPCON-to-FRAPCON restart file WRITE
    INTEGER(ipk) :: ntaps = 12_ipk
    ! FRAPCON-to-FRAPCON restart file READ
    INTEGER(ipk) :: ntapi = 13_ipk
    ! FRAPCON-to-FRAPTRAN restart file
    INTEGER(ipk) :: ftunit = 22_ipk
    ! Dating file
    INTEGER(ipk) :: ddunit = 8_ipk
    ! Coolant conditions File (used only If nqchn = 2)
    INTEGER(ipk) :: ntco = 4_ipk
    ! Unit options
    INTEGER(ipk), PARAMETER :: SI = 0_ipk, British = 1_ipk
    ! Input/Output units
    INTEGER(ipk) :: nunits = British
    ! Number of timesteps
    INTEGER(ipk) :: im = 0_ipk
    ! Cladding mechanical deformation model
    INTEGER(ipk) :: mechan = 2_ipk
    ! Number of axial nodes
    INTEGER(ipk) :: na = 9_ipk
    ! Number of radial gas release nodes (equal-volume) in the pellet for FGR calculations
    INTEGER(ipk) :: ngasr = 45_ipk
    ! Number of radial elements in the cladding for the FEA model
    INTEGER(ipk) :: nce = 5_ipk
    ! Number of fuel mesh nodes
    INTEGER(ipk) :: nfmesh = 17_ipk
    ! Number of radial nodes in the pellet for thermal calculations
    INTEGER(ipk) :: nr = 17_ipk
    ! Number of cladding mesh nodes
    INTEGER(ipk) :: ncmesh = 2_ipk
    ! Number of gas mesh nodes
    INTEGER(ipk) :: ngmesh = 2_ipk
    ! Number of oxide mesh nodes
    INTEGER(ipk) :: noxide = 0_ipk
    ! Number of crud mesh nodes
    INTEGER(ipk) :: ncrud = 0_ipk
    ! Clad ID mesh point
    INTEGER(ipk) :: ncladi = 0_ipk
    ! Sum of fuel and cladding mesh nodes
    INTEGER(ipk) :: nmesh = 0_ipk
    ! Number of axial nodes (na) * number of timesteps (im)
    INTEGER(ipk) :: naxim = 0_ipk
    ! Number of materials solution can solve for
    INTEGER(ipk), PARAMETER :: nomat = 5
    ! Melting temperature for each material
    REAL(r8k), DIMENSION(nomat) :: tmelt = 0.0_r8k
    ! Number of graphing axial nodes [Not used]
    INTEGER(ipk) :: graphna = 0_ipk
    ! Number of graphing radial nodes [Not used]
    INTEGER(ipk) :: graphnr = 0_ipk
    ! Available cladding types
    INTEGER(ipk), PARAMETER :: Zirc2 = 2_ipk, Zirc4 = 4_ipk, M5 = 5_ipk, ZIRLO = 6_ipk, OptZIRLO = 7_ipk, &
      &                        Zr1Nb = 9_ipk, E110 = 10_ipk, SiC = 11_ipk, FeCrAl = 15_ipk, SS = 16_ipk
    ! Cladding type
    INTEGER(ipk) :: icm = Zirc4
    ! Available oxide types
    INTEGER(ipk), PARAMETER :: ZrO2 = 0_ipk, SiO2 = 1_ipk
    ! Oxide type
    INTEGER(ipk) :: ioxm = ZrO2
    ! Available fuel types
    INTEGER(ipk), PARAMETER :: UO2 = 0_ipk, MOX_NFI = 1_ipk, MOX_Halden = 2_ipk
    ! Fuel type (Default to UO2)
    INTEGER(ipk) :: imox = UO2
    ! Available plant types
    INTEGER(ipk), PARAMETER :: PWR = -2_ipk, BWR = -3_ipk, HBWR = -4_ipk
    ! Plant type
    INTEGER(ipk) :: iplant
    ! Zirc-2 vintage types
    INTEGER(ipk), PARAMETER :: Pre_1998 = 0_ipk, Post_1998 = 1_ipk
    ! Zircaloy-2 vintage (Used when icm = 2) (0 = pre-1998, 1 = post-1998)
    INTEGER(ipk) :: zr2vintage = Post_1998
    ! Available FGR Models
    INTEGER(ipk), PARAMETER :: ANS54_1982=1_ipk, Massih=2_ipk, FRAPFGR=3_ipk, ANS54_2011=4_ipk
    ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
    INTEGER(ipk) :: ngasmod = Massih
    ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
    INTEGER(ipk) :: iq
    ! Timestep to begin calculation of fission gas release
    INTEGER(ipk) :: igas
    ! Internal pressure calculation for FEA model
    INTEGER(ipk) :: igascal
    ! Suboption to choose between assuming perfectly stochiometric oxide or stoichiometry gradient in Cathcart-Pawel model
    INTEGER(ipk) :: iStoicGrad = 0     ! (0 = Perfect stoichiometry, 1 = Stoichiometry gradient)
    ! Specify to use fixed cladding surface temperatures
    INTEGER(ipk) :: ifixedtsurf
    ! Specify which type of coolant conditions to use (0 = constant, 1 = time-dependent)
    INTEGER(ipk) :: nsp
    ! Specify whether to use user-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER(ipk) :: ifixedcoolt
    ! Specify whether to use user-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER(ipk) :: ifixedcoolp
    ! Specify time integration technique (0 = None, 1 = Linear Interpolation, 2 = Histogram)
    INTEGER(ipk) :: TimeIntegration
    ! Flag for type of Pu used in MOX (Used if icm = 1 or 2)
    INTEGER(ipk) :: moxtype
    ! Assembly geometry flag
    INTEGER(ipk) :: geom
    ! Original number of timesteps when invoking timestep reduction
    INTEGER(ipk), TARGET :: im_old
    ! # of iterations performed in transient solution
    INTEGER(ipk) :: IterationCount
    ! Timestep indicator
    INTEGER(ipk) :: ntstep
    ! Option to specify an explicit solution (if soltyp = 1). One iteration per timestep is performed and no check is made.
    INTEGER(ipk) :: soltyp = 0
    ! Minimum diametral gap thickness
    REAL(r8k) :: gapmin = 0.0_r8k
    ! Hot state unrelocated radial gap (m)
    REAL(r8k) :: gaph = 0.0_r8k
    ! Hot state relocated radial gap (m)
    REAL(r8k) :: gapt = 0.0_r8k
    ! Initial moles of air in the rod
    REAL(r8k) :: airin = 0.0_r8k
    ! Initial nitrogen concentration in the fuel (moles)
    REAL(r8k) :: angi = 0.0_r8k
    ! Initial moles of N2 in the rod
    REAL(r8k) :: an2in = 0.0_r8k
    ! Initial moles of Argon in the rod
    REAL(r8k) :: argin = 0.0_r8k
    ! Average fast neutron flux
    REAL(r8k) :: avflux = 0.0_r8k
    ! Rod average burnup to end of power-time stpe (MWd/mtU)
    REAL(r8k) :: RodAvgBU = 0.0_r8k
    ! Rod average burnup to beginning of power-time stpe (MWd/mtU)
    REAL(r8k) :: buold = 0.0_r8k
    ! Minimum burnup for calculating ID oxidation (GWd/MTU) - User Input
    REAL(r8k) :: BuOxide = 30.0_r8k
    ! Cold free volume including dishes and chamfers
    REAL(r8k) :: cfv = 0.0_r8k
    ! Cold free volume assuming cylindrical pellets
    REAL(r8k) :: cfva = 0.0_r8k
    ! Convergence criteria on gap temperature drop
    REAL(r8k) :: convc = 0.0_r8k
    ! Empty volume of the plenum
    REAL(r8k) :: cpv = 0.0_r8k
    ! Outer clad diameter at beginning of life
    REAL(r8k) :: dcobol = 0.0_r8k
    ! Average burnup during power-time step for node j (MWd/mtU)
    REAL(r8k) :: delbp = 0.0_r8k
    ! Rod average burnup during power-time step (MWd/mtU)
    REAL(r8k) :: delbu = 0.0_r8k
    ! Time step size (hours)
    REAL(r8k) :: delh = 0.0_r8k
    ! Temeprature drop across the crud (F)
    REAL(r8k) :: deltcr = 0.0_r8k
    ! Temperature drop acros the film using Dittus-Boelter (F)
    REAL(r8k) :: deltdb = 0.0_r8k
    ! Temperature drop acros the film total using forced convection (F)
    REAL(r8k) :: deltfc = 0.0_r8k
    ! Temperature drop across the film using Jens-Lottes (F)
    REAL(r8k) :: deltjl = 0.0_r8k
    ! Time step size (seconds)
    REAL(r8k) :: delhs = 0.0_r8k
    ! Relative change in length with respect to the fuel length change (in)
    REAL(r8k) :: dlrel = 0.0_r8k
    ! Change in length of the active cladding length (in)
    REAL(r8k) :: dlrod = 0.0_r8k
    ! Diameter of the fuel pellet hot and swelled
    REAL(r8k) :: dphf = 0.0_r8k
    ! Diameter of the fuel pellet hot and swelled plus relocation
    REAL(r8k) :: dphfrl = 0.0_r8k
    ! Fuel stack axial extension (in)
    REAL(r8k) :: dhfll = 0.0_r8k
    ! Initial moles of fission gas in the rod
    REAL(r8k) :: fgin = 0.0_r8k
    ! Cold fuel open porosity fraction
    REAL(r8k) :: fpor1 = 0.0_r8k
    ! Total hot crack volume (in^3)
    REAL(r8k) :: hcrv = 0.0_r8k
    ! Total hot clad volume (in^3)
    REAL(r8k) :: hcv = 0.0_r8k
    ! Total hot dish + interface volume (in**3) or just interface vol. if rc > 0.0 or hdish = 0.0
    REAL(r8k) :: hdshv = 0.0_r8k
    ! Initial moles of helium in the rod
    REAL(r8k) :: hein = 0.0_r8k
    ! Initial moles of water vapor in the rod
    REAL(r8k) :: h2oin = 0.0_r8k
    ! Initial moles of hydrogen in the rod
    REAL(r8k) :: h2in = 0.0_r8k
    ! Maximum hot fuel stack length (in)
    REAL(r8k) :: hfll = 0.0_r8k
    ! Dittus-Boelter film heat transfer coefficient (btu/hr-ft**2-F)
    REAL(r8k) :: hflmdb = 0.0_r8k
    ! Forced convection film coefficient (btu/hr-ft**2-F)
    REAL(r8k) :: hflmp = 0.0_r8k
    ! Increase of fuel stack height (%)
    REAL(r8k) :: hfper = 0.0_r8k
    ! Total hot fuel volume (in^3)
    REAL(r8k) :: hfv = 0.0_r8k
    ! Total gap conductance (btu/hr-ft^2-F)
    REAL(r8k) :: hgapt = 0.0_r8k
    ! Total hot gap volume (in^3)
    REAL(r8k) :: hgv = 0.0_r8k
    ! Conduction contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k) :: hgap = 0.0_r8k
    ! Contact contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k) :: hsolid = 0.0_r8k
    ! Radiation contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k) :: hgapr = 0.0_r8k
    ! Hot plenum length (in)
    REAL(r8k) :: hpl = 0.0_r8k
    ! Total hot porosity volume (in**3)
    REAL(r8k) :: hporv = 0.0_r8k
    ! Hot plenum volume (in**3)
    REAL(r8k) :: hpv = 0.0_r8k
    ! Total annulus volume (in**3)
    REAL(r8k) :: hva = 0.0_r8k
    ! Initial water content in the fuel (moles)
    REAL(r8k) :: h2omi = 0.0_r8k
    ! Initial moles of krypton in the rod
    REAL(r8k) :: kryin = 0.0_r8k
    ! Nusselt number of plentum gas mixture
    REAL(r8k) :: nu = 0.0_r8k
    ! Fuel dish volume fraction
    REAL(r8k) :: pecdh = 0.0_r8k
    ! Unknown. Used in burnup calculations
    REAL(r8k) :: rprm1 = 0.0_r8k
    ! Rod intenal gas pressure (psia)
    REAL(r8k) :: press = 0.0_r8k
    ! Initial fuel porosity
    REAL(r8k) :: prty = 0.0_r8k
    ! Cladding inner radius
    REAL(r8k) :: rci = 0.0_r8k
    ! Cladding outer radius
    REAL(r8k) :: rco = 0.0_r8k
    ! Average linear heat generation rate (kW/ft)
    REAL(r8k) :: qav = 0.0_r8k
    ! Peak linear heat generation rate (kW/ft)
    REAL(r8k) :: qpeak = 0.0_r8k
    ! Peak ZrO2 weight gain (gram/m^2)
    REAL(r8k) :: pkZrO2WtGain = 0.0_r8k
    ! Fuel roughness volume (%)
    REAL(r8k) :: rfnvff = 0.0_r8k
    ! As-fabricated pellet radius (in)
    REAL(r8k) :: rp = 0.0_r8k
    ! True cladding strain rate (1/s)
    REAL(r8k) :: rstran = 0.0_r8k
    ! Radius out to the region where crack begins (m)
    REAL(r8k) :: rtran = 0.0_r8k
    ! Sum of axial crack volumes / crack temperature (in^3/F)
    REAL(r8k) :: sumck = 0.0_r8k
    ! Sum of axial dish volumes / fuel avg. temp. (in**3/F)
    REAL(r8k) :: sumdh = 0.0_r8k
    ! Total fuel surface displacement due to thermal expan (in)
    REAL(r8k) :: sumexp = 0.0_r8k
    ! Sum of axial gap volumes / gap temperatures (in**3/F)
    REAL(r8k) :: sumgp = 0.0_r8k
    ! Sum of axial porosity volumes / porosity temps (in**3/F)
    REAL(r8k) :: sumpor = 0.0_r8k
    ! Sum of axial roughness volumes / roughness temperature
    REAL(r8k) :: sumrg = 0.0_r8k
    ! Total problem time (hours)
    !REAL(r8k) :: t
    ! Total free gas volume
    REAL(r8k) :: thvv = 0.0_r8k
    ! Cumulative fission gas release fraction
    REAL(r8k) :: tfgfr = 0.0_r8k
    ! Cumulative helium release fraction
    REAL(r8k) :: thefr = 0.0_r8k
    ! Cumulative water vapor release fraction
    REAL(r8k) :: th2ofr = 0.0_r8k
    ! Cumulative nitrogen release fraction
    REAL(r8k) :: tn2fr = 0.0_r8k
    ! Pellet average temperature
    REAL(r8k) :: tpa = 0.0_r8k
    ! Plenum temperature (F)
    REAL(r8k) :: tplen = 0.0_r8k
    ! Temperature above which no cracking is assumed (K). Set to 80% of tsint
    REAL(r8k) :: transt = 0.0_r8k
    ! Clad thermal expansion coefficient in radial direction (in/in-F)
    REAL(r8k) :: txa = 0.0_r8k
    ! Clad thermal expansion coefficient in axial direction (in/in-F)
    REAL(r8k) :: txal = 0.0_r8k
    ! Cladding thermal expansion coefficient (1/F)
    REAL(r8k) :: txc = 0.0_r8k
    ! Gas viscosity used in plenum temperature calculation
    REAL(r8k) :: visc = 0.0_r8k
    ! Volume of pellet annulus
    REAL(r8k) :: vplt = 0.0_r8k
    ! Average bulk coolant temperature at node j (F)
    REAL(r8k) :: wt = 0.0_r8k
    ! Initial moles of xenon in the rod
    REAL(r8k) :: xein = 0.0_r8k
    ! ZrO2 Weight Gain
    REAL(r8k) :: zro2wg = 0.0_r8k
    ! Average claddimg temperature
    REAL(r8k) :: cladavgtemp = 0.0_r8k
    ! Axial average centerline temperature (F)
    REAL(r8k) :: tpca = 0.0_r8k
    ! Axial averaged interface temperature (F)
    REAL(r8k) :: tntera = 0.0_r8k
    ! Input effective fast fluence for strength coefficient (neutrons/(m**2))
    REAL(r8k) :: fnck = 0.0_r8k
    ! Input effective fast fluence for strain hardening exponent at time step start (neutrons/(m**2))
    REAL(r8k) :: fncn = 0.0_r8k
    ! Input effective cold work for strength coefficient (unitless ratio of areas)
    REAL(r8k) :: cwkf = 0.0_r8k
    ! Input effective cold work for strain hardening exponent at time step start (unitless ratio of areas)
    REAL(r8k) :: cwnf = 0.0_r8k
    ! Cladding cold work
    REAL(r8k) :: coldwk = 0.0_r8k
    ! Fuel/Cladding friction coefficient for FEA model
    REAL(r8k) :: frcoef = 0.0_r8k
    ! Additional fuel thermal expansion (multiplier)
    REAL(r8k) :: afal = 0.0_r8k
    ! Additional fuel densification factor (multiplier)
    REAL(r8k) :: afdn = 0.0_r8k
    ! Additional fractional gas release factor
    REAL(r8k) :: afgr = 0.0_r8k
    ! Mole fraction of air
    REAL(r8k) :: amfair = 0.0_r8k
    ! Mole fraction of fission gas
    REAL(r8k) :: amffg = 0.0_r8k
    ! Clad texture factor
    REAL(r8k) :: catexf = 0.0_r8k
    ! As-fabricated clad hydrogen content (wt.ppm)
    REAL(r8k) :: chorg = 0.0_r8k
    ! Clad cold work (0.5= SRA, 0.0= RXA)
    REAL(r8k) :: cldwks = 0.2_r8k
    ! Cold plenum length
    REAL(r8k) :: cpl = 0.0_r8k
    ! Creep step duration (Defualt = 10hr)
    REAL(r8k) :: crephr = 0.0_r8k
    ! Constant crud thickness
    REAL(r8k) :: crdt = 0.0_r8k
    ! Crud accumulation rate
    REAL(r8k) :: crdtr = 0.0_r8k
    ! Dish shoulder width
    REAL(r8k) :: dishsd = 0.0_r8k
    ! Peak-to-average power ratio (fa = 1 if iq = 0)
    REAL(r8k) :: fa = 0.0_r8k
    ! Fill gas pressure
    REAL(r8k) :: fgpav = 0.0_r8k
    ! Fuel oxygen-to-metal ratio (Default = 2.0)
    REAL(r8k) :: fotmtl = 0.0_r8k
    ! Chamfer height
    REAL(r8k) :: chmfrh = 0.0_r8k
    ! Chamfer width
    REAL(r8k) :: chmfrw = 0.0_r8k
    ! Dish height
    REAL(r8k) :: hdish = 0.0_r8k
    ! Pellet height
    REAL(r8k) :: hplt = 0.0_r8k
    ! Center to center rod distance
    REAL(r8k) :: pitch = 0.0_r8k
    ! Weight ppm H2O in fuel (wt.ppm)
    REAL(r8k) :: ppmh2o = 0.0_r8k
    ! Weight ppm N2 in fuel (wt. ppm)
    REAL(r8k) :: ppmn2 = 0.0_r8k
    ! Dish radius of curvature
    REAL(r8k) :: rdish = 0.0_r8k
    ! Clad roughness
    REAL(r8k) :: roughc = 0.0_r8k
    ! Fuel roughness
    REAL(r8k) :: roughf = 0.0_r8k
    ! Fision Gas atoms per 100 fissions (Default = 31)
    REAL(r8k) :: sgapf = 0.0_r8k
    ! Reference temperature upon which the stored energy is based (F)
    REAL(r8k) :: tref = 0.0_r8k
    ! Reference temeprature for specified gas fabrication pressure
    REAL(r8k) :: TGasFab = 0.0_r8k
    ! Expected resintering density increase
    REAL(r8k) :: rsntr = 0.0_r8k
    ! Pellet sintering temperature
    REAL(r8k) :: tsint = 0.0_r8k
    ! Input grain size (effective diameter) of the fuel (Default = 10.0 microns)
    REAL(r8k) :: grnsize = 0.0_r8k
    ! Boron-10 enrichment (atom %) in ZrB2
    REAL(r8k) :: b10 = 0.0_r8k
    ! ZrB2 density
    REAL(r8k) :: zrb2den
    ! Input total densification from previous time step (%) (For radial region l, axial node j)
    REAL(r8k) :: prvden
    ! Percent IFBA rods in core (%)
    REAL(r8k) :: ifba
    ! Fuel melting temperature
    REAL(r8k) :: ftmelt
    ! Cladding melting temperture
    REAL(r8k) :: ctmelt
    ! Variation in fuel melting temperature
    REAL(r8k) :: fdelta
    ! Rod average burnup for node j (MWs/kgU)
    REAL(r8k) :: bup
    ! Input average oxygen concentration excluding oxide layer - average oxygen concentration of
    ! as-received cladding  (kg oxygen/kg zircaloy) [Not used]
    REAL(r8k) :: deloxy
    ! Previous oxide thickness, m
    REAL(r8k) :: zro2i
    ! Current oxide thickness, m
    REAL(r8k) :: zro2o
    ! ZrO2 thermal conductivity
    REAL(r8k) :: zoxk
    ! Excess hydrogen in cladding
    REAL(r8k) :: excesh2
    ! Fraction of theoretical density (= 0.01 * den)
    REAL(r8k) :: frden
    ! Total length of active fuel
    REAL(r8k) :: totl
    ! Gas release fraction per axial node per cumulative time
    REAL(r8k) :: rdot
    ! As-Fabricated Free Volume (in3)
    REAL(r8k) :: cvv
    ! As-fabricated # of moles of gas
    REAL(r8k) :: gmlesAsFab
    ! Released # of moles of gas
    REAL(r8k) :: gmlesReleased
    ! Volume fraction of gases contained in the plenum
    REAL(r8k) :: vfrcpl
    ! Plenum temperature (K)
    REAL(r8k) :: tplens
    ! User supplied swelling limit (vol fraction) (Default = 0.05)
    REAL(r8k) :: slim
    ! User supplied gap heat transfer coefficient multiplier
    REAL(r8k) :: gaphtcmult
    ! Problem Time (s) to use for a restart calculation
    REAL(r8k) :: RestartTime
    ! Uncertainty options
    ! Bias on fuel thermal conductivity model (# of standard deviations)
    REAL(r8k) :: sigftc
    ! Bias on fuel thermal expansion model (# of standard deviations)
    REAL(r8k) :: sigftex
    ! Bias on fission gas release model (# of standard deviations)
    REAL(r8k) :: sigfgr
    ! Bias on fuel swelling model (# of standard deviations)
    REAL(r8k) :: sigswell
    ! Bias on cladding creep model (# of standard deviations)
    REAL(r8k) :: sigcreep
    ! Bias on cladding axial growth model (# of standard deviations)
    REAL(r8k) :: siggro
    ! Bias on cladding corrosion model (# of standard deviations)
    REAL(r8k) :: sigcor
    ! Bias on cladding hydrogen pickup model (# of standard deviations)
    REAL(r8k) :: sigh2
    ! The new timestep value to use for the calculation. (days) Used when TimeIntegration = 1 or 2
    REAL(r8k) :: newtimestep
    ! Flag to specify when to stop the oxidation calcualtion
    REAL(r8k) :: stopox
    ! Fuel open porosity fraction (%TD)
    REAL(r8k) :: deng
    ! As-fabricated apparent fuel density (%TD)
    REAL(r8k) :: den
    ! Molar fraction of helium
    REAL(r8k) :: amfhe = 1.0_r8k
    ! Molar fraction of hydrogen
    REAL(r8k) :: amfh2 = 0.0_r8k
    ! Molar fraction of nitrogen
    REAL(r8k) :: amfn2 = 0.0_r8k
    ! Molar fraction of argon
    REAL(r8k) :: amfarg = 0.0_r8k
    ! Molar fraction of krypton
    REAL(r8k) :: amfkry = 0.0_r8k
    ! Molar fraction of xenon
    REAL(r8k) :: amfxe = 0.0_r8k
    ! Molar fraction of water
    REAL(r8k) :: amfh2o = 0.0_r8k
    ! Fuel pellet Pu-239 content
    REAL(r8k) :: enrpu39 = 0.0_r8k
    ! Fuel pellet Pu-240 content
    REAL(r8k) :: enrpu40 = 0.0_r8k
    ! Fuel pellet Pu-241 content
    REAL(r8k) :: enrpu41 = 0.0_r8k
    ! Fuel pellet Pu-242 content
    REAL(r8k) :: enrpu42 = 0.0_r8k
    ! User supplied time for printing debug information
    REAL(r8k) :: DebugTime
    ! Moderator heating fraction
    REAL(r8k) :: modheat
    ! User-supplied value for cladding elastic modulus (Pa)
    REAL(r8k) :: cladelmod
    ! User-supplied value for fuel relocation
    REAL(r8k) :: fuelreloc
    ! User-supplied value for gap recovery
    REAL(r8k) :: gaprecov
    ! Fuel relocation that is added to fuel surface displacement (*.5)
    REAL(r8k) :: relocm_true
    ! Fuel relocation after accounting for gap thermal conductivity
    REAL(r8k) :: relocm_mod
    ! Incrimental time increase during transient solution
    REAL(r8k) :: TimeIncrement
    ! Advanced Time (Time0 + TimeIncrement)
    REAL(r8k) :: Time
    ! Current Time
    REAL(r8k) :: Time0
    ! Maximum fractional change in temperature at any radial node between two successive iterations for convergence
    REAL(r8k) :: tmpac1 = 0.005_r8k
    ! Maximum fractional change in internal fuel rod pressure between two successive iterations for convergance
    REAL(r8k) :: prsacc = 0.005_r8k
    ! Flag to specify whether or not to calculate the oxidation reaction
    LOGICAL :: calcoxide
    ! Indicates whether or not to write an updated FRAPTRAN restart file
    LOGICAL :: updated_restart
    ! Convergence flag on gas pressure iteration
    LOGICAL :: gasflg
    ! Convergence flag on gap and pellet temperature distribution iteration
    LOGICAL :: nconvg
    ! non-convergence index (30 iterations) on gap and pellet temperature distribution iteration
    LOGICAL :: ncont
    ! Debugging flag
    LOGICAL :: Ndebug = .FALSE.
    ! The following is for storing the code ID Information
    ! The code version identifier (should be changed each time changes are made in the code)
    CHARACTER(LEN=*),PARAMETER :: codeid = 'FAST-1.0.002'
    ! The code build date identifier (should be changed each time changes are made in the code)
    CHARACTER(LEN=*), PARAMETER :: buildid = 'Built January 23, 2017'
    ! Stores the title card information
    CHARACTER(LEN=80) :: title
    ! Specifies fuel relocation model
    CHARACTER(LEN=12) :: RelocModel = 'FRAPCON-3.5'
    !
    ! Arrays
    !
    ! Fixed dimension arrays
    ! Mole fraction of gas constituents. Fixed to ngases
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gases
    ! Gap temperature rise used for temperature convergence loop
    REAL(r8k), DIMENSION(31) :: dltgc = 0.0_r8k
    ! Axial-Dependent Arrays, dimensioned (na)
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IgapGapIndex
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IgapIndexOld
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IgapIndexPrevOld
    ! Clad-to-coolant Heat transfer mode
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: HTC_Mode
    ! Total inner cladding surface displacement, mils
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: totinner
    ! Total outer cladding surface displacement, mils
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: totcrl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFluxd
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFluenced
    ! Bulk Coolant Temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BulkCoolantTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dpwxrate
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelletRad
    ! Average cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladAveTemp
    ! Average fuel pellet temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelAveTemp
    ! Average gap temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapAveTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelCentTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: sigeff
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSurfDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladInSurDisp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladInPermDef
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: sigy
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialNodLength
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEffPlasStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RinterfacPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelCladGap
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolantPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PlastStrnep1
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldGapPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladAvTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CreepStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CreepStrain1
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladDiamHot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevCladEffPlasStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevFuelStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HotNodLength
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PermFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StepNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: TotalHgap
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SolidHgap
    
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasHgap
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadHgap
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFlux
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFluence
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelPorosity
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FilmCoefficient
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EstGapDeltaT
    ! Beginning of step OD ZrO2 thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSZrO2Thk_OD
    ! Beginning of step OD oxygen uptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSO2Uptake_OD
    ! Beginning of step OD alpha thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSAlphaThk_OD
    ! End of step OD ZrO2 thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSZrO2Thk_OD
    ! End of step OD oxygen uptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSO2Uptake_OD
    ! End of step OD alpha thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSAlphaThk_OD
    ! Metal-Water reaction energy
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WatrMetlEnrgy
    ! Beginning of step ID oxide thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSZrO2thk_ID
    ! Beginning of step ID oxygen uptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSO2Uptake_ID
    ! Beginning of step ID alpha thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSAlphaThk_ID
    ! End of step ID oxide thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSZrO2thk_ID
    ! End of step ID oxygen uptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSO2Uptake_ID
    ! End of step ID alpha thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSAlphaThk_ID
    ! Equivalent Cladding Reacted (ECR)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ECR
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ZrO2ThkNoAd
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelCondFactor
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Relocation
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RinternalVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RinterfacVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelVolume
    ! Fuel average fraction of theoretical density (current, not as-fab)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fden
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PorosityVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SurfTempOxide
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AnnulusVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gapplot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevOldCoolPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevOldGapPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevOldCladAvTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevOldFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevCreepStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladIrradGrowStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: UniformAxNodStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladH2Concen
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ExcessH2Concen
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevCladStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StartofStepH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StartofStepPickupH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EndofStepPickupH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelTempRestruRad
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldHealedCrackRadius
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StoredEnergy
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HealedCrackRadius
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HotThermalGap
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkArray1
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkArray2
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkArray3
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladInSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladOutSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Power
    ! Power generated in the cladding
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladdingPower
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gpthe
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gpth
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qc
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: totdef
    ! Input fuel burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: buin
    ! Inner cladding diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dci
    ! Outer cladding diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dco
    ! As-fabricated cladding thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: thkcld
    ! Diametral gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cdg
    ! As-fabricated gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: thkgap
    ! Hydraulic diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: de
    ! Axial node length (ft)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltaz
    ! Axial node elevation (ft)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxNodElevat
    ! PuO2 weight percent if MOX fuel (wt%)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: comp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ctmax
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: flux
    ! End-node to plenum heat transfer fraction (Default = 0.03). 1 value per power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qend
    ! Axial crud thickness multiplier
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: crudmult
    ! Crud thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: crdtt
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rdotwrt
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FDItave
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: creapratearray
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapCond
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fuelexptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fuelswltot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fuelcreeptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fueldentot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fuelburntot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cladcrptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gapHTC
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oxidelayer
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gapmech
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gapthrm
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fuelrelmod
    ! Void volume per unit temperature at axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: NodalMoles
    ! Moles at each axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: NodalGMLES
    ! Relocation strain (unitless) (1/2 is added to the radial strain as permanent outward strain)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rlcstrn
    ! Relocation strain at previous timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rlcstrnold
    ! Radius of the fuel pellet central annulus (in)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rc
    ! Gadolinia content at each axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gadoln
    ! ZrB2 thickness at each axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ZrB2thick
    ! Net permanent fuel displacement due to swelling and densification
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: colddef
    ! Net cladding displacement due to creep/plastic deformation
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: colddec
    ! Rod power at node j
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qnode
    ! Fuel U-235 enrichment (at%)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: enrch
    ! Cladding strain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: epsav
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: wimsburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldwimsburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: stold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: stnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltimeold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltimenew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: sagold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: sagnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: delsagold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: delsagnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: delst
    !
    ! ***These arrays are dimensioned (na, 2)***
    !
    ! Nitrogen release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: angr
    ! Water release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ah2ogr
    ! Fission gas production for each node (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: fgmgp
    ! Cumulative fission gas release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: fmgr
    ! Cumulative helium gas production per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: hemgp
    ! Cumulative helium release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: hmgr
    ! Cumulative nitrogen concentration in fuel per power step (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ang
    ! Cumulative water concentration in fuel per power step (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ah2og
    !
    ! ***These arrays are dimensioned (na, 3)***
    !
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: epp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: eppp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: eppsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: eps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: feps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: reps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: repsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: repsp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rfeps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rfepp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rfpsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: sig
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: epp1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: sig1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: eps1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ThermalStrain
    ! ANS 5.4 2011 axial release fractions array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RB_axial
    ! ANS 5.4 2011 total rod release fractions array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RB_rod
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrc
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrco
    !
    ! ***Time-Dependent***
    !
    ! # of qf, x pairs for each axial power shape
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jn
    ! # of cladt, xt pairs for each axial temperature distribution
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jnsurftemp
    ! Maximum power node for each power shape
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jpeak
    ! Sequential # of the power shape to be used for each timestep
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jst
    ! Sequential # of the cladding temperature profile to be used for each timestep
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jstsurftemp
    ! Keeps track of the cycle steps, used for increased # of timestep runs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: CycleSteps
    ! Average of the q'' for each axial power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: avgqi
    ! Coolant mass flux around fuel rod, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: go
    ! Coolant System Pressure, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: p2
    ! Cumulative time at the end of each timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ProblemTime
    ! Cumulative time at the end of each timestep. Set equal to problemtime when timestep reduction is used
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ProblemTime_Prev
    ! Specify the time step history to be used in transient conduction solution calculations (time step size(s), problem time(s))
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtmaxa
    ! Specify the interval of problem time between output file printouts. Used only during transient solution
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtpoa     ! (time step size(s), print interval(s))
    ! Specify the interval of problem time between plot file printouts. Used only during transient solution
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtplta    ! (time step size(s), print interval(s))
    ! LHGR at each timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qmpy
    ! Coolant inlet temperature, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tw
    ! Rod internal pressure for each time step for FEA model
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: p1
    ! Cumulative fission gas release
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acmfg
    ! Cumulative helium release
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acmhe
    ! Cumulative hydrogen release
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acmH2
    ! Cumulative water vapor release
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acmH2O
    ! Cumulative nitrogen release
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: acmn2
    ! Cumulative fission gas produced
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: amgpt
    ! Gram-moles of gas in rod
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gasmo
    ! Cumulative helium production (gram-moles)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hmgpt
    ! Radial and axial fuel averaged temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tafa
    ! Radial and axial gap averaged temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: taga
    ! Axial averaged fuel surface temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tsfa
    ! Radial and axial averaged cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: taca
    ! Peak power node average temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkPelAveTemp
    ! Peak power node power
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkPower
    ! Peak power node average cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkAveCladTemp
    ! Peak power node gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkGap
    ! Peak power node fuel surface temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkPelSurfTemp
    ! Peak power node fuel centerline temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkPelCentTemp
    ! Peak power node hydrogen pickup
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkH2up
    ! Peak power node fuel/clad interfacial pressure
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkIntefacePres
    ! Peak power node inner surface cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkIDCladTemp
    ! Peak power node outer surface cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkODCladTemp
    ! Rod internal gas pressure
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pit
    ! Peak power node cladding hoop stress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkHoopStres
    ! Peak power node cladding axial stress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkAxlStres
    ! Peak power node burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkBurnup
    ! Peak power node cladding hoop strain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkHoopStrain
    ! Peak power node fuel pellet OD
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkFuelPelOD
    ! Peak power node gap conductance
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkGapCond
    ! Peak power node oxide thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pkZrO2
    ! Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1.
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: zcool
    ! Bulk coolant temperature at each axial node & timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tcoolant
    ! Bulk coolant pressure at each axial node & timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pcoolant
    ! Elevation in each qf, x array defining a power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: x
    ! Ratio of linear power at x(n) elevation to axially average value for each M-th power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qf
    ! Elevation in each cladt, xt array defining a cladding temperature profile
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: xt
    ! Cladding surface temperature at xt(n) elevation for each M-th power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cladt
    ! Additional gmles added to the total # of gmles released from the fuel
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: addgmles
    ! Additional swelling added to the fuel pellet
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: addswell
    ! Time increment, used in Subroutine ans54
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dt
    ! Used in Subroutine totgas
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: he_ifba
    !
    ! *** Radial-Dependent Arrays, dimensioned (nr)***
    !
    ! Fuel radial temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tfuelr
    ! Fuel radial temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tfuelr2
    ! Fuel ring temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tfring
    ! Reversed array to match nodaliztion in fueltp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rrapow
    !
    ! 2-D Arrays
    !
    ! Array of material indexes
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: imaterials
    ! User-defined coolant temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: coolanttemp
    ! User-defined coolant pressure
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: coolantpressure
    ! User-defined cladding temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: cladtarray
    ! Burnup array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: buarray
    ! Strain array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: strainarray
    ! Fuel swelling rate array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dpwxarray
    ! Cladding creep rate array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: creaparray
    ! Dummy array used in inital
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dumarray3
    ! 2-D array (axial,radial) of normalized form factors, correlated to the radial boundaries. (dimensionless).
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: formf
    ! Fuel volume per increment + ring (in^3)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ringvol
    ! Cold ring length (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: coldringl
    ! Porosity left at beginning of power step (m/m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: porosold
    ! Porosity left after power step (m/m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: porosnew
    ! Fuel mesh point radii array (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rrev
    ! Normalized axial power profile
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: qaxnorm
    ! Cold fuel ring radius (m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: crad
    ! Hot fuel ring radius (m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: hrad
    ! Normalized radial power profile array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: rapow
    ! Thermal expansion of fuel ring (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: uo2exp
    ! Fuel sweling (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dpw
    ! Fuel swelling at previous timestep (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dpwpp
    ! Fuel creep (in/in) [Not used]
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dpw2
    ! Fuel creep at previous timestep (in/in) [Not used]
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dpwpp2
    ! Fuel densification (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: densf
    ! Fuel densification at previous timestep (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: densp
    ! Hot pellet ring length (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: hotringl
    ! Fuel temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tmpfuel
    ! Gas available for transient FGR
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: TR_gasavail
    !
    ! 3-D Arrays
    !
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: prdct
    ! Nodal burnup, (MWd/mtU)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: brnup3
    ! Diffusion parameter, used in Subroutine ans54 (im,ngasr,na)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: ansd
    !
    ! Spring Data
    TYPE SpringProp
        REAL(r8k) :: vs = 0.0_r8k           ! Number of spring turns
        REAL(r8k) :: dspg = 0.0_r8k         ! Spring diameter
        REAL(r8k) :: dspgw = 0.0_r8k        ! Spring wire diameter
        REAL(r8k) :: Vcold = 0.0_r8k        ! Hot volume
        REAL(r8k) :: Vhot = 0.0_r8k         ! Cold Volume
        REAL(r8k) :: Vf = 0.0_r8k           ! (Plenum) Volume fraction occupied by spring
    END TYPE SpringProp
    !
    TYPE (SpringProp), SAVE :: Spring
    !
    CONTAINS
    !
    SUBROUTINE Set_Defaults
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This subroutine sets default values for scalars
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/19/2015
    nvoid = 0_ipk
    rdot = 0.0_r8k
    gaph = 1.0e-2_r8k
    gapt = 1.0e-2_r8k
    fdelta = 0.0_r8k
    ctmelt = 1.0_r8k
    deloxy = 0.0_r8k
    fnck = 0.0_r8k
    fncn = 0.0_r8k
    cwnf = 0.0_r8k
    !
    END SUBROUTINE Set_Defaults
    !
    !
    !
    SUBROUTINE Allocate_Time_Arrays
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the time-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (qend(1:im+1))
    ALLOCATE (avgqi(1:im+1))
    ALLOCATE (go(1:im+1))
    ALLOCATE (p2(1:im+1))
    ALLOCATE (ProblemTime(0:im+1))
    ALLOCATE (ProblemTime_Prev(0:im+1))
    ALLOCATE (dtmaxa(1:2*(im+4)))
    ALLOCATE (dtpoa(1:2*(im+4)))
    ALLOCATE (dtplta(1:2*(im+4)))
    ALLOCATE (qmpy(0:im+1))
    ALLOCATE (tw(1:im+1))
    ALLOCATE (p1(1:im+1))
    ALLOCATE (jn(1:im+1))
    ALLOCATE (jnsurftemp(1:im+1))
    ALLOCATE (jpeak(1:im+1))
    ALLOCATE (jst(1:im+1))
    ALLOCATE (jstsurftemp(1:im+1))
    ALLOCATE (acmfg(1:im+1))
    ALLOCATE (acmhe(1:im+1))
    ALLOCATE (acmH2(1:im+1))
    ALLOCATE (acmH2O(1:im+1))
    ALLOCATE (acmn2(1:im+1))
    ALLOCATE (amgpt(1:im+1))
    ALLOCATE (gasmo(0:im+1))
    ALLOCATE (hmgpt(1:im+1))
    ALLOCATE (tafa(1:im+1))
    ALLOCATE (taga(1:im+1))
    ALLOCATE (tsfa(1:im+1))
    ALLOCATE (taca(0:im+1))
    ALLOCATE (pkODCladTemp(0:im+1))
    ALLOCATE (pkPelAveTemp(0:im+1))
    ALLOCATE (pkPower(0:im+1))
    ALLOCATE (pkAveCladTemp(0:im+1))
    ALLOCATE (pkIDCladTemp(0:im+1))
    ALLOCATE (pkGap(0:im+1))
    ALLOCATE (pkPelSurfTemp(0:im+1))
    ALLOCATE (pkH2up(0:im+1))
    ALLOCATE (pkPelCentTemp(0:im+1))
    ALLOCATE (pkIntefacePres(0:im+1))
    ALLOCATE (pit(0:im+1))
    ALLOCATE (pkHoopStres(0:im+1))
    ALLOCATE (pkAxlStres(0:im+1))
    ALLOCATE (pkBurnup(0:im+1))
    ALLOCATE (pkHoopStrain(0:im+1))
    ALLOCATE (pkFuelPelOD(0:im+1))
    ALLOCATE (pkGapCond(0:im+1))
    ALLOCATE (pkZrO2(0:im+1))
    ALLOCATE (dt(1:im+2))
    ALLOCATE (he_ifba(1:im+1))
    ALLOCATE (addgmles(1:im+1))
    ALLOCATE (addswell(1:im+1))
    ALLOCATE (CycleSteps(1:im+1))
    !
    ! Assign a value to each array
    !
    qend = 0.0_r8k
    avgqi = 0.0_r8k
    go = 0.0_r8k
    p2 = 0.0_r8k
    ProblemTime = 0.0_r8k
    ProblemTime_Prev = 0.0_r8k
    dtmaxa = 0.0_r8k
    dtpoa = 0.0_r8k
    dtplta = 0.0_r8k
    qmpy = 0.0_r8k
    tw = 0.0_r8k
    p1 = 0.0_r8k
    jn = 0_ipk
    jnsurftemp = 0_ipk
    jpeak = 0_ipk
    jst = 0_ipk
    acmfg = 0.0_r8k
    acmhe = 0.0_r8k
    acmH2 = 0.0_r8k
    acmH2O = 0.0_r8k
    acmn2 = 0.0_r8k
    amgpt = 0.0_r8k
    gasmo = 0.0_r8k
    hmgpt = 0.0_r8k
    tafa = 0.0_r8k
    taga = 0.0_r8k
    tsfa = 0.0_r8k
    taca = 0.0_r8k
    pkODCladTemp = 0.0_r8k
    pkPelAveTemp = 0.0_r8k
    pkPower = 0.0_r8k
    pkAveCladTemp = 0.0_r8k
    pkIDCladTemp = 0.0_r8k
    pkGap = 0.0_r8k
    pkPelSurfTemp = 0.0_r8k
    pkH2up = 0.0_r8k
    pkPelCentTemp = 0.0_r8k
    pkIntefacePres = 0.0_r8k
    pit = 0.0_r8k
    pkHoopStres = 0.0_r8k
    pkAxlStres = 0.0_r8k
    pkBurnup = 0.0_r8k
    pkHoopStrain = 0.0_r8k
    pkFuelPelOD = 0.0_r8k
    pkGapCond = 0.0_r8k
    pkZrO2 = 0.0_r8k
    dt = 0.0_r8k
    jstsurftemp = 0
    he_ifba = 0.0_r8k
    addgmles = 0.0_r8k
    addswell = 0.0_r8k
    CycleSteps = 0_ipk
    !
    END SUBROUTINE Allocate_Time_Arrays
    !
    !
    SUBROUTINE Allocate_Axial_Arrays
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the axial-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (HTC_Mode(1:na+1))
    ALLOCATE (BulkCoolantTemp(1:na+1))
    ALLOCATE (dpwxrate(1:na))
    ALLOCATE (PelletRad(1:na))
    ALLOCATE (FastFluxd(1:na))
    ALLOCATE (FastFluenced(1:na))
    ALLOCATE (totinner(1:na))
    ALLOCATE (totcrl(1:na))
    ALLOCATE (CladAveTemp(1:na))
    ALLOCATE (PelAveTemp(1:na))
    ALLOCATE (GapAveTemp(1:na))
    ALLOCATE (PelSurfTemp(1:na))
    ALLOCATE (PelCentTemp(1:na))
    ALLOCATE (sigeff(1:na))
    ALLOCATE (FuelSurfDispl(1:na))
    ALLOCATE (CladInSurDisp(1:na))
    ALLOCATE (CladInPermDef(1:na))
    ALLOCATE (sigy(1:na))
    ALLOCATE (AxialNodLength(1:na))
    ALLOCATE (CladEffPlasStrain(1:na))
    ALLOCATE (RinterfacPress(1:na))
    ALLOCATE (FuelCladGap(1:na))
    ALLOCATE (GapPress(1:na))
    ALLOCATE (CoolantPress(1:na))
    ALLOCATE (PlastStrnep1(1:na))
    ALLOCATE (OldCladStrn(1:na))
    ALLOCATE (OldFuelStrn(1:na))
    ALLOCATE (OldGapPress(1:na))
    ALLOCATE (OldCoolPress(1:na))
    ALLOCATE (OldFuelDispl(1:na))
    ALLOCATE (OldCladAvTemp(1:na))
    ALLOCATE (CreepStrain(1:na))
    ALLOCATE (CreepStrain1(1:na))
    ALLOCATE (CladDiamHot(1:na))
    ALLOCATE (PrevCladEffPlasStrn(1:na))
    ALLOCATE (PrevFuelStrain(1:na))
    ALLOCATE (HotNodLength(1:na))
    ALLOCATE (PermFuelDispl(1:na))
    ALLOCATE (BOSNodeburnup(1:na))
    ALLOCATE (EOSNodeburnup(1:na))
    ALLOCATE (StepNodeburnup(1:na))
    ALLOCATE (TotalHgap(1:na))
    ALLOCATE (SolidHgap(1:na))
    ALLOCATE (GasHgap(1:na))
    ALLOCATE (RadHgap(1:na))
    ALLOCATE (FastFlux(1:na))
    ALLOCATE (FastFluence(1:na))
    ALLOCATE (FuelPorosity(1:na))
    ALLOCATE (FilmCoefficient(1:na))
    ALLOCATE (EstGapDeltaT(1:na))
    ALLOCATE (BOSZrO2Thk_OD(1:na))
    ALLOCATE (BOSO2Uptake_OD(1:na))
    ALLOCATE (BOSAlphaThk_OD(1:na))
    ALLOCATE (EOSZrO2Thk_OD(1:na))
    ALLOCATE (EOSO2Uptake_OD(1:na))
    ALLOCATE (EOSAlphaThk_OD(1:na))
    ALLOCATE (WatrMetlEnrgy(1:na))
    ALLOCATE (BOSZrO2Thk_ID(1:na))
    ALLOCATE (BOSO2Uptake_ID(1:na))
    ALLOCATE (BOSAlphaThk_ID(1:na))
    ALLOCATE (EOSZrO2Thk_ID(1:na))
    ALLOCATE (EOSO2Uptake_ID(1:na))
    ALLOCATE (EOSAlphaThk_ID(1:na))
    ALLOCATE (ECR(1:na))
    ALLOCATE (ZrO2ThkNoAd(1:na))
    ALLOCATE (FuelCondFactor(1:na))
    ALLOCATE (Relocation(1:na))
    ALLOCATE (RinternalVolume(1:na))
    ALLOCATE (CladVolume(1:na))
    ALLOCATE (CrackVolume(1:na))
    ALLOCATE (RinterfacVolume(1:na))
    ALLOCATE (FuelVolume(1:na))
    ALLOCATE (fden(1:na))
    ALLOCATE (GapVolume(1:na))
    ALLOCATE (PorosityVolume(1:na))
    ALLOCATE (SurfTempOxide(1:na))
    ALLOCATE (AnnulusVolume(1:na))
    ALLOCATE (gapplot(1:na))
    ALLOCATE (PrevOldCoolPress(1:na))
    ALLOCATE (PrevOldGapPress(1:na))
    ALLOCATE (PrevOldCladAvTemp(1:na))
    ALLOCATE (PrevOldFuelDispl(1:na))
    ALLOCATE (PrevCreepStrain(1:na))
    ALLOCATE (CladIrradGrowStrn(1:na))
    ALLOCATE (UniformAxNodStrn(1:na))
    ALLOCATE (CladH2Concen(1:na))
    ALLOCATE (ExcessH2Concen(1:na))
    ALLOCATE (PrevCladStrain(1:na))
    ALLOCATE (StartofStepH2Con(1:na))
    ALLOCATE (StartofStepPickupH2Con(1:na))
    ALLOCATE (EndofStepPickupH2Con(1:na))
    ALLOCATE (FuelTempRestruRad(1:na))
    ALLOCATE (OldHealedCrackRadius(1:na))
    ALLOCATE (StoredEnergy(1:na))
    ALLOCATE (HealedCrackRadius(1:na))
    ALLOCATE (HotThermalGap(1:na))
    ALLOCATE (WorkArray1(1:na))
    ALLOCATE (WorkArray2(1:na))
    ALLOCATE (WorkArray3(1:na))
    ALLOCATE (CladInSurfTemp(1:na))
    ALLOCATE (CladOutSurfTemp(1:na))
    ALLOCATE (Power(1:na))
    ALLOCATE (CladdingPower(1:na))
    ! Note: 30 is the number of iterations on the gap convergence
    ALLOCATE (gpthe(1:30))
    ALLOCATE (gpth(1:30))
    ALLOCATE (qc(1:na))
    ALLOCATE (totdef(1:na))
    ALLOCATE (IgapGapIndex(1:na))
    ALLOCATE (IgapIndexOld(1:na))
    ALLOCATE (IgapIndexPrevOld(1:na))
    ALLOCATE (buin(1:na))
    ALLOCATE (dci(1:na))
    ALLOCATE (dco(1:na))
    ALLOCATE (cdg(1:na))
    ALLOCATE (thkcld(1:na))
    ALLOCATE (thkgap(1:na))
    ALLOCATE (de(1:na))
    ALLOCATE (deltaz(1:na))
    ALLOCATE (AxNodElevat(0:na))
    ALLOCATE (comp(1:na))
    ALLOCATE (ctmax(1:na))
    ALLOCATE (dp(1:na))
    ALLOCATE (flux(1:na))
    ALLOCATE (crudmult(1:na))
    ALLOCATE (crdtt(1:na))
    ALLOCATE (rdotwrt(1:na))
    ALLOCATE (FDItave(1:na))
    ALLOCATE (creapratearray(1:na))
    ALLOCATE (GapCond(1:na))
    ALLOCATE (fuelexptot(1:na))
    ALLOCATE (fuelswltot(1:na))
    ALLOCATE (fuelcreeptot(1:na))
    ALLOCATE (fueldentot(1:na))
    ALLOCATE (fuelburntot(1:na))
    ALLOCATE (cladcrptot(1:na))
    ALLOCATE (gapHTC(1:na))
    ALLOCATE (oxidelayer(1:na))
    ALLOCATE (gapmech(1:na))
    ALLOCATE (gapthrm(1:na))
    ALLOCATE (fuelrelmod(1:na))
    ALLOCATE (zcool(1:na))
    ALLOCATE (tcoolant(1:naxim))
    ALLOCATE (pcoolant(1:naxim))
    ALLOCATE (x(1:naxim))
    ALLOCATE (qf(1:naxim))
    ALLOCATE (xt(1:naxim))
    ALLOCATE (cladt(1:naxim))
    ALLOCATE (NodalMoles(1:na))
    ALLOCATE (NodalGMLES(1:na))
    ALLOCATE (rlcstrn(1:na))
    ALLOCATE (rlcstrnold(1:na))
    ALLOCATE (rc(1:na))
    ALLOCATE (gadoln(1:na-1))
    ALLOCATE (ZrB2thick(1:na-1))
    ALLOCATE (stold(1:na))
    ALLOCATE (stnew(1:na))
    ALLOCATE (deltimeold(1:na))
    ALLOCATE (deltimenew(1:na))
    ALLOCATE (sagold(1:na))
    ALLOCATE (sagnew(1:na))
    ALLOCATE (delsagold(1:na))
    ALLOCATE (delsagnew(1:na))
    ALLOCATE (delst(1:na))
    ALLOCATE (colddef(1:na))
    ALLOCATE (colddec(1:na))
    ALLOCATE (qnode(1:na))
    ALLOCATE (enrch(1:na-1))
    ALLOCATE (epsav(1:na))
    ALLOCATE (wimsburnup(1:na))
    ALLOCATE (oldwimsburnup(1:na))
    !
    ! Assign a value to each array
    !
    totinner = 0.0_r8k
    totcrl = 0.0_r8k
    HTC_Mode = 0_ipk
    BulkCoolantTemp = 0.0_r8k
    PelletRad = 0.0_r8k
    dpwxrate = 0.0_r8k
    FastFluxd = 0.0_r8k
    FastFluenced = 0.0_r8k
    CladAveTemp = 0.0_r8k!500.0_r8k
    PelAveTemp = 0.0_r8k
    GapAveTemp = 0.0_r8k
    PelSurfTemp = 0.0_r8k
    PelCentTemp = 0.0_r8k
    sigeff = 0.0_r8k
    FuelSurfDispl = 0.0_r8k
    CladInSurDisp = 0.0_r8k
    CladInPermDef = 0.0_r8k
    sigy = 0.0_r8k
    AxialNodLength = 0.0_r8k
    CladEffPlasStrain = 0.0_r8k
    RinterfacPress = 0.0_r8k
    FuelCladGap = 0.0_r8k
    GapPress = 0.0_r8k
    CoolantPress = 0.0_r8k
    PlastStrnep1 = 0.0_r8k
    OldCladStrn = 0.0_r8k
    OldFuelStrn = 0.0_r8k
    OldGapPress = 0.0_r8k
    OldCoolPress = 0.0_r8k
    OldFuelDispl = 0.0_r8k
    OldCladAvTemp = 0.0_r8k
    CreepStrain = 0.0_r8k
    CreepStrain1 = 0.0_r8k
    CladDiamHot = 0.0_r8k
    PrevCladEffPlasStrn = 0.0_r8k
    PrevFuelStrain = 0.0_r8k
    HotNodLength = 0.0_r8k
    PermFuelDispl = 0.0_r8k
    BOSNodeburnup = 0.0_r8k
    EOSNodeburnup = 0.0_r8k
    StepNodeburnup = 0.0_r8k
    TotalHgap = 0.0_r8k
    SolidHgap = 0.0_r8k
    GasHgap = 0.0_r8k
    RadHgap = 0.0_r8k
    FastFlux = 0.0_r8k
    FastFluence = 0.0_r8k
    FuelPorosity = 0.0_r8k
    FilmCoefficient = 0.0_r8k
    EstGapDeltaT = 0.0_r8k
    BOSZrO2Thk_OD = 0.0_r8k
    BOSO2Uptake_OD = 0.0_r8k
    BOSAlphaThk_OD = 0.0_r8k
    EOSZrO2Thk_OD = 0.0_r8k
    EOSO2Uptake_OD = 0.0_r8k
    EOSAlphaThk_OD = 0.0_r8k
    WatrMetlEnrgy = 0.0_r8k
    BOSZrO2Thk_ID = 0.0_r8k
    BOSO2Uptake_ID = 0.0_r8k
    BOSAlphaThk_ID = 0.0_r8k
    EOSZrO2Thk_ID = 0.0_r8k
    EOSO2Uptake_ID = 0.0_r8k
    EOSAlphaThk_ID = 0.0_r8k
    ECR = 0.0_r8k
    ZrO2ThkNoAd = 0.0_r8k
    FuelCondFactor = 0.0_r8k
    Relocation = 0.0_r8k
    RinternalVolume = 0.0_r8k
    CladVolume = 0.0_r8k
    CrackVolume = 0.0_r8k
    RinterfacVolume = 0.0_r8k
    FuelVolume = 0.0_r8k
    fden = 0.0_r8k
    GapVolume = 0.0_r8k
    PorosityVolume = 0.0_r8k
    SurfTempOxide = 0.0_r8k
    AnnulusVolume = 0.0_r8k
    gapplot = 0.0_r8k
    PrevOldCoolPress = 0.0_r8k
    PrevOldGapPress = 0.0_r8k
    PrevOldCladAvTemp = 0.0_r8k
    PrevOldFuelDispl = 0.0_r8k
    PrevCreepStrain = 0.0_r8k
    CladIrradGrowStrn = 0.0_r8k
    UniformAxNodStrn = 0.0_r8k
    CladH2Concen = 10.0_r8k
    ExcessH2Concen = 0.0_r8k
    PrevCladStrain = 0.0_r8k
    StartofStepH2Con = 10.0_r8k
    StartofStepPickupH2Con = 0.0_r8k
    EndofStepPickupH2Con = 0.0_r8k
    FuelTempRestruRad = 0.0_r8k
    OldHealedCrackRadius = 0.0_r8k
    StoredEnergy = 0.0_r8k
    HealedCrackRadius = 0.0_r8k
    HotThermalGap = 0.0_r8k
    WorkArray1 = 0.0_r8k
    WorkArray2 = 0.0_r8k
    WorkArray3 = 0.0_r8k
    CladInSurfTemp = 0.0_r8k
    CladOutSurfTemp = 0.0_r8k
    Power = 0.0_r8k
    CladdingPower = 0.0_r8k
    gpthe = 0.0_r8k
    gpth = 0.0_r8k
    qc = 0.0_r8k
    totdef = 0.0_r8k
    IgapGapIndex = 0.0_r8k
    IgapIndexOld = 0.0_r8k
    IgapIndexPrevOld = 0.0_r8k
    buin = 0.0_r8k
    dci = 0.0_r8k
    dco = -1.0_r8k
    cdg = 0.0_r8k
    thkcld = -1.0_r8k
    thkgap = -1.0_r8k
    de = 0.0_r8k
    deltaz = 0.0_r8k
    AxNodElevat = 0.0_r8k
    comp = -1.0_r8k
    comp(1) = 0.0_r8k
    ctmax = 0.0_r8k
    dp = 0.0_r8k
    flux = 0.0_r8k
    crudmult = 0.0_r8k
    crdtt = 0.0_r8k
    rdotwrt = 0.0_r8k
    FDItave = 0.0_r8k
    creapratearray = 0.0_r8k
    GapCond = 0.0_r8k
    fuelexptot = 0.0_r8k
    fuelswltot = 0.0_r8k
    fuelcreeptot = 0.0_r8k
    fueldentot = 0.0_r8k
    fuelburntot = 0.0_r8k
    cladcrptot = 0.0_r8k
    gapHTC = 0.0_r8k
    oxidelayer = 0.0_r8k
    gapmech = 0.0_r8k
    gapthrm = 0.0_r8k
    fuelrelmod = 0.0_r8k
    zcool = 0.0_r8k
    tcoolant = 0.0_r8k
    pcoolant = 0.0_r8k
    x = 0.0_r8k
    qf = 0.0_r8k
    xt = 0.0_r8k
    cladt = 0.0_r8k
    NodalMoles = 0.0_r8k
    NodalGMLES = 0.0_r8k
    rlcstrn = 0.0_r8k
    rlcstrnold = 0.0_r8k
    rc = 0.0_r8k
    gadoln = 0.0_r8k
    ZrB2thick = 0.0_r8k
    stold = 0.0_r8k
    stnew = 0.0_r8k
    deltimeold = 0.0_r8k
    deltimenew = 0.0_r8k
    sagold = 0.0_r8k
    sagnew = 0.0_r8k
    delsagold = 0.0_r8k
    delsagnew = 0.0_r8k
    delst = 0.0_r8k
    colddef = 0.0_r8k
    colddec = 0.0_r8k
    qnode = 0.0_r8k
    enrch = 0.0_r8k
    epsav = 0.0_r8k
    wimsburnup = 0.0_r8k
    oldwimsburnup = 0.0_r8k
    !
    END SUBROUTINE Allocate_Axial_Arrays
    !
    !
    !
    SUBROUTINE Allocate_Radial_Arrays
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the radial-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (tfuelr(1:nr))
    ALLOCATE (tfuelr2(1:nr))
    ALLOCATE (tfring(1:nr))
    ALLOCATE (rrapow(1:nr))
    !
    tfuelr = 500.0_r8k
    tfuelr2 = 500.0_r8k
    tfring = 0.0_r8k
    rrapow = 0.0_r8k
    !
    END SUBROUTINE Allocate_Radial_Arrays
    !
    !
    !
    SUBROUTINE Allocate_2D_Arrays (N_mesh)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates 2D arrays for various combinations of na, nr and im
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    INTEGER(ipk), INTENT(IN) :: N_mesh
    !
    ALLOCATE (imaterials(1:N_mesh,1:na))
    ALLOCATE (coolanttemp(1:im+1,1:na+1))
    ALLOCATE (coolantpressure(1:im+1,1:na+1))
    ALLOCATE (buarray(1:im+1,1:na))
    ALLOCATE (strainarray(1:im+1,1:na))
    ALLOCATE (dpwxarray(1:im+1,1:na))
    ALLOCATE (creaparray(1:im+1,1:na))
    ALLOCATE (dumarray3(1:im+1,1:na))
    ALLOCATE (formf(1:na,1:nr))
    ALLOCATE (ringvol(1:nr,1:na))
    ALLOCATE (coldringl(1:nr,1:na))
    ALLOCATE (porosold(1:nr,1:na))
    ALLOCATE (porosnew(1:nr,1:na))
    ALLOCATE (cladtarray(1:im+1,1:na+1))
    ALLOCATE (rrev(1:nr,1:na))
    ALLOCATE (qaxnorm(1:na,1:im+1))
    ALLOCATE (crad(1:nr,1:na))
    ALLOCATE (hrad(1:nr,1:na))
    ALLOCATE (rapow(1:nr,1:na))
    ALLOCATE (uo2exp(1:nr,1:na))
    ALLOCATE (dpw(1:nr,1:na))
    ALLOCATE (dpwpp(1:nr,1:na))
    ALLOCATE (dpw2(1:nr,1:na))
    ALLOCATE (dpwpp2(1:nr,1:na))
    ALLOCATE (densf(1:nr,1:na))
    ALLOCATE (densp(1:nr,1:na))
    ALLOCATE (hotringl(1:nr,1:na))
    ALLOCATE (tmpfuel(1:nr,1:na))
    ALLOCATE (TR_gasavail(1:na,1:ngasr,1:3))
    ALLOCATE (angr(1:na,2))
    ALLOCATE (ah2ogr(1:na,2))
    ALLOCATE (fgmgp(1:na,2))
    ALLOCATE (fmgr(1:na,2))
    ALLOCATE (hemgp(1:na,2))
    ALLOCATE (hmgr(1:na,2))
    ALLOCATE (ang(1:na,2))
    ALLOCATE (ah2og(1:na,2))
    ALLOCATE (epp(1:na,3))
    ALLOCATE (eppp(1:na,3))
    ALLOCATE (eppsv(1:na,3))
    ALLOCATE (eps(1:na,3))
    ALLOCATE (feps(1:na,3))
    ALLOCATE (reps(1:na,3))
    ALLOCATE (repsv(1:na,3))
    ALLOCATE (repsp(1:na,3))
    ALLOCATE (rfeps(1:na,3))
    ALLOCATE (rfepp(1:na,3))
    ALLOCATE (rfpsv(1:na,3))
    ALLOCATE (sig(1:na,3))
    ALLOCATE (epp1(1:na,3))
    ALLOCATE (sig1(1:na,3))
    ALLOCATE (eps1(1:na,3))
    ALLOCATE (ThermalStrain(1:na,3))
    ! Note: 15 is the number of radioactive gases being tracked by ANS-5.4
    ALLOCATE (RB_axial(15,1:na))
    ALLOCATE (RB_rod(15,1:im+1))
    ALLOCATE (radsrc(1:na,1:nmesh))
    ALLOCATE (radsrco(1:na,1:nmesh))
    !
    ! Assign a value to each array
    !
    imaterials = 0
    coolanttemp = 0.0_r8k
    coolantpressure = 0.0_r8k
    buarray = 0.0_r8k
    strainarray = 0.0_r8k
    dpwxarray = 0.0_r8k
    creaparray = 0.0_r8k
    dumarray3 = 0.0_r8k
    formf = 0.0_r8k
    ringvol = 0.0_r8k
    coldringl = 0.0_r8k
    porosold = 0.0_r8k
    porosnew = 0.0_r8k
    cladtarray = 500.0_r8k
    rrev = 0.0_r8k
    qaxnorm = 0.0_r8k
    crad = 0.0_r8k
    hrad = 0.0_r8k
    rapow = 0.0_r8k
    uo2exp = 0.0_r8k
    dpw = 0.0_r8k
    dpwpp = 0.0_r8k
    dpw2 = 0.0_r8k
    dpwpp2 = 0.0_r8k
    densf = 0.0_r8k
    densp = 0.0_r8k
    hotringl = 0.0_r8k
    angr = 0.0_r8k
    ah2ogr = 0.0_r8k
    fgmgp = 0.0_r8k
    fmgr = 0.0_r8k
    hemgp = 0.0_r8k
    hmgr = 0.0_r8k
    ang = 0.0_r8k
    ah2og = 0.0_r8k
    epp = 0.0_r8k
    eppp = 0.0_r8k
    eppsv = 0.0_r8k
    eps = 0.0_r8k
    feps = 0.0_r8k
    reps = 0.0_r8k
    repsv = 0.0_r8k
    repsp = 0.0_r8k
    rfeps = 0.0_r8k
    rfepp = 0.0_r8k
    rfpsv = 0.0_r8k
    sig = 0.0_r8k
    epp1 = 0.0_r8k
    sig1 = 0.0_r8k
    eps1 = 0.0_r8k
    ThermalStrain = 0.0_r8k
    tmpfuel = 0.0_r8k
    TR_gasavail = 0.0_r8k
    RB_axial = 0.0_r8k
    RB_rod = 0.0_r8k
    radsrc = 0.0_r8k
    radsrco = 0.0_r8k
    
    !
    END SUBROUTINE Allocate_2D_Arrays
    !
    !
    !
    SUBROUTINE Allocate_3D_Arrays
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates 3D arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ! FGR Radial Nodes, Axial Nodes, Timesteps
    ALLOCATE (prdct(1:ngasr,1:na,1:im+1))
    ! Axial Nodes, Radial Nodes, 2
    ALLOCATE (brnup3(1:na,1:nr,1:2))
    ! FGR Radial Nodes, Axial Nodes, Timesteps
    ALLOCATE (ansd(1:ngasr,1:na,1:im+1))
    !
    ! Assign a value to each array
    !
    prdct = 0.0_r8k
    brnup3 = 0.0_r8k
    ansd = 0.0_r8k
    !
    END SUBROUTINE Allocate_3D_Arrays
    !
END MODULE Variables