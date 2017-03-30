MODULE Conversions
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This file contains the subroutines used to perform unit conversions and holds the conversion factors used in the code.
    !>@author
    !> Developed by Ian Porter, NRC
    !>@date
    !> April, 2014
    
    PUBLIC
    
    !
    ! ** Burnup **
    !
    REAL(r8k), PARAMETER :: MWskgUtoMWdMTU = 1000.0_r8k / 86400.0_r8k! ((1000 kg * 1 day) / (1 Tonne * 86400s))
    !
    ! ** Constants **
    !
    ! Avogadro's Number
    REAL(r8K), PARAMETER :: Avogadro = 6.0221413E23_r8k              ! (molecules / mol)
    ! Boltzmann's Constant
    REAL(r8k), PARAMETER :: Boltzmann = 1.3806505E-23_r8k            ! (J / K)
    ! Gravitational Acceleration
    REAL(r8K), PARAMETER :: Gravity_SI = 9.80665_r8k                 ! (m / s)
    ! Ideal Gas Constant
    REAL(r8K), PARAMETER :: R_JmolK = 8.3144621_r8k                  ! (J / mol - K)
    ! Ideal Gas Constant
    REAL(r8k), PARAMETER :: R_in3psiRlbmol = 10.73159_r8k            ! (in^3*psi)/(Rankine*lb-mol)
    ! Pi
    REAL(r8k), PARAMETER :: pi = 3.1415926535897932_r8k              ! Value for pi
    ! Speed of Light
    REAL(r8K), PARAMETER :: speedlight = 2.99792458E8_r8k            ! (m / s)
    !
    ! ** Distance **
    !
    ! Convert feet to inches
    REAL(r8k), PARAMETER :: fttoin = 12.0_r8k                        ! (inch / ft)
    ! Convert inches to cm
    REAL(r8k), PARAMETER :: intocm = 2.54_r8k                        ! (cm / in)
    ! Convert feet to cm
    REAL(r8k), PARAMETER :: ftocm = (fttoin * intocm)                ! (cm / ft)
    ! Convert feet to m
    REAL(r8k), PARAMETER :: ftom = ftocm / 100.0_r8k                 ! (meter / ft)
    ! Convert inches to to feet
    REAL(r8k), PARAMETER :: intoft = 1.0_r8k / fttoin                ! (ft / in)
    ! Convert inches to mm
    REAL(r8k), PARAMETER :: intomm = 10.0_r8k * intocm               ! (mm / in)
    ! Convert inches to meter
    REAL(r8k), PARAMETER :: intom = 0.01_r8k * intocm                ! (m / in)
    ! Convert meter to feet
    REAL(r8k), PARAMETER :: mtoft = 1.0_r8k / ftom                   ! (ft / m)
    ! Convert cm to feet
    REAL(r8k), PARAMETER :: cmtoft = 1.0_r8k / ftocm                 ! (ft / cm)
    ! Convert cm to inches
    REAL(r8k), PARAMETER :: cmtoin = 1.0_r8k / intocm                ! (in / cm)
    ! Convert cm to meter
    REAL(r8k), PARAMETER :: cmtom = 0.01_r8k                         ! (m / cm)
    ! Convert meter to inches
    REAL(r8k), PARAMETER :: mtoin = 100.0_r8k * cmtoin               ! (in / m)
    ! Convert meter to cm
    REAL(r8k), PARAMETER :: mtocm = 1.0_r8k / cmtom                  ! (cm / m)
    ! Convert meter to mil
    REAL(r8k), PARAMETER :: mtomil = 1000.0_r8k  * mtoin             ! (m / mil)
    ! Convert mil to meter
    REAL(r8k), PARAMETER :: miltom = 1.0_r8k / mtomil                ! (mil / m)
    ! Convert mil to foot
    REAL(r8k), PARAMETER :: miltoft = 0.001_r8k * intoft             ! (ft / mil)
    ! Convert foot to mil
    REAL(r8k), PARAMETER :: fttomil = 1.0_r8k / miltoft              ! (mil / ft)
    ! Convert mil to millimeter
    REAL(r8k), PARAMETER :: miltomm = 0.01_r8k * intocm              ! (mm / mil)
    ! Convert mil to micrometer
    REAL(r8k), PARAMETER :: miltoum = 10.0_r8k * intocm              ! (micrometer / mil)
    ! Convert micrometer to mil
    REAL(r8k), PARAMETER :: umtomil = 0.1_r8k * cmtoin               ! (mil / micrometer)
    ! Convert micrometer to meter
    REAL(r8k), PARAMETER :: umtom = 1.0E-6_r8k                       ! (meter / micrometer)
    !
    ! ** Area **
    !
    ! Convert ft^2 to in^2
    REAL(r8k), PARAMETER :: ft2toin2 = fttoin ** 2                   ! (in^2 / ft^2)
    ! Convert ft^2 to m^2
    REAL(r8k), PARAMETER :: ft2tom2 = ftom ** 2                      ! (in^2 / ft^2)
    ! Convert in^2 to cm^2
    REAL(r8k), PARAMETER :: in2tocm2 = intocm ** 2                   ! (cm^2 / in^2)
    ! Convert in^2 to cm^2
    REAL(r8k), PARAMETER :: in2tom2 = intom ** 2                     ! (cm^2 / in^2)
    ! Convert in^2 to ft^2
    REAL(r8k), PARAMETER :: in2toft2 = intoft ** 2                   ! (ft^2 / in^2)
    !
    ! ** Volume **
    !
    ! Convert ft^3 to in^3
    REAL(r8k), PARAMETER :: ft3toin3 = fttoin ** 3                   ! (in^3 / ft^3)
    ! Convert ft^3 to m^3
    REAL(r8k), PARAMETER :: ft3tom3 = ftom ** 3                      ! (m^3 / ft^3)
    ! Convert in^3 to cm^3
    REAL(r8k), PARAMETER :: in3tocm3 = intocm ** 3                   ! (cm^3 / in^3)
    ! Convert in^3 to mm^3
    REAL(r8k), PARAMETER :: in3tomm3 = intomm ** 3                   ! (m^3 / in^3)
    ! Convert in^3 to m^3
    REAL(r8k), PARAMETER :: in3tom3 = intom ** 3                     ! (m^3 / in^3)
    ! Convert in^3 to ft^3
    REAL(r8k), PARAMETER :: in3toft3 = intoft ** 3                   ! (ft^3 / in^3)
    ! Convert cm^3 to in^3
    REAL(r8k), PARAMETER :: cm3toin3 = cmtoin ** 3                   ! (in^3 / cm^3)
    ! Convert cm^3 to m^3
    REAL(r8k), PARAMETER :: cm3tom3 = cmtom ** 3                     ! (m^3 / cm^3)
    ! Convert m^3 to cm^3
    REAL(r8k), PARAMETER :: m3tocm3 = mtocm ** 3                     ! (cm^3 / m^3)
    ! Convert m^3 to in^3
    REAL(r8k), PARAMETER :: m3toin3 = mtoin ** 3                     ! (in^3 / m^3)
    !
    ! ** Time **
    !
    ! Convert day to hour
    REAL(r8k), PARAMETER :: daytohr = 24.0_r8k                       ! (hour / day)
    ! Convert day to seconds
    REAL(r8k), PARAMETER :: daytosec = 86400.0_r8k                   ! (second / day)
    ! 1 day
    REAL(r8k), PARAMETER :: oneday = daytosec                        ! 1 day in seconds
    ! Convert day to year
    REAL(r8k), PARAMETER :: daytoyr = 1.0_r8k / 365.25_r8k           ! (yr / day)
    ! Convert hour to day
    REAL(r8k), PARAMETER :: hrtoday = 1.0_r8k / daytohr              ! (day / hour)
    ! Convert hour to seconds
    REAL(r8k), PARAMETER :: hrtosec = 3600.0_r8k                     ! (second / hour)
    ! Convert second to hour
    REAL(r8k), PARAMETER :: sectohr = 1.0_r8k / hrtosec              ! (hour / second)
    ! Convert second to day
    REAL(r8k), PARAMETER :: sectoday = 1.0_r8k / daytosec            ! (second / day)
    ! Convert years to days
    REAL(r8k), PARAMETER :: yrtoday = 1.0_r8k / daytoyr              ! (day / year)
    ! Convert years to seconds
    REAL(r8k), PARAMETER :: yrtosec = 3.1536E7_r8k                   ! (second / year)
    !
    ! ** Energy **
    !
    ! Convert BTU/lb to J/kg
    REAL(r8k), PARAMETER :: BTUlbtoJkg = 2326.000292_r8k             ! (J/kg / BTU/lb)
    ! Convert J/kg to BTU/lb
    REAL(r8k), PARAMETER :: JkgtoBTUlb = 1.0_r8k / BTUlbtoJkg        ! (BTU/lb / J/kg)
    !
    ! ** Heat Conductance (Thermal Conductivity) **
    !
    ! Convert Btu/(hr*ft*F) to W/(m*K)
    REAL(r8k), PARAMETER :: BhftFtoWmK = 1.73073467_r8k              ! (W/m*K / Btu/hr*ft*F)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBhftF = 1.0_r8k / BhftFtoWmK        ! (Btu/hr*ft*F / W/m*K)
    ! Convert W/(m*K) to Btu/(s*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBsftF = WmKtoBhftF / 3600.0_r8k     ! (Btu/s*ft*F / W/m*K)
    ! Convert Btu/(hr*ft*F) to Btu/(sec*ft*F)
    REAL(r8k), PARAMETER :: BhftFtoBsftF = 1.0_r8k / hrtosec         ! (Btu/(sec*ft*F) / Btu/(hr*ft*F))
    ! Convert Btu/(sec*ft*F) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: BsftFtoBhftF = hrtosec                   ! (Btu/(hr*ft*F) / Btu/(sec*ft*F))
    !
    ! ** Heat Capacity **
    !
    ! Convert Btu/(lb*F) to J/(kg*K)
    REAL(r8k), PARAMETER :: BtulbFtoJkgK = 4183.995381_r8k           ! (J/kg*K / Btu/lb*F) or (J/kg*C)
    ! Convert J/(kg*K) to Btu/(lb*F)
    REAL(r8k), PARAMETER :: JkgKtoBtulbF = 1.0_r8k / BtulbFtoJkgK    ! (Btu/lb*F / J/kg*K) or (J/kg*C)
    !
    ! ** Heat Flux **
    !
    ! Convert (Btu/(hr*ft^2)) to (W/m^2)
    REAL(r8k), PARAMETER :: Bhft2toWm2 = 3.152481_r8k                ! (W/m2) / (Btu/h*ft2)
    ! Note:  3.154590745 (BTU(IT)) or 3.152481 (BTU(TH))
    ! Convert (Btu/(hr*ft^2)) to (W/m^2)
    REAL(r8k), PARAMETER :: Wm2toBhft2 = 1.0_r8k / Bhft2toWm2        ! (Btu/h*ft2) / (W/m2)
    !
    ! ** Heat Transfer Coefficients **
    !
    ! Convert Btu/(hr*ft^2*F) to W/(m^2*K)
    ! Note: 5.6744658974 (BTU(TH)) or 5.6782633411 (BTU(IT))
    REAL(r8k), PARAMETER :: Bhft2FtoWm2K = 5.6782633411_r8k          ! Convert gap HTC (W/(m^2*K) / Btu/hr*ft^2*F)
    ! Convert W/(m^2*K) to Btu/(hr*ft^2*F)
    REAL(r8k), PARAMETER :: Wm2KtoBhft2F = 1.0_r8k / Bhft2FtoWm2K    ! Convert gap HTC (Btu/(hr*ft^2*F) / W/(m^2*K))
    !
    ! ** Mass **
    !
    ! Convert lb to grams
    REAL(r8k), PARAMETER :: lbtog = 453.5923699997481_r8k            ! (grams / lb)
    ! Convert grams to lb
    REAL(r8k), PARAMETER :: gtolb = 1.0_r8k / lbtog                  ! (lb / gram)
    ! Convert kg to grams
    REAL(r8k), PARAMETER :: kgtog = 1000.0_r8k                       ! (grams / kg)
    ! Convert grams to kg
    REAL(r8k), PARAMETER :: gtokg = 1.0_r8k / kgtog                  ! (kg / gram)
    !
    ! ** Mass Flux **
    !
    ! Convert kg/(s*m^2) to lbm/(hr*ft^2)
    REAL(r8k), PARAMETER :: ksm2tolbhrft2 = 737.3422919_r8k          ! (lbm/(hr*ft^2) / kg/(s*m^2))
    ! Convert lbm/(hr*ft^2) kg/(s*m^2)
    REAL(r8k), PARAMETER :: lbhrft2toksm2 = 1.0_r8k / ksm2tolbhrft2  ! (kg/(s*m^2) / lbm/(hr*ft^2))
    !
    ! ** Density **
    !
    ! Convert lb/ft3 to kg/m3
    REAL(r8k), PARAMETER :: lbft3tokgm3 = 16.0184634_r8k             ! (kg/m^3 / lb/ft^3)
    ! Convert kg/m3 to lb/ft3
    REAL(r8k), PARAMETER :: kgm3tolbft3 = 1.0_r8k / lbft3tokgm3      ! (lb/ft^3 / kg/m^3)
    ! Convert kg/m3 to g/in3
    REAL(r8k), PARAMETER :: kgm3togin3 = kgtog / m3toin3             ! (g/in^3 / kg/m^3)
    !
    ! ** Specific Volume **
    !
    ! Convert ft3/lb to m3/kg
    REAL(r8k), PARAMETER :: ft3lbtom3kg = 1.0_r8k / lbft3tokgm3      ! (m^3/kg / ft^3/lb)
    ! Convert m3/kg to ft3/lb
    REAL(r8k), PARAMETER :: m3kgtoft3lb = 1.0_r8k / ft3lbtom3kg      ! (ft^3/lb / m^3/kg)
    !
    ! ** Power **
    !
    ! Convert watt to btu/hr
    ! Note: 3.41214163513 (BTU(IT)) or 3.41442595 (BTU(TH))
    REAL(r8k), PARAMETER :: WtoBTUh = 3.41442595_r8k                 ! (Btu/hr / W)
    ! Convert watt to btu/s
    REAL(r8k), PARAMETER :: WtoBTUs = WtoBTUh / hrtosec              ! (Btu/second / W)
    ! Convert kilowatt to btu/hr
    REAL(r8k), PARAMETER :: kWtoBTUh = WtoBTUh * 1.0E3_r8k           ! (Btu/hr / kW)
    ! Convert kilowatt to btu/s
    REAL(r8k), PARAMETER :: kWtoBTUs = kWtoBTUh / hrtosec            ! (Btu/second / kW)
    ! Convert megawatt to btu/hr
    REAL(r8k), PARAMETER :: MWtoBTUh = WtoBTUh * 1.0E6_r8k           ! (Btu/hr / MW)
    ! Convert megawatt to btu/s
    REAL(r8k), PARAMETER :: MWtoBTUs = MWtoBTUh / hrtosec            ! (Btu/second / MW)
    ! Convert btu/hr to watt
    REAL(r8k), PARAMETER :: BTUhtoW = 1.0_r8k / WtoBTUh              ! (W / Btu/hr)
    ! Convert btu/hr to kilowatt
    REAL(r8k), PARAMETER :: BTUhtokW = 1.0_r8k / kWtoBTUh            ! (kW / Btu/hr)
    ! Convert btu/hr to megawatt
    REAL(r8k), PARAMETER :: BTUhtoMW = 1.0_r8k / MWtoBTUh            ! (MW / Btu/hr)
    ! Convert btu/s to watt
    REAL(r8k), PARAMETER :: BTUstoW = 1.0_r8k / WtoBTUs              ! (W / Btu/sec)
    ! Convert btu/s to kilowatt
    REAL(r8k), PARAMETER :: BTUstokW = 1.0_r8k / kWtoBTUs            ! (kW / Btu/sec)
    ! Convert btu/s to megawatt
    REAL(r8k), PARAMETER :: BTUstoMW = 1.0_r8k / MWtoBTUs            ! (MW / Btu/sec)
    ! Convert kW/m to kW/ft
    REAL(r8k), PARAMETER :: kWmtokWft = ftom                         ! (kW/ft / kW/m)
    ! Convert kW/ft to kW/m
    REAL(r8k), PARAMETER :: kWfttokWm = 1.0_r8k / kWmtokWft          ! (kW/m / kW/ft)
    ! Convert kW/ft to W/m
    REAL(r8k), PARAMETER :: kWfttoWm = 1.0E3_r8k * kWfttokWm         ! (W/m / kW/ft)
    ! Convert W/m to kW/ft
    REAL(r8k), PARAMETER :: WmtokWft = 1.0_r8k / kWfttoWm            ! (kW/ft / W/m)
    ! Convert W/cm^3 to kW/m^3
    REAL(r8k), PARAMETER :: Wcm3tokWm3 = 1.0E-3_r8k / cm3tom3        ! (kW/m^3 / W/cm^3)
    !
    ! ** Power Density **
    !
    ! Convert btu/hr*ft^3 to watt/m^3
    REAL(r8k), PARAMETER :: BTUhft3toWm3 = BTUhtoW / ft3tom3         ! (W / Btu/hr)
    !
    ! ** Pressure **
    !
    ! Convert atm to psi
    REAL(r8k), PARAMETER :: ATMtoPSI = 14.6959488_r8k                ! (psi / atm)
    ! Convert psi to atm
    REAL(r8k), PARAMETER :: PSItoATM = 1.0_r8k / ATMtoPSI            ! (atm / psi)
    ! Convert psi to lb/ft^2
    REAL(r8k), PARAMETER :: PSItolbft2 = 144.0_r8k                   ! (lb/in^2 / lb/ft^2)
    ! Convert kg/cm^2 to psi
    REAL(r8k), PARAMETER :: kgcm2toPSI = 14.2233433_r8k              ! (psi / kg-Force/cm^2)
    ! Convert psi to kg/cm^2
    REAL(r8k), PARAMETER :: PSItokgcm2 = 1.0_r8k / kgcm2toPSI        ! (kg-Force/cm^2 / psi)
    ! Convert psi to kPa
    REAL(r8k), PARAMETER :: PSItokPa = 6.89475728_r8k                ! (kPa / psi)
    ! Convert psi to Pa
    REAL(r8k), PARAMETER :: PSItoPa = 1.0E3_r8k * PSItokPa           ! (Pa / psi)
    ! Convert psi to MPa
    REAL(r8k), PARAMETER :: PSItoMPa = 1.0E-3_r8k * PSItokPa         ! (MPa / psi)
    ! Convert Pa to psi
    REAL(r8k), PARAMETER :: PatoPSI = 1.0_r8k / PSItoPa              ! (Pa / psi)
    ! Convert MPa to psi
    REAL(r8k), PARAMETER :: MPatoPSI = 1.0_r8k / PSItoMPa            ! (Pa / psi)
    !
    ! ** Viscosity **
    !
    ! Convert kg/m*s to lbm/ft*hr
    REAL(r8k), PARAMETER :: kgmstolbfthr = 2419.088310502_r8k        ! (lbm/ft*hr / kg/m*s)
    ! Convert lbm/ft*hr to kg/m*s
    REAL(r8k), PARAMETER :: lbfthrtokgms = 1.0_r8k / kgmstolbfthr    ! (kg/m*s / lbm/ft*hr)
    !
    CONTAINS
    !
    ! ** Temperature Conversion Functions **
    !
    !
    ! ** Temperature Conversion Functions **
    !
    PURE ELEMENTAL FUNCTION tkf (T_Kelvin) RESULT (T_Fahrenheit)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: K to F
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/14/2016
    REAL(r8k), INTENT(IN) :: T_Kelvin
    REAL(r8k) :: T_Fahrenheit
    
    T_Fahrenheit = (T_Kelvin * 1.8_r8k) - 459.67_r8k
    
    END FUNCTION tkf
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfr (T_Fahrenheit) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  R
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Rankine
    
    T_Rankine = T_Fahrenheit + 459.67_r8k
    
    END FUNCTION tfr
    !
    !
    !
    PURE ELEMENTAL FUNCTION tkc (T_Kelvin) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: Kelvin to Celcius
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/23/2016
    REAL(r8k), INTENT(IN) :: T_Kelvin
    REAL(r8k) :: T_Celcius
    
    T_Celcius = T_Kelvin - 273.15_r8k
    
    END FUNCTION tkc
    !
    !
    !
    PURE ELEMENTAL FUNCTION tck (T_Celcius) RESULT (T_Kelvin)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: Celcius to Kelvin
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/23/2016
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Kelvin
    
    T_Kelvin = T_Celcius + 273.15_r8k
    
    END FUNCTION tck
    !
    !
    !
    PURE ELEMENTAL FUNCTION trc (T_Rankine) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  R  to  C
    REAL(r8k), INTENT(IN) :: T_Rankine
    REAL(r8k) :: T_Celcius
    
    T_Celcius = (T_Rankine / 1.8_r8k) - 273.15_r8k
    
    END FUNCTION trc
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfk (T_Fahrenheit) RESULT (T_Kelvin)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: F to K
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/14/2016
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Kelvin
    
    T_Kelvin = (T_Fahrenheit + 459.67_r8k) / 1.8_r8k
    
    END FUNCTION tfk
    !
    !
    !
    PURE ELEMENTAL FUNCTION tcr (T_Celcius) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  R
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Rankine
    
    T_Rankine = ((T_Celcius * 1.8_r8k) + 32.0_r8k) + 459.67_r8k
    
    END FUNCTION tcr
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfc (T_Fahrenheit) RESULT (T_Celcius)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  C
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Celcius
    
    T_Celcius = (T_Fahrenheit - 32.0_r8k) / 1.8_r8k
    
    END FUNCTION tfc
    !
    !
    !
    PURE ELEMENTAL FUNCTION tcf (T_Celcius) RESULT (T_Fahrenheit)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  F
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Fahrenheit
    
    T_Fahrenheit = (T_Celcius * 1.8_r8k) + 32.0_r8k
    
    END FUNCTION tcf
    !
END MODULE Conversions