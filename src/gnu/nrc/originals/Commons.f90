MODULE Commons
    USE Kinds
    IMPLICIT NONE
    
    PUBLIC
    PRIVATE :: Mat
    
    ! Output file unit #
    INTEGER(ipk) :: ounit = 6_ipk
    
    ! Problem time incriment, (s)
    REAL(r8k) :: deltaTime = 0.0_r8k
    ! Material type
    TYPE :: Mat
        INTEGER(ipk) :: id = -1_ipk
    END TYPE Mat
    
    ! Fuel parameters
    
!    TYPE, EXTENDS (Mat) :: Fuel_Mat
!    END TYPE Fuel_Mat
    
!    TYPE (Fuel_Mat) :: UO2_Fuel(0_ipk), MOX_NFI_Fuel(1_ipk), MOX_Halden_Fuel(2_ipk)
!    TYPE (Fuel_Mat) :: FuelType
    ! Available fuel types
    INTEGER(ipk), PARAMETER :: UO2_id = 0_ipk, MOX_NFI_id = 1_ipk, MOX_Halden_id = 2_ipk, U3Si2_id = 3_ipk
    ! Fuel type
    INTEGER(ipk) :: FuelType = -1_ipk
    
    ! As-fabricated apparent fuel density (%TD)
    REAL(r8k) :: den = 95.0
    ! Fuel open porosity fraction (%TD)
    REAL(r8k) :: deng = 0.0_r8k
    ! Fuel roughness value, (m)
    REAL(r8k) :: FuelRoughness = 2.0e-6_r8k
    ! Expected pellet resintering density increase, (kg/m^3)
    REAL(r8k) :: rsntr = 0.0_r8k
    ! Pellet sintering temperature, (K)
    REAL(r8k) :: tsint = 0.0_r8k
    ! Specifies fuel relocation model
    CHARACTER(LEN=12) :: RelocModel = 'FRAPCON-3.5'
    
    ! Cladding parameters
    
!    TYPE, EXTENDS (Mat) :: Clad_Mat
!    END TYPE Clad_Mat
!    TYPE (Clad_Mat) :: Zirc2_Clad(2_ipk), Zirc4_Clad(4_ipk), M5_Clad(5_ipk), ZIRLO_Clad(6_ipk), OptZIRLO_Clad(7_ipk), &
!      &                Zr1Nb_Clad(9_ipk), M110_Clad(10_ipk), SiC_Clad(11_ipk)
!    TYPE (Clad_Mat) :: CladType
    ! Available cladding types
    INTEGER(ipk), PARAMETER :: Zirc2_id = 2_ipk, Zirc4_id = 4_ipk, M5_id = 5_ipk, ZIRLO_id = 6_ipk, OptZIRLO_id = 7_ipk, &
      &                        Zr1Nb_id = 9_ipk, E110_id = 10_ipk, SiC_id = 11_ipk, FeCrAl_id = 15_ipk, SS304_id = 16_ipk, &
      &                        SS316_id = 17_ipk, SS347_id = 18_ipk, Inconel600_id = 19_ipk, Inconel718_id = 20_ipk, &
      &                        CSA508_id = 21_ipk
    ! Cladding type
    INTEGER(ipk) :: CladType = -1_ipk
    
    ! User-defined cladding elastic modulus, (Pa)
    REAL(r8k) :: cladelmod = 0.0_r8k
    ! Cladding roughness value, (m)
    REAL(r8k) :: CladRoughness = 5.0e-7_r8k
    
    ! Gas parameters
    
!    TYPE, EXTENDS (Mat) :: Gas_Mat
!    END TYPE Gas_Mat
!    TYPE (Gas_Mat) :: He(1_ipk), Argon(2_ipk), Krypton(3_ipk), Xenon(4_ipk), Hydrogen(5_ipk), Nitrogen(6_ipk), &
!      &               Air(7_ipk), Steam(8_ipk)
!    TYPE (Gas_Mat) :: GasType
    ! Gas type used by code
    INTEGER(ipk) :: GasType = -1_ipk
    ! Molar composition
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasMixture
    
    ! Oxide parameters
    
    ! Available oxide types
    INTEGER(ipk), PARAMETER :: ZrO2_id = 0_ipk, SiO2_id = 1_ipk
    ! Oxide type used by code
    INTEGER(ipk) :: OxideType = -1_ipk
    
    ! Crud parameters
    
    ! Fraction of theoretical density of crud (-)
    REAL(r8k), PARAMETER :: ftd_crud = 1.0_r8k
    
    ! Coolant parameters
    
!    TYPE, EXTENDS (Mat) :: Coolant_Mat
!    END TYPE Coolant_Mat
!    TYPE (Coolant_Mat) :: Supplied(0_ipk), Water(1_ipk), Helium(2_ipk), Sodium(3_ipk)
!    TYPE (Coolant_Mat) :: CoolantType
    ! Available Coolant Types
    INTEGER(ipk), PARAMETER :: Supplied = 0_ipk, Water = 1_ipk, Helium = 2_ipk, Sodium = 3_ipk
    ! Coolant type used by code
    INTEGER(ipk) :: CoolantType = -1_ipk
    
    ! Uncertainty parameters
    
    ! Bias on cladding axial growth (# of standard deviations away)
    REAL(r8k) :: siggro = 0.0_r8k
    ! Bias on fuel thermal conductivity
    REAL(r8k) :: sigftc = 0.0_r8k
    ! Bias on fuel thermal expansion
    REAL(r8k) :: sigftex = 0.0_r8k
    ! Bias on fuel swelling
    REAL(r8k) :: sigswell = 0.0_r8k
    
    CONTAINS
        
        SUBROUTINE Set_Material_ids (Fuel_mat_id, Gas_mat_id, Clad_mat_id, Oxide_mat_id, Coolant_mat_id)
        IMPLICIT NONE
        !>@brief
        !> This subroutine sets the material types for the fuel, cladding, gap-gas and coolant
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/15/2017
        INTEGER(ipk), INTENT(IN), OPTIONAL :: Fuel_mat_id, Gas_mat_id, Clad_mat_id, Oxide_mat_id, Coolant_mat_id
        
        IF (Present(Fuel_mat_id)) FuelType = Fuel_mat_id
        IF (Present(Gas_mat_id)) GasType = Gas_mat_id
        IF (Present(Clad_mat_id)) CladType = Clad_mat_id
        IF (Present(Oxide_mat_id)) OxideType = Oxide_mat_id
        IF (Present(Coolant_mat_id)) CoolantType = Coolant_mat_id
        
        END SUBROUTINE Set_Material_ids
        
        SUBROUTINE Set_materials_commons (Outputunit, ElasticModulus, roughc, roughf, Resintering, SinteringTemp, &
          &                               RelocationModel, AsFabDensity, FuelOpenPorosity)
        IMPLICIT NONE
        !>@brief
        !> This subroutine sets the common variables used in the materials library package
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/15/2017
        
        INTEGER(ipk), INTENT(IN), OPTIONAL :: Outputunit
        REAL(r8k), INTENT(IN), OPTIONAL :: ElasticModulus, roughc, roughf, Resintering, SinteringTemp, AsFabDensity, &
          &                                FuelOpenPorosity
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: RelocationModel
        
        IF (PRESENT(Outputunit)) ounit = Outputunit
        IF (PRESENT(ElasticModulus)) cladelmod = ElasticModulus
        IF (PRESENT(roughc)) CladRoughness = roughc
        IF (PRESENT(roughf)) FuelRoughness = roughf
        IF (PRESENT(Resintering)) rsntr = Resintering
        IF (PRESENT(SinteringTemp)) tsint = SinteringTemp
        IF (PRESENT(RelocationModel)) RelocModel = RelocationModel
        IF (PRESENT(AsFabDensity)) den = AsFabDensity
        IF (PRESENT(FuelOpenPorosity)) deng = FuelOpenPorosity
        
        END SUBROUTINE Set_materials_commons
        !
        !
        !
        SUBROUTINE Set_Uncertainties (CladAxialGrowth, Fuelthcon, Fuelthexp, Fuelswell)
        !>@brief
        !> This subroutine sets the uncertainty variables used in the materials library package
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 3/15/2017
        
        REAL(r8k), INTENT(IN), OPTIONAL :: CladAxialGrowth, Fuelthcon, Fuelthexp, Fuelswell
        
        IF (PRESENT(CladAxialGrowth)) siggro = CladAxialGrowth
        IF (PRESENT(Fuelthcon)) sigftc = Fuelthcon
        IF (PRESENT(Fuelthexp)) sigftex = Fuelthexp
        IF (PRESENT(Fuelswell)) sigswell = Fuelswell
        
        END SUBROUTINE Set_Uncertainties
END MODULE Commons