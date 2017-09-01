MODULE FissionGas
    USE Kinds
    USE Functions, ONLY : terp
    USE Variables, ONLY : ounit, na, rc, ngasr, ANS54_1982, Massih, FRAPFGR, ANS54_2011
    USE FGR_Mesh, ONLY : scangas, IFBA_Coating
    IMPLICIT NONE
    !>@brief
    !> This module contains the fission gas release subroutines
    !>@author
    !> FissionGas was re-written by Ian Porter, NRC
    !>@date
    !> January 2017
    
    PRIVATE
    PUBLIC :: gaspro, fgasre, gasplt, totgas
    
    ! Refabricated nodal burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSNodeburnuprefab
    
    ! Parameters for ANS correlations
    ! # of short-lived nuclides tracked by ANS54 model
    INTEGER(ipk), PARAMETER :: nshortnuclides = 11
    ! # of long-lived nuclides tracked by ANS54 model
    INTEGER(ipk), PARAMETER :: nlongnuclides = 4
    ! Total # of nuclides tracked by ANS54 model
    INTEGER(ipk), PARAMETER :: nnuclides = nshortnuclides + nlongnuclides
    TYPE FGR_Nuclide
        REAL(r8k), DIMENSION(nshortnuclides) :: half = 0.0_r8k
        REAL(r8k), DIMENSION(nshortnuclides) :: decay = 0.0_r8k
        ! Fraction of short-lived radioactive gases released
        REAL(r8k), DIMENSION(nshortnuclides) :: release = 0.0_r8k
    END TYPE FGR_Nuclide
    ! Master array of ANS54 variables
    TYPE (FGR_Nuclide), SAVE :: ANS54
    
    ! Massih's constants for approximating the integration kernal
    REAL(r8k), DIMENSION(4), PARAMETER :: a = [0.231545_r8k, 0.0948453_r8k, 0.0282898_r8k, 0.645320_r8k]
    REAL(r8k), DIMENSION(4), PARAMETER :: b = [72.5968_r8k, 796.773_r8k, 29083.0_r8k, 10.2469_r8k]
    !
    CONTAINS
    !
    SUBROUTINE gaspro
    USE Conversions, ONLY : in2tocm2, Pi, ftocm, Avogadro
    USE Variables, ONLY : ProblemTime, imox, comp, moxtype, EOSNodeburnup, dp, na, fgmgp, hemgp, &
      &                   deltaz, qc, rc, it, j, frden, sgapf, dcoBOL, UO2, MOX_NFI, MOX_Halden
    USE Comde, ONLY : Enrichment
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and computes the fission gas and helium production.
    !>@author
    !> This routine was coded by g a berna in december 1977.
    !
    ! Input
    !
    ! dcoBOL - cladding outside diameter (in)
    ! deltaz - node length (ft)
    ! Enrichment - Isotopic enrichment, (Atom % in heavy metal)
    ! it     - power-time step index
    ! j      - axial node index
    ! na     - maximum number of axial nodes
    ! qc     - heat flux at node j (btu/hr-ft**2)
    ! sgapf  - number of fission gas atoms produced per 100 fissions
    ! ProblemTime - time (sec)
    !
    ! Output
    !
    ! fgmgp   - cumulative fission gas production (gm-moles)
    ! hemgp   - cumulative helium gas production (gm-moles)
    !
    INTEGER(ipk) :: lt, il, i
    REAL(r8k) :: burnup
    REAL(r8k), PARAMETER, DIMENSION(2) :: A1 = [  1.5350E-4_r8k, -2.4360E-4_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: A2 = [  2.1490E-3_r8k,  3.6059E-3_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: B1 = [ -2.9080E-3_r8k,  3.3790E-3_r8k ]
    REAL(r8k), PARAMETER, DIMENSION(2) :: B2 = [  9.7223E-2_r8k,  5.3658E-2_r8k ]
    !
    burnup = EOSNodeburnup(j-1)
    il = MAX(it - 1, 1)
    DO lt = il, it
        i = 2 + lt - it
        fgmgp(j-1,i) = qc(j-1) * ProblemTime(lt) * dcoBOL * deltaz(j-1) * sgapf * 9.95e-18_r8k
        fgmgp(j-1,i) = fgmgp(j-1,i) * 4.0_r8k
        SELECT CASE (imox)
        CASE (UO2)
            hemgp(j-1,i) = qc(j-1) * ProblemTime(lt) * dcoBOL * deltaz(j-1) * 2.98e-18_r8k
        CASE (MOX_NFI, MOX_Halden)
            hemgp(j-1,i) = MAX((((A1(moxtype) * comp(j-1) + A2(moxtype)) * burnup ** 2 + (B1(moxtype) * comp(j-1) + &
                &              B2(moxtype)) * burnup) * 0.88_r8k / 4.0_r8k / 1.0e6_r8k * frden * 11.0_r8k * pi / 4.0_r8k * &
                &              (dp(j-1) ** 2 - (2.0_r8k * rc(j-1)) ** 2) * in2tocm2 * deltaz(j-1) * ftocm + &
                &              (2.11e10_r8k * Enrichment(j-1)%Pu239 + 7.77e10_r8k * Enrichment(j-1)%Pu240 + &
                &               8.49e8_r8k * Enrichment(j-1)%Pu240 + 1.36e9_r8k * Enrichment(j-1)%Pu242) / &
                &              Avogadro * ProblemTime(lt) * pi / 4.0_r8k * (dp(j-1) ** 2 - &
                &              (2.0_r8k * rc(j-1)) ** 2) * in2tocm2 * deltaz(j-1) * ftocm), 1.0E-30_r8k)
        CASE DEFAULT
            ERROR STOP 'Bad input for imox. Execution terminated in Subroutine: gaspro'
        END SELECT
    END DO
    !
    END SUBROUTINE gaspro
    !
    !
    !
    SUBROUTINE fgasre
    USE Conversions, ONLY : tfk, Pi
    USE Variables, ONLY : ProblemTime, ngasmod, tfuelr, nr, ngasr, crad, deltaz, na, angr, fmgr, hmgr, hemgp, fgmgp, Power, &
      &                   ang, rdotwrt, it, j, totl, angi, delbp, gasflg, nvoid, rdot, ah2ogr, ah2og, h2omi, EOSNodeBurnup
    USE Mesh, ONLY : FGR_Elements, ZrB2
    USE FGR_Mesh, ONLY : ans54_node, Massih_Node, FRAPFGR_node, fgr_node
    IMPLICIT NONE
    !>@brief
    !> Subroutine fgasre computes the fission gas production and release, the helium production and release,
    !> and the nitrogen release for a given axial node. fgasre is called by the frpcon subroutine.
    !>@author
    !> This Subroutine was modified for use in frap-t and frapcon by g a berna from frap-s in oct 77.
    !> The Subroutine was modified by DD Lanning and K Geelhood in 1995 to better accommodate a new fission gas release
    !> subroutine (MASSIH) and modified version of RADAR.
    !> See Letters:
    !> Lanning to Siefken, 7/24/95, "Recommended Fission Gas Release Models for FRAPCON-3" 
    !> Lanning to Davis, 8/17/95, "Recommended Updates to FRAPCON Regarding use of subroutines RADAR and TUBRNP for ...."
    !
    !
    ! Input
    !
    ! afgr          - additional fractional gas release factor
    ! ang           - cumulative n2 concentration in fuel per power step (moles)
    ! angi          - initial nitrogen concentration in fuel (moles)
    ! EOSNodeBurnup - burnup to eos for axial node j (MWd/mtU)
    ! BOSNodeBurnup - burnup to bos for axial node j (MWd/mtU)
    ! delbp         - incremental burnup this time step, this axial node
    ! dcoBOL        - as fabricated clad o.d.
    ! delhs         - power-time step time (sec)
    ! deltaz        - length of the axial node (ft)
    ! dp            - pellet diameter
    ! gasflg        - convergence flag on gas pressure iteration
    ! press         - rod internal gas pressure (psia)
    ! hemgp         - cumulative helium  gas production per time step & node
    ! im            - total number of time steps
    ! it            - power-time step index
    ! iter          - gas loop iteration index
    ! j             - axial node index
    ! jst           - indicator for time dependent axial power profile
    ! ngasr         - fission gas release model index
    !                 >= 6, ans-5.4
    !                 >= 11, ans-5.4 using modified Massih model
    ! nmesh         - cladding outside surface node index
    ! nr            - maximum number of radial nodes
    ! nvoid         - central void index
    !             0 = no central void
    !             1 = central void exists
    ! q             - axial power profile array
    ! qmpy          - time dependent power array
    ! r             - cold state radial node locations from cl (in) (radial node 1 is at the fuel surface)
    ! rc            - radius of pellet annulus
    ! rnodls        - cold state radial node locations from cl (in)
    ! tfuelr        - fuel ring boundary temperature array (F)
    ! ProblemTime   - time step array
    ! tlast         - time to beginning of power-time step (sec)
    ! tnow          - time to end of power-time step (sec)
    ! totl          - total active fuel length (ft)
    !
    ! Output
    !
    ! angr   - nitrogen release per node & power step (moles)
    ! fmgr   - cumulative fission gas release per node & time (moles)
    ! hmgr   - helium release per node & power step (moles)
    !
    ! Note:
    !
    ! y(aa,bb) = (1.e0-EXP(-0.436e-4*(bb-2.e4)))/(1.e0+(0.665e0/aa)*EXP(-0.1107e-3*(bb-2.e4)))
    ! i=1, time to beginning of power-time step
    ! i=2, time to End of the power-time step
    
    INTEGER(ipk) :: i, l, il, n, m, nrings
    REAL(r8k) :: coeff, coef1, PelAvgBu, fl, flh, fnn, rr, rpstn, frd, frdh, frdn, flh2o, frdh2o
    REAL(r8k), DIMENSION(2) :: times
    REAL(r8k), DIMENSION(ngasr) :: flxfc
    REAL(r8k), DIMENSION(nr) :: dplh, dpn, dph2o, dv, rv
    REAL(r8k), DIMENSION(nr,2) :: fh, fn, fh2o
    !
    IF (.NOT. ALLOCATED(EOSNodeburnuprefab)) THEN
        ALLOCATE (EOSNodeburnuprefab(1:na))
        EOSNodeburnuprefab = 0.0_r8k
    END IF
    
    times(1) = ProblemTime(it-1)
    times(2) = ProblemTime(it)
    rdot = 0.0_r8k
    DO i = 1, 2
        n = 1
        IF (nvoid == 1) n = 2
        DO l = n, nr
            ! Calculation of Helium fraction release (Booth diffusion model)
            
            ! Diffusion coefficient for He divided by effective diffusion radius squared (s^-1)
            IF (tfuelr(l) <= 1112.0_r8k) THEN
                dplh(l) = 0.452847e-10_r8k
            ELSE
                dplh(l) = 0.28e-5_r8k * EXP((4.0e4_r8k / 1.986_r8k) * (1.0_r8k / 1673.0_r8k - 1.0_r8k / tfk(tfuelr(l))))
            END IF
            
            IF (times(i) <= (1.0_r8k / (pi ** 2 * dplh(l)))) THEN
                fh(l,i) = 4.0_r8k * SQRT(dplh(l) * times(i) / pi) - 3.0_r8k * dplh(l) * times(i) / 2.0_r8k
                IF (fh(l,i) > 0.57_r8k) THEN
                    coeff = Pi ** 2 * dplh(l) * times(i)
                    coef1 = coeff
                    IF (coef1 > 200.0_r8k) coef1 = 200.0_r8k
                    fh(l,i) = 1.0_r8k + (0.607927_r8k * EXP(-coef1) - 0.653644_r8k) / coeff
                    IF (fh(l,i) > 1.0_r8k) fh(l,i) = 1.0_r8k
                END IF
            ELSE
                coeff = Pi ** 2 * dplh(l) * times(i)
                coef1 = coeff
                IF (coef1 > 200.0_r8k) coef1 = 200.0_r8k
                fh(l,i) = 1.0_r8k + (0.607927_r8k * EXP(-coef1) - 0.653644_r8k) / coeff
                IF (fh(l,i) > 1.0_r8k) fh(l,i) = 1.0_r8k
            END IF
            
            ! Nitrogen fractional release
            dpn(l) = 0.173_r8k * EXP(-33400.0_r8k / (1.9869_r8k * tfk(tfuelr(l))))
            coeff = pi ** 2 * dpn(l) * times(i)
            coef1 = MIN(200.0_r8k, coeff)
            IF (coeff > 1.0_r8k) THEN
                fn(l,i) = 1.0_r8k - 6.0_r8k * EXP(-coef1) / (pi ** 2)
            ELSE
                fn(l,i) = 6.0_r8k * SQRT(dpn(l) * times(i) / pi) - 3.0_r8k * dpn(l) * times(i)
            END IF
            IF (fn(l,i) > 1.0_r8k) fn(l,i) = 1.0_r8k
            
            ! Water fractional release
            dph2o(l) = 0.0_r8k
            fh2o(l,i) = 0.0_r8k
        END DO
        ! i=1, time is referenced to beginning of power-time step
        IF (i == 2) THEN
            ! Calculation of fission gas release
            PelAvgBu = EOSNodeburnup(j-1) - delbp / 2.0_r8k
            ! NOTE: THE MESH NEVER CHANGES. THIS SHOULD BE CALLED DURING PROGRAM SETUP
            !       AND NOT REPEATEDLY CALLED EVERY AXIAL NODE.
            ! Update the mesh
            ! *** NOTE: THE ONLY VARIABLE THAT IS ACTUALLY UPDATED IS rv ***
            !CALL Create_FGR_Mesh (na, nr, ngasr, dp, rc, j-1, rv, dv, FGRData%ansr)
            ! Update radial power profile
            !CALL Update_Radial_Power_Profile (j-1, rv, dv)
            BLOCK
                !Update_Radial_Power_Profile (z, rv, dv)
                USE Variables, ONLY : rapow
                !INTEGER(ipk), INTENT(IN) :: z
                INTEGER(ipk) :: r_thermal, z
                !REAL(r8k), DIMENSION(:), INTENT(OUT) :: rv, dv
                DO r_thermal = 1, nr
                        ! Cold node diameter
                        ! NOTE: dv was replaced with %r
    !                    Thermal_Mesh(z,r_thermal)%r = crad(r_thermal,z) * 2.0_r8k
                        !dv(r_thermal) = crad(r_thermal,z) * 2.0_r8k
                    dv(r_thermal) = crad(r_thermal,j-1) * 2.0_r8k
                    
                        ! Radial power fraction
                        ! NOTE: rv was replaced with q_rel
                        !Thermal_Mesh(z,r_thermal)%q_rel = rapow(r_thermal,z)
                        !rv(r_thermal) = rapow(r_thermal,z)
                    rv(r_thermal) = rapow(r_thermal,j-1)
                END DO
            
                !END SUBROUTINE Update_Radial_Power_Profile
            END BLOCK
            
            ! Update the flux depression (based on new radial power profile)
            CALL Update_Flux_Depression (FGR_Elements(j-1,:), dv, rv, flxfc)
            
            ! Call fission gas release model
            SELECT TYPE (FGR_Elements)
            TYPE IS (FGR_Node)
            CLASS IS (ANS54_Node)
                SELECT CASE (ngasmod)
                CASE (ANS54_1982)
                    CALL ans54_1982_FGR (PelAvgBu, flxfc, FGR_Elements(:,:))
                CASE (ANS54_2011)
                    CALL ans54_2011_FGR (PelAvgBu, flxfc, FGR_Elements(:,:))
                END SELECT
            CLASS IS (Massih_Node)
                CALL Massih_FGR (flxfc, FGR_Elements(:,:))
            CLASS IS (FRAPFGR_Node)
                CALL frapfgr_FGR (flxfc, FGR_Elements(:,:))
                
                gfortran_error: BLOCK 
                  USE ISO_FORTRAN_ENV, only : ERROR_UNIT
                  WRITE(ERROR_UNIT,*) 'fgasre (FissionGas): "call Burst_Release(...)" commented becuase of gfortran 7 bug'
                END BLOCK gfortran_error
                ! CALL Burst_Release (j-1, FGR_Elements(j-1,:))
            END SELECT
            
            ! Calculate helium production from ZrB2 coated IFBA rods
            CALL ZrB2(j-1)%Calc_He_Prod(Power(j-1), gasflg)
        END IF
        ! Calculation of the increment average gas fraction release of fission gas, helium, and nitrogen
        fl = 0.0_r8k
        flh = 0.0_r8k
        fnn = 0.0_r8k
        flh2o = 0.0_r8k
        m = 2
        IF (nvoid == 1) m = 3
        DO il = m, nr
            rr = -(crad(il,j-1) ** 2 - crad(il-1,j-1) ** 2) / 2.0_r8k
            flh = flh + (fh(il,i) + fh(il-1,i)) * rr
            fnn = fnn + (fn(il,i) + fn(il-1,i)) * rr
            flh2o = flh2o + (fh2o(il,i) + fh2o(il-1,i)) * rr
        END DO
        nrings = nr - 1
        IF (nvoid == 1) nrings = nrings - 1
        rpstn = (crad(1,j-1) ** 2) * nrings
        frd = rdot
        ! Helium release
        frdh = flh / rpstn
        ! Nitrogen release
        frdn = fnn / rpstn
        ! Water release
        frdh2o = flh2o / rpstn
        ! Calculation of gas releases in moles
        ! Fission gas release
        fmgr(j-1,i) = frd * fgmgp(j-1,i)
        ! Helium release
        hmgr(j-1,i) = frdh * hemgp(j-1,i)
        ! Helium release from IFBA
        IF (i == 2) CALL ZrB2(j-1)%SaveIFBARel
        ! Nitrogen release
        angr(j-1,i) = frdn * angi / (totl / deltaz(j-1))
        ! Water release
        ah2ogr(j-1,i) = frdh2o * h2omi / (totl / deltaz(j-1))
    END DO
    ang(j-1,2) = MAX((ang(j-1,1) - angr(j-1,2) + angr(j-1,1)), 0.0_r8k)
    ah2og(j-1,2) = MAX((ah2og(j-1,1) - ah2ogr(j-1,2) + ah2ogr(j-1,1)), 0.0_r8k)
    
    ! Capture rdot for printing out
    rdotwrt(j-1) = rdot
    !
    END SUBROUTINE fgasre
    !
    !
    !
    SUBROUTINE ans54_1982_FGR (PelAvgBu, flxfc, FGR_Elements)
    USE Conversions, ONLY : Pi, tfk, intoft, fttoin, oneday, BtuhtokW, kgtog, m3tocm3
    USE Variables, ONLY : ProblemTime, jst, qmpy, dt, prdct, ansd, den, ounit, crad, nr, im, bup, &
                          qaxnorm, ngasr, na, tfuelr, dp, it, j, dcoBOL, rdot
    USE FGR_Mesh, ONLY : ans54_node
    USE MatLib, ONLY : MatProp
    IMPLICIT NONE
    !> @brief
    !> The fission gas release is calculated inside the time step and gas pressure loops for each axial region.
    !> The fuel temperatures are calculated inside the axial loop.
    !> Only the diffusion parameter, ansd, needs to be saved for each radial and axial region every time step,
    !> ansd (along with prdct) is tri-dimensioned (fissiongasradial,axial,timestep)
    !> Although the local gas production rate and burnup are also needed they are calculated as they are needed.
    !> ans54 is called from fgasre.
    !> The ans standard calls for at least six radial regions and ten or more axial regions.
    !> The subroutine establishes the radial divisions but the number of axial regions is a user input.
    !> Also, the time step size should be 2000 MWd/mtU burnup or shorter.
    !> The diffusion parameters for iodine, cesium, and tellurium are not calculated by the code.
    !> @author
    !> This Subroutine (ans54) was coded by f e panisko and w n rausch of battelle,
    !> Pacific Northwest Laboratories, august 1979 (pnl-3077)
    !
    ! Input
    !
    ! it          - current time step number
    ! PelAvgBu       - axial local mid step value in MWd/mtU
    ! dcoBOL      - fabricated clad od, inches
    ! dp(j-1)     - fabricated fuel dia.,inches
    ! fuelTD      - theoretical density (g/cm3)
    ! flow        - fraction release predicted by the low temp stable model
    ! flowr       - fraction release predicted by the low temp radioactive model
    ! frden       - fraction of theoretical density
    ! j           - axial region counter for frapcon
    ! j-1         - axial region counter for ans54
    ! jst         - indicator for time dependent axial profile
    ! npow        - total no. of axial regions
    ! ngasr       - number of radial regions in the fuel, user input as ngasr
    ! im          - total no. of steps
    ! nr          - number of flux depression regions in the fuel
    ! ProblemTime - time history array, cumulative in seconds
    ! q           - axial power profile array
    ! qmpy        - time dependent power array
    ! sp          - specific power (megawatts per metric ton of fuel)
    ! powr(qc)    - fuel surface heat flux for an axial region
    ! rc(j-1)     - radius of the annulus
    ! tt and ts   - the fuel radial temp(F) and Dimension (in) arrays
    ! %pf         - weighting factor for the radioactive gas release fraction to compensate for 
    !               different production rates in different regions
    ! crad        - fuel node radii (in)
    ! tfuelr      - fuel node temperatures (F)
    ! nr          - number of thermal radial nodes
    !
    ! Output
    !
    ! rdot        - gas release fraction per axial node per cumulative time
    ! %release    - fraction of short-lived radioactive gases released
    !
    CLASS (ANS54_Node), DIMENSION(:,:), INTENT(INOUT) :: FGR_Elements
    INTEGER(ipk) :: n1, itime, i1, i2, i3, i4, i5, i6, jj, i, jx, ii, total
    REAL(r8k) :: totb, tempk, tempf, preex, x, y, summ, powr, rdott, pftot, frden, pfave, &
      &          flow, fbuaxl, fburad, buring, a1, a5, a6, delex, deltim, &
      &          rtime, xmu, taut, arg, a, z, c, p, e, g, frac, ii2, arg2, h, o, sp, flowr, fuelTD, temp
    INTEGER(ipk), PARAMETER :: nshortnuclides = 11
    REAL(r8k), INTENT(IN) :: PelAvgBu
    REAL(r8k), PARAMETER :: pi2 = pi ** 2
    REAL(r8k), PARAMETER :: pi4 = pi2 ** 2
    REAL(r8k), DIMENSION(2) :: tau, gtau
    REAL(r8k), DIMENSION(ngasr) :: f, flxfc, flxfacb
    !
    fuelTD = MatProp (Mat_Type='FUEL', Property='TDENSITY') * kgtog / m3tocm3
    frden = den / 100.0_r8k
    
    ! Check to see if case is within subroutine's limits
    IF (na < 11) WRITE (ounit,350) (na-1)
    
    dt(1) = 0.0_r8k
    DO n1 = 2, im
        ! dt(n1) is the time increment for step n1 (units are seconds)
        dt(n1) = ProblemTime(n1) - ProblemTime(n1-1)
    END DO
    ! j-1 and now i1 are the local axial region numbers
    i1 = j - 1
    ! check on the size of problem
    ! ** top of the radial region gas release loop ******************
    totb = 0.0_r8k
    !
    DO i2 = 1, ngasr
        prdct(i2,j-1,it) = flxfc(i2) * REAL(ngasr)
        tempf = terp(FGR_Elements(j-1,i2)%ansr,crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        ! ansd at 1400 deg.c =2.2e-10=d*EXP(-72300./(1673r))
        ! solving for d gives 0.61377
        ! so ansd(i2,i1,it) = 0.61377*EXP(-72300./(1.987*tempk))
        ansd(i2,i1,it) = 0.61377_r8k * EXP(-36386.0_r8k / tempk)
        ! obtain mid-time step pellet ring burnup
        fbuaxl = qmpy(1) * qaxnorm(j-1, jst(1)) * ProblemTime(1)
        fburad = qmpy(1) * qaxnorm(j-1, jst(1)) * prdct(i2,i1,1) * ProblemTime(1)
        IF (it > 2) THEN
            DO itime = 2, (it - 1)
                fbuaxl = fbuaxl + qmpy(itime) * qaxnorm(j-1,jst(itime)) * dt(itime)
                fburad = fburad + qmpy(itime) * qaxnorm(j-1,jst(itime)) * prdct(i2,i1,itime) * dt(itime)
            END DO
        END IF
        fbuaxl = fbuaxl + qmpy(it) * qaxnorm(j-1,jst(it)) * dt(it) / 2.0_r8k
        fburad = fburad + qmpy(it) * qaxnorm(j-1,jst(it)) * prdct(i2,i1,it) * dt(it) / 2.0_r8k
        buring = PelAvgBu * fburad / fbuaxl
        bup = PelAvgBu * prdct(i2,j-1,it)
        ! calculate diffusion coefficient for local burnup
        ansd(i2,i1,it) = ansd(i2,i1,it) * 100.0_r8k ** (buring / 28000.0_r8k)
        a6 = 0.0_r8k
        preex = 0.0_r8k
        f(i2) = 0.0_r8k
        ! when it eq 1 time is assumed to be zero so skip gas release
        IF (it == 1) EXIT
        ! *** top of the time step loop *********
        DO i3 = 2, it
            IF (dt(i3) >= 14400.0_r8k) THEN
                ! convert surface heat flux to kw/ft
                powr = (qmpy(i3) * BTUhtokW * qaxnorm(j-1,jst(i3))) * (dcoBOL * intoft) * pi
                tau(1) = 0.0_r8k
                DO i4 = i3, it
                    tau(1) = tau(1) + ansd(i2,i1,i4) * dt(i4)
                END DO
                tau(2) = 0.0_r8k
                tau(2) = tau(1) - ansd(i2,i1,i3) * dt(i3)
                a1 = 0.0_r8k
                ! The following cards are for the finite sum
                DO i5 = 1, 2
                    gtau(i5) = 0.0_r8k
                    IF (i5 /= 2 .OR. i3 /= it) THEN
                        IF (tau(i5) <= 0.1_r8k) THEN
                            gtau(i5) = 1.0_r8k - 4.0_r8k * SQRT(tau(i5) / pi) + 1.5_r8k * tau(i5)
                        ELSE
                            summ = 0.0_r8k
                            DO i6 = 1, 3
                                x = REAL(i6)
                                x = x * x
                                y = -(x * pi2 * tau(i5))
                                IF (y >= -200.0_r8k) summ = summ + EXP(y) / (x * x * pi4)
                            END DO
                            gtau(i5) = (1.0_r8k / (15.0_r8k * tau(i5)) - (6.0_r8k * summ / tau(i5)))
                        END IF
                    END IF
                END DO
                a1 = tau(1) * gtau(1) - tau(2) * gtau(2)
                a5 = a1 * prdct(i2,j-1,i3) * powr / ansd(i2,i1,i3)
                delex = prdct(i2,j-1,i3) * dt(i3) * powr
                ! The following card is to obviate the need for double precision
                IF (a5 > delex) a5 = delex
                a6 = a6 + a5
                preex = preex + delex
            END IF
        END DO
        ! *** bottom of the time step loop *********
        IF (preex == 0.0_r8k) THEN
            preex = 1.0_r8k
            a6 = 1.0_r8k
        END IF
        f(i2) = 1.0_r8k - a6 / preex
        totb = totb + preex
        flxfacb(i2) = preex
    END DO
    !
    IF (it > 1) THEN
        rdott = 0.0_r8k
        DO i = 1, ngasr
            flxfacb(i) = flxfacb(i) / totb
            rdott = rdott + flxfacb(i) * f(i)
        END DO
        rdot = rdott
        ! bottom of the radial region gas release loop ******************
        ! low temperature release
        flow = 0.7e-7_r8k * PelAvgBu
        IF (flow > rdot) rdot = flow
    END IF
    !
    ! The remainder of the subroutine is for the calculation of the radioactive gas release fraction
    ! for a series of different half-lives.
    ! The statements calculate the release averaged over the whole rod.
    ! The ans short half-lived release standard is valid only for when no previous buildup is present.  
    ! This requires a shutdown of a 4*half-life period of time. The Subroutine does not consider this problem.
    IF (j == 2) THEN
        IF (it == 1 .OR. jst(it) /= 1) THEN
            ! Calculate gas production factors to use as weighting factors for average release fraction for the rod
            pftot = 0.0_r8k
            DO jj = 1, (na - 1)
                DO i = 1, ngasr
                    FGR_Elements(jj,i)%pf = qaxnorm(jj,jst(it)) * flxfc(i)
                    pftot = pftot + FGR_Elements(jj,i)%pf
                END DO
            END DO
            pfave = pftot / ((na - 1) * ngasr)
            ! Normalize the power factor
            CALL FGR_Elements%pf_norm (pfave)
        END IF
        deltim = 0.5_r8k
        DO i = 1, nshortnuclides
            rtime = 1.0_r8k + (i-1) * deltim
            ! Half is the half lives of the isotopes ranging on a log scale from 10 to 1000000 seconds
            ANS54%half(i) = 10.0_r8k ** rtime
            ANS54%decay(i) = 1.0_r8k / ANS54%half(i)
            ANS54%release(i) = 0.0_r8k
        END DO
    END IF
    IF (ProblemTime(it) < oneday) RETURN
    DO i = 1, ngasr
        DO jx = 1, nshortnuclides
            xmu = ANS54%decay(jx) / ansd(i,j-1,it)
            taut = ansd(i,j-1,it) * ProblemTime(it)
            arg = taut * xmu
            arg = MIN(arg, 675.0_r8k)
            IF (taut - 0.1_r8k <= 0.0_r8k) THEN
                a = 3.0_r8k / (1.0_r8k - EXP(-arg))
                z = ERF(SQRT(xmu * taut))
                c = 2.0_r8k * SQRT(xmu * taut / pi) * EXP(-arg)
                p = (z - c) / SQRT(xmu)
                e = (1.0_r8k + xmu * taut) * EXP(-arg)
                g = (1.0_r8k - e) / xmu
                frac = a * (p - g)
            ELSE
                a = 1.0_r8k / TANH(SQRT(xmu))
                z = 3.0_r8k * (a / SQRT(xmu) - 1 / xmu)
                c = 6.0_r8k * xmu / (EXP(arg) - 1.0_r8k)
                p = 0.0_r8k
                DO ii = 1, 3
                    ii2 = ii * ii
                    arg2 = ii2 * pi2 * taut
                    IF (arg2 > 675.0_r8k) arg2 = 675.0_r8k
                    h = 1.0_r8k - EXP(-arg2)
                    o = ii2 * pi2 * (ii2 * pi2 + xmu)
                    p = p + h / o
                END DO
                frac = z - c * p
                ! frac is the release fraction at any one node(radial,axial,time step)
            END IF
            frac = frac * FGR_Elements(j-1,i)%pf
            ! release sums the fraction released for each axial node during a time step
            ANS54%release(jx) = ANS54%release(jx) + frac
        END DO
    END DO
    IF (j /= na) RETURN
    ! The following statements will be executed only once each time step
    total = ngasr * (na - 1)
    DO i = 1, nshortnuclides
        ANS54%release(i) = ANS54%release(i) / total
        ! convert surface heat flux to kw/ft
        powr = (qmpy(it) * BTUhtokW) * (dcoBOL * intoft) * pi
        ! convert kw/ft to mw/mtm (specific power)
        sp = powr * 0.001_r8k * (1.0_r8k / (fttoin * ((dp(j-1) / 2.0_r8k) ** 2 * pi)) * &
          &  1.0_r8k / (frden * fuelTD)) * 1.0e6_r8k * 6.102e-2_r8k
        flowr = (0.7e-7_r8k * SQRT(ANS54%decay(i)) + 2.0e-12_r8k * sp) / ANS54%decay(i)
        ANS54%release(i) = MAX(ANS54%release(i), flowr)
    END DO
    !
    ! The results are output by a call to gasplt
350 FORMAT (1x,100('*')/5x,'problem may not satisfy conditions for ans-5.4 fission gas release standard'/5x, &
      &                    'number of axial regions only',i3/1x,100('*'))
    !
    END SUBROUTINE ans54_1982_FGR
    !
    !
    !
    SUBROUTINE Massih_FGR (flxfc, FGR_Elements)
    USE Conversions, ONLY : tfk, sectoday, umtom, psitoatm
    USE Variables, ONLY : imox, den, sigfgr, igas, Power, crad, nr, ngasr, tfuelr, EOSNodeburnup, &
      &                   nplot, j, it, gasflg, delbp, press, grnsize, delhs, rdot, UO2, MOX_NFI, MOX_Halden
    USE Refabrication, ONLY : irefab
    USE FGR_Mesh, ONLY : massih_node
    IMPLICIT NONE
    !>@brief
    !> Subroutine Massih_FGR is called by fgasre and returns cumulative fission gas release for the axial region (rdot)
    !> This Subroutine is based on the solution to the booth diffusion problem by k. forsberg and a. r. massih, 
    !> journal of nuclear material. volume 135, 1985, pp. 140 to 148.
    !>@author
    !> This version was programmed by d.d. lanning and k. geelhood
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> june,19
    !
    ! Input
    !
    ! it      - current time step number
    ! j-1     - axial region number
    ! ngasr   - number of equal volume radial rings (must be greater than 10) for FGR calculations
    ! nr      - number of radial nodes
    ! gasflg  - convergence signal on gas pressure iteration
    ! EOSNodeBurnup - axial region burnup at End of step (MWd/mtU)
    ! delbp   - burnup increment (MWd/mtU)
    ! delhs   - time increment (sec)
    ! dp      - pellet outer diameter (inches)
    ! crad    - radii of nodes from fuel centerline (in)
    ! rc      - pellet inner diameter (inches)
    ! den     - pellet density (per cent theoretical density)
    ! press   - gas pressure in the rod (psi)
    ! grnsize - input grain size (effective diameter, microns)
    !
    ! major internal variables (all concentrations in moles/m^3)
    !
    ! gp      - concentration of produced fission gas
    ! gb      - concentration of gas at grain boundaries
    ! gg      - concentration of gas within grains
    ! grs     - concentration of gas re-solved
    ! rls     - concentration of gas released
    ! gs      - saturation concentraion
    ! d       - diffusion constant, m^2 per second
    ! brn     - ring-specific burnup, MWd/mtU
    ! fr      - ring-specific cumulative release fraction
    ! tfuelr  - fuel node temperatures (F)
    !
    ! Output
    !
    ! rdot    - pellet vol. average cumulative release fraction
    !
    CLASS (Massih_Node), DIMENSION(:,:), INTENT(INOUT) :: FGR_Elements
    INTEGER(ipk) :: i, i2
    REAL(r8k) :: frden, rpext, grn, tempf, tempk, delbrn, delprod, fit, ttest, bup, testr, bupm, bb1, d, dtest, rmoxmult, &
      &          deltau, dtau, term1, term2, term3, rq, delgg, delgb, factor, delgrs, rns, gs, delrls, flow, check, arg
    LOGICAL :: Refab_Step
    REAL(r8k), DIMENSION(4) :: f = 0.0_r8k, delg = 0.0_r8k
    REAL(r8k), DIMENSION(ngasr) :: flxfc, fr
    
    ! Convert density from % to fraction
    frden = den / 100.0_r8k
    ! rpext is pressure on bubbles, in atmospheres
    rpext = press * PSItoATM
    ! grn = grain size (radius) in meters (grnsize is fixed at 10 microns, so grn is fixed at 5 microns)
    grn = (grnsize / 2.0_r8k) * umtom
    IF (it == 1) RETURN
    !
    ! *******radial node loop********
    !
    rdot = 0.0_r8k
    DO i2 = 1, ngasr
        !
        ! ******calculate ring-average temperature (K)***********
        !
        tempf = terp(FGR_Elements(j-1,i2)%ansr,crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        !
        ! moles gas produced per cubic meter = MWd/mtU*mtU/m^3*stpcm^3/MWd*moles/stpcm^3
        ! = delbp*(frden*10.96*0.88)*31.*(1./22420) = delbp*frden*0.013336
        ! also multiply by flxfc(i2)*ngasr to distribute production according
        ! to the radial power distribution. similarly distribute the burnuup increment.
        !
        delbrn = delbp * flxfc(i2) * REAL(ngasr)
        FGR_Elements(j-1,i2)%burnup%new = FGR_Elements(j-1,i2)%burnup%old + delbrn
        delprod = delbp * frden * 0.013336_r8k * flxfc(i2) * REAL(ngasr)
        FGR_Elements(j-1,i2)%gp%new = FGR_Elements(j-1,i2)%gp%old + delprod
        ! fit = massih's recomended resolution rate (m/s) = b*lambda
        ! factor of 250 on resolution comes from fit to steady-state and power-ramp integral rod fgr data
        ! factor changed to 300 by KJ Geelhood on 11-14-02
        !
        fit = 1.84e-14_r8k * 300.0_r8k
        !
        ! resolution not permitted on extremely short time steps (< 1 day)
        ttest = delhs * sectoday
        IF (ttest < 1.0_r8k) fit = 0.0_r8k
        !
        ! calculate diffusion constant
        !
        ! calcualte burnup enhancment factor bb1
        ! Form and parameter values based on guidance from ans5.4 plus
        ! reference to low-and high-burnup fgr data for cases with fgr.
        ! Last change (brn-25000) changed to (brn-21000) made by CE Beyer on 5/29/97
        ! (bupm/35000) changed to (bupm/40000) by KJ Geelhood on 11-14-02
        !
        bup = FGR_Elements(j-1,i2)%burnup%new - 21000.0_r8k
        testr = 0.0_r8k
        bupm = MAX(bup, testr)
        bb1 = MIN(20000.0_r8k, 100.0_r8k ** (bupm / 40000.0_r8k))
        ! Factor of 1.15 on the activation energy (q/r) term comes from fit to steady-state integral rod fgr data.
        d = bb1 * 2.14e-13_r8k * EXP((-22884.0_r8k * 1.15_r8k) / MIN(tempk, 1850.0_r8k))
        ! Diffusion constant not permitted to fall below the low-temperature function proposed by massih/forsberg.
        dtest = 1.51e-17_r8k * EXP(-9508.0_r8k / tempk)
        IF (d < dtest) d = dtest
        ! Factor of 14.0 on diffusion constant comes from fit to steady-state and especially power-ramp fgr data.
        ! Factor changed to 12.0 on 11-14-02 by KJ Geelhood
        d = d * 12.0_r8k
        ! If fuel is MOX, multiply d by a factor of 1.75.  Change made by DD Lanning and KJ Geelhood on 2/12/03
        IF (IMOX == MOX_NFI .OR. IMOX == MOX_Halden) THEN
            rmoxmult = 1.75_r8k
            d = d * rmoxmult
        END IF
        ! Add on uncertainty
        IF (sigfgr > 0.0_r8k) THEN
            d = d * (1.0_r8k + sigfgr * 1.0_r8k)
        ELSE
            d = d / (1.0_r8k - sigfgr * 1.0_r8k)
        END IF
        !
        deltau = d * delhs
        dtau = deltau / grn ** 2
        ! term1, term2, term3, and rq refer to terms in equation
        ! number 45 in forsberg/massih, jnm 135 (with no re-solution,
        ! i.e., h4 = 0, and production constant througout the time step)
        ! f1,f2,f3,  and g1,g2,g3 are defined in equation 43.
        !
        ! alternate calculations shown here for f1, f2, and f3 are
        ! done to prevent underflows (i.e., numbers less
        ! than 1.e-38) or poor approximations (exponent arguments smaller than 4.0e-4_r8k absolute)
        DO i = 1, 4
            arg = b(i) * dtau
            IF (arg <= 4.0e-4_r8k) THEN
                f(i) = -arg
            ELSE IF (arg <= 85.0_r8k) THEN
                f(i) = EXP(-arg) - 1.0_r8k
            ELSE
                f(i) = -1.0_r8k
            END IF
        END DO
        
        term1 = 0.0_r8k
        DO i = 1, 4
            term1 = term1 + f(i) * a(i) / b(i)
        END DO
        
!        term1 = f1 * a1 / b1 + f2 * a2 / b2 + f3 * a3 / b3 + f4 * a4 / b4
        ! short-term approximation
        term2 = 2.2568_r8k * (dtau ** 1.5_r8k) - 1.5_r8k * (dtau ** 2)
        ! long-term approximation
        IF (dtau > 0.1_r8k) term2 = dtau - 0.0616_r8k * (1.0_r8k - EXP(-9.87_r8k * dtau))
        !
        term3 = 0.0_r8k
        DO i = 1, 4
            term3 = term3 + f(i) * FGR_Elements(j-1,i2)%g(i)%StdGrain%old
        END DO
        
        rq = delprod / (term2 - term1) / grn ** 2
        DO i = 1, 4
            delg(i) = -rq * grn ** 2 * (a(i) / b(i)) * f(i) + f(i) * FGR_Elements(j-1,i2)%g(i)%StdGrain%old
            FGR_Elements(j-1,i2)%g(i)%StdGrain%New = FGR_Elements(j-1,i2)%g(i)%StdGrain%old + delg(i)
        END DO
        
        delgg = SUM(delg)
        FGR_Elements(j-1,i2)%gg%StdGrain%New = FGR_Elements(j-1,i2)%gg%StdGrain%Old + delgg
        delgb = (grn ** 2) * rq * term2 - term3
        !
        ! Re-solution factor, derived from the forsberg/massih jnm 1
        factor = grn * fit / (3.0_r8k * d)
        !
        ! Re-solution partition of delgb done here.
        !
        delgrs = delgb * factor / (1.0_r8k + factor)
        delgb = delgb / (1.0_r8k + factor)
        FGR_Elements(j-1,i2)%gb%StdGrain%New = FGR_Elements(j-1,i2)%gb%StdGrain%Old + delgb
        FGR_Elements(j-1,i2)%grs%StdGrain%New = FGR_Elements(j-1,i2)%grs%StdGrain%Old + delgb
        ! calculation of the grain boundary saturation concentration, gs.
        ! saturation concentration in moles/m^2, assuming bubble size is 0.5 mi
        ! dihedral half-angle = 50 degrees, and surface tension = 0.6 j/m^2.
        rns = (5.7589e-9_r8k / tempk) * (2.4e6_r8k + rpext * 1.013e5_r8k)
        gs = 3.0_r8k * rns / (2.0_r8k * grn)
        IF ((tempk - FGR_Elements(j-1,i2)%Temp%Old) > 300.0_r8k .AND. Power(j-1) > 9.0_r8k .AND. ttest < 0.17_r8k) gs = gs * 0.5_r8k
        !
        ! test for grain boundary gas saturation
        ! if not saturated, no gas release
        ! if saturated, release all grain boundary gas and all resolved gas upon saturation
        ! then zero out grain boundary and re-solved gas inventories.
        !
        IF (FGR_Elements(j-1,i2)%gb%StdGrain%New <= gs) THEN
            delrls = 0.0_r8k
        ELSE
            delrls = FGR_Elements(j-1,i2)%gb%StdGrain%New + FGR_Elements(j-1,i2)%grs%StdGrain%New
            FGR_Elements(j-1,i2)%gb%StdGrain%New = 0.0_r8k
            FGR_Elements(j-1,i2)%grs%StdGrain%New = 0.0_r8k
        END IF
        !
        ! Update total gas release
        !
        FGR_Elements(j-1,i2)%rls%new = FGR_Elements(j-1,i2)%rls%old + delrls
        !
        ! If the option to suppress FGR before a given time step is active, set gas release to zero
        !
        IF (it <= igas) FGR_Elements(j-1,i2)%rls%new = 0.0_r8k
        !
        ! Compute gas release fraction
        !
        fr(i2) = FGR_Elements(j-1,i2)%rls%new / FGR_Elements(j-1,i2)%gp%new
        !
        ! Low-temperature gas release (taken from ans5.4 model)
        !
        flow = 7.0e-8_r8k * FGR_Elements(j-1,i2)%burnup%new
        IF (it >= irefab+1) flow = 7.0e-8_r8k * (FGR_Elements(j-1,i2)%burnup%new - FGR_Elements(j-1,i2)%brnrefab)
        IF (flow > fr(i2) .AND. it > igas) fr(i2) = flow
        
        check = (FGR_Elements(j-1,i2)%rls%new + FGR_Elements(j-1,i2)%gg%StdGrain%New + FGR_Elements(j-1,i2)%gb%StdGrain%New + &
          &      FGR_Elements(j-1,i2)%grs%StdGrain%New) / FGR_Elements(j-1,i2)%gp%new
        rdot = fr(i2) * FGR_Elements(j-1,i2)%burnup%new / EOSNodeburnup(j-1) / REAL(ngasr) + rdot
        !
        ! update the 'old' variables to the 'new' values, once per time step,
        ! when gas pressure convergence is acheived (gasflg = .TRUE.)
        Refab_Step = .FALSE.
        IF (it == irefab) Refab_Step = .TRUE.
        ! Update old values for next time step
        IF (gasflg) CALL FGR_Elements(j-1,i2)%Store (grn, Refab_Step, tempk)
        ! Assumes only standard grains
        FGR_Elements(j-1,i2)%gg%Net%New = FGR_Elements(j-1,i2)%gg%StdGrain%New
        FGR_Elements(j-1,i2)%gb%Net%New = FGR_Elements(j-1,i2)%gb%StdGrain%New
        FGR_Elements(j-1,i2)%grs%Net%New = FGR_Elements(j-1,i2)%grs%StdGrain%New
    END DO
    IF (it == irefab) EOSNodeburnuprefab(j-1) = EOSNodeburnup(j-1)
    ! Added gas release to account for high-burnup, low-power lwr gas release
    ! An additional 1 percent gas release is accumulated for every 10 GWd/MTU above 40 GWd/MTU
    IF (rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k .AND. it > igas) THEN
        IF (it < irefab + 1) THEN
            rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - 40000.0_r8k) / 10000.0_r8k
        ELSE
            rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - MAX(40000.0_r8k, EOSNodeburnuprefab(j-1))) / 10000.0_r8k
        END IF
    END IF
    !
    ! CS/FPI CAD/1467/FRAPCON
    ! Instrumentation Scanair
    !IF (nplot > 1) CALL FGR_Elements(j-1,:)%scangas (j-1)
    IF (nplot > 1) CALL scangas (FGR_Elements(j-1,:),j-1)
    ! CS/FPI
    !
    END SUBROUTINE Massih_FGR
    !
    !
    !
    SUBROUTINE FRAPFGR_FGR (flxfc, FGR_Elements)
    USE Conversions, ONLY : tfk, psitoatm
    USE Variables, ONLY : den, sigfgr, Power, TR_gasavail, crad, nr, ngasr, tfuelr, rc, dp, &
      &                   nplot, j, it, gasflg, delbp, delh, delhs, press, grnsize, rdot, EOSNodeburnup
    USE Refabrication, ONLY : irefab
    USE FGR_Mesh, ONLY : frapfgr_node
    IMPLICIT NONE
    !>@brief
    !> Subroutine frapfgr is called by fgasre and returns cumulative fission gas release for the axial region (rdot)
    !> as well as gas on the grain boundaries for initialization of the FRAPTRAN FGR model.
    !> This Subroutine is based on a modified solution to the booth diffusion problem in Ref. (1)
    !>@author
    !> this version was programmed by k.j geelhood
    !> Updated by Patrick Raynaud, NRC, on 07/01/2015
    !>@date
    !> October 23, 2007
    !
    ! Input
    !
    ! it      - current time step number
    ! j-1     - axial region number
    ! ngasr   - number of rings (must be greater than 10)
    ! nr      - number of radial nodes
    ! gasflg  - convergence signal on gas pressure iteration
    ! EOSNodeburnup - axial region burnup at end of step (MWd/mtU)
    ! delbp   - burnup increment (MWd/mtU)
    ! delh    - time increment (hr)
    ! dp(j-1) - pellet outer diameter (inches)
    ! crad    - radii of nodes from fuel centerline (in)
    ! rc      - pellet inner diameter (inches)
    ! den     - pellet density (per cent theoretical density)
    ! press   - gas pressure in the rod (psi)
    !
    ! Output
    !
    ! rdot    - pellet vol. average cumulative release fraction
    !
    ! Internal (all concentrations in moles/m^3)
    !
    ! gp      - concentration of produced fission gas
    ! gb      - concentration of gas at grain boundaries
    ! gg      - concentration of gas within grains
    ! grs     - concentration of gas re-solved
    ! rls     - concentration of gas released
    ! gs      - saturation concentraion
    ! d       - diffusion constant, m^2 per second
    ! brn     - ring-specific burnup, MWd/mtU
    ! fr      - ring-specific cumulative release fraction
    ! tfuelr  - fuel node temperatures (F)
    !
    ! Reference:
    !
    ! (1)  k. forsberg and a. r. massih, journal of nuclear material, volume 135, 1985, pp. 140 to 148.
    !
    CLASS (FRAPFGR_Node), DIMENSION(:,:), INTENT(INOUT) :: FGR_Elements
    INTEGER(ipk) :: i, i2, k2
    REAL(r8k) :: frden, rpext, grn, rdot2, tempf, tempk, delbrn, pow, grnfuel, rimedge, rimstart, delprod, d, bup, testr, &
      &          bupm, adr, bb1, bken, deltau, dtau, resolterm, term1, term2, term3, rq, delgg, dktest, delgb, testken, &
      &          rns, gs, delrls, arg, gbtot
    LOGICAL :: Refab_Step
    REAL(r8k), DIMENSION(4) :: f = 0.0_r8k, delg = 0.0_r8k
    REAL(r8k), DIMENSION(ngasr) :: flxfc, fr
    
    ! Convert density from % to fraction
    frden = den / 100.0_r8k
    ! rpext is pressure on bubbles, in atmospheres
    rpext = press * PSItoATM
    ! rpext = (400.0 * PSItoATM)
    ! grn = grain size (radius) in meters (fixed at 5 microns)
    grn = grnsize / 2.0_r8k * 1.0e-6_r8k
    IF (it == 1) THEN
        ! Initialize variables on time step 1
        DO i2 = 1, ngasr
            FGR_Elements(j-1,i2)%grn%old = grn
        END DO
        RETURN
    END IF
    
    ! *******radial node loop********
    rdot = 0.0_r8k
    rdot2= 0.0_r8k
    DO i2 = 1, ngasr
        ! ******calculate ring-average temperature (K)***********
        tempf = terp(FGR_Elements(j-1,i2)%ansr,crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        ! moles gas produced per cubic meter = MWd/mtU*mtU/m^3*stpcm^3/MWd*moles/stpcm^3
        ! = delbp*(frden*10.96*0.88)*31.*(1./22420) = delbp*frden*0.013336
        ! also multiply by flxfc(i2)*ngasr to distribute production according
        ! to the radial power distribution. similarly distribute the burnuup increment.
        delbrn = delbp * flxfc(i2) * REAL(ngasr)
        FGR_Elements(j-1,i2)%burnup%new = FGR_Elements(j-1,i2)%burnup%old + delbrn
        pow = Power(j-1) * flxfc(i2) * REAL(ngasr)
        ! Calculate Grain Growth using Khoruzhii Model JNM 265 p.112
        grnfuel = graingro(pow, delh, FGR_Elements(j-1,i2)%grn%old, tempk, dp(j-1), rc(j-1), den)
        ! Calculate Rim thickness from Manzel Optical Microscopy Data
        ! JNM 301 p. 170 if temperature is less than 1000 Deg. C
        IF (tempk > 1273.15_r8k) THEN
            FGR_Elements(j-1,i2)%restructure = 0.0_r8k
        ELSE 
            IF (EOSNodeburnup(j-1) > 30000.0_r8k) THEN
                FGR_Elements(j-1,ngasr)%rimthick = (1.439e-6_r8k * ((EOSNodeburnup(j-1) / 1000.0_r8k) ** 4.427_r8k)) / 25400.0_r8k
            ELSE
                FGR_Elements(j-1,ngasr)%rimthick = 0.0_r8k
            END IF
            ! Find edge and start of rim
            rimedge = dp(j-1) / 2.0_r8k - FGR_Elements(j-1,ngasr)%rimthick
            rimstart = dp(j-1) / 2.0_r8k - 2.0_r8k * FGR_Elements(j-1,ngasr)%rimthick
            IF (FGR_Elements(j-1,i2)%ansr >= rimedge) THEN
                FGR_Elements(j-1,i2)%restructure = 1.0_r8k
            ELSE IF (FGR_Elements(j-1,i2)%ansr >= rimstart) THEN
                FGR_Elements(j-1,i2)%restructure = (FGR_Elements(j-1,i2)%ansr - rimstart) / (rimedge - rimstart)
            ELSE
                FGR_Elements(j-1,i2)%restructure = 0.0_r8k
            END IF
        END IF
        grn = FGR_Elements(j-1,i2)%restructure * 0.35e-6_r8k + (1.0_r8k - FGR_Elements(j-1,i2)%restructure) * grnfuel
        ! Calculate porosity using model base on observations of high burnup fuel
        ! JNM 231p179, JNM 288p20, LWR Fuel Performance, 2000
        IF (FGR_Elements(j-1,i2)%burnup%new >= 60000.0_r8k) THEN
            FGR_Elements(j-1,i2)%porosity = 11.283_r8k * LOG(FGR_Elements(j-1,i2)%burnup%new / 1000.0_r8k) - 45.621_r8k
        ELSE
            FGR_Elements(j-1,i2)%porosity = 0.0_r8k
        END IF
        frden = den / 100.0_r8k - FGR_Elements(j-1,i2)%porosity / 100.0_r8k
        !
        delprod = delbp * frden * 0.013336_r8k * flxfc(i2) * REAL(ngasr)
        FGR_Elements(j-1,i2)%gp%new = FGR_Elements(j-1,i2)%gp%old + delprod
        ! Calculate diffusion for standard grains and restructured grains separately.
        DO k2 = 1, 2
            ! Diffusion constant
            IF (tempk < 1381.0_r8k) THEN
                d = 1.51e-17_r8k * EXP(-9508.0_r8k / MAX(tempk, 675.0_r8k))
            ELSE IF (tempk < 1650.0_r8k) THEN
                d = 2.14e-13_r8k * EXP(-22884.0_r8k / tempk)
            ELSE
                d = 7.14433e-10_r8k * EXP(-34879.0_r8k / MIN(tempk, 1850.0_r8k))
            END IF
            ! Grain size
            IF (k2 == 1) THEN
                grn = grnfuel
            ELSE
                grn = 0.075e-6_r8k
            END IF
            ! calcualte burnup enhancment factor (bb1).
            ! Form and parameter values based on guidance from ans5.4
            ! plus reference to low-and high-burnup fgr data for cases with fgr.
            IF (k2 == 1) THEN
                bup = FGR_Elements(j-1,i2)%burnup%new - 21000.0_r8k
                testr = 0.0_r8k
                bupm = MAX(bup, testr)
                adr = MIN(FGR_Elements(j-1,i2)%burnup%new, 12000.0_r8k) / 12000.0_r8k * 10.0_r8k
                bb1 = 10.0_r8k ** (bupm / 40000.0_r8k) + adr
                IF (bb1 > 49.81_r8k) bb1 = 49.81_r8k
            ELSE
                bken = FGR_Elements(j-1,i2)%burnup%new / 1000.0_r8k
                bb1 = MAX(1.0_r8k, -2.54132e-7_r8k * bken ** 4.0_r8k + 3.91491e-5_r8k * bken ** 3.0_r8k + &
                  &       2.32780e-2_r8k * bken ** 2.0_r8k - 3.92956_r8k * bken + 1.56649e2_r8k)
                IF (bken > 90.0_r8k) bb1 = bb1 * (0.0084_r8k * bken + 0.2523_r8k)
            END IF
            
            d = d * bb1
            ! User error function to account for effect of power on D
            d = d / (1.0_r8k + 3.0_r8k * 0.5_r8k * (1.0_r8k - ERF(pow - 3.0_r8k)))
            ! add on uncertainty
            IF (sigfgr > 0.0_r8k) d = d * (1.0_r8k + sigfgr * 1.0_r8k)
            IF (sigfgr < 0.0_r8k) d = d / (1.0_r8k - sigfgr * 1.0_r8k)
            SELECT CASE (k2)
            CASE (1)
                ! Assume all gas in restructured grains diffuses out.  Only resolved gas remaines in these grains.
                deltau = d * delhs
                dtau = deltau / (grn ** 2)
                ! term1, term2, term3, and rq refer to terms in equation number 45 in forsberg/massih, jnm 135
                ! (with no re-solution, i.e., h4 = 0, and production constant througout the time step)
                ! f1,f2,f3, and g1,g2,g3 are defined in equation 43.
                ! Alternate calculations shown here for f1, f2, and f3 are done to prevent underflows or poor approximations
                
                DO i = 1, 4
                    arg = b(i) * dtau
                    IF (arg <= 4.0e-4_r8k) THEN
                        f(i) = -arg
                    ELSE IF (arg <= 85.0_r8k) THEN
                        f(i) = EXP(-arg) - 1.0_r8k
                    ELSE
                        f(i) = -1.0_r8k
                    END IF
                END DO
                
                ! Resolution term as a function of temp and use in the calculation of term1, term2, and term3
                IF (tempk <= 1528.77_r8k) THEN
                    resolterm = MAX(1.0_r8k, 0.14009_r8k * EXP(0.00282_r8k * tempk))
                ELSE
                    resolterm = MAX(1.0_r8k, 22.976_r8k - 0.0082_r8k * tempk)
                END IF
                
                term1 = 0.0_r8k
                DO i = 1, 4
                    term1 = term1 + f(i) * a(i) / b(i)
                END DO
                term1 = term1 / resolterm
                
                ! Short-term approximation of term2
                term2 = (2.2568_r8k * dtau ** 1.5_r8k - 1.5_r8k * dtau ** 2) / resolterm
                ! Long-term approximation of term2
                IF (dtau > 0.1_r8k) term2 = (dtau - 0.0616_r8k * (1.0_r8k - EXP(-9.87_r8k * dtau))) / resolterm
                !
                SELECT CASE (k2)
                CASE (1)
                    term3 = 0.0_r8k
                    DO i = 1, 4
                        term3 = term3 + f(i) * FGR_Elements(j-1,i2)%g(i)%StdGrain%old
                    END DO
                    rq = delprod / (term2 - term1) / grn ** 2
                    DO i = 1, 4
                        delg(i) = -rq * grn ** 2 * (a(i) / b(i)) * f(i) + f(i) * FGR_Elements(j-1,i2)%g(i)%StdGrain%old
                    END DO
                CASE (2)
                    term3 = 0.0_r8k
                    DO i = 1, 4
                        term3 = term3 + f(i) * FGR_Elements(j-1,i2)%g(i)%RestrGrain%old
                    END DO
                    rq = delprod / (term2 - term1) / grn ** 2
                    DO i = 1, 4
                        delg(i) = -rq * grn ** 2 * (a(i) / b(i)) * f(i) + f(i) * FGR_Elements(j-1,i2)%g(i)%RestrGrain%old
                    END DO
                END SELECT
                
                delgg = SUM(delg)
                ! Make sure increase in gas in grains is <= increase in production + gb gas from previous time steps
                ! (sum of sources)
                IF (k2 == 1) THEN
                    dktest = delprod + FGR_Elements(j-1,i2)%gb%StdGrain%Old + FGR_Elements(j-1,i2)%grs%StdGrain%Old
                ELSE
                    dktest = delprod + FGR_Elements(j-1,i2)%gb%RestrGrain%Old + FGR_Elements(j-1,i2)%grs%RestrGrain%Old
                END IF
                IF (delgg > dktest) THEN
                    DO i = 1, 4
                        delg(i) = dktest * delg(i) / delgg
                    END DO
                    delgg = dktest
                END IF
                ! Makes sure that decrease in gas in grains is not greater than what was on grains in previous time step
                ! (gas on grains for this time step cannot be negative)
                IF (k2 == 1) THEN
                    dktest = -FGR_Elements(j-1,i2)%gg%StdGrain%Old
                ELSE
                    dktest = -FGR_Elements(j-1,i2)%gg%RestrGrain%Old
                END IF
                IF (delgg < dktest) THEN
                    DO i = 1, 4
                        delg(i) = dktest * delg(i) / delgg
                    END DO
                    delgg = dktest
                END IF
                !
                DO i = 1, 4
                    FGR_Elements(j-1,i2)%g(i)%StdGrain%New = FGR_Elements(j-1,i2)%g(i)%StdGrain%old + delg(i)
                END DO
                !
                FGR_Elements(j-1,i2)%gg%StdGrain%New = FGR_Elements(j-1,i2)%gg%StdGrain%Old + delgg
                delgb = delprod - delgg
                FGR_Elements(j-1,i2)%gb%StdGrain%New = FGR_Elements(j-1,i2)%gb%StdGrain%Old + delgb
            CASE (2)
                ! simple calculation for restructured grains
                ! resultion factor from Massih
                testken = 0.075e-6_r8k * 1.84e-14_r8k / 3.0_r8k / d
                FGR_Elements(j-1,i2)%gg%RestrGrain%New = FGR_Elements(j-1,i2)%gb%RestrGrain%Old * testken / (1.0_r8k + testken)
                delgb = delprod - (FGR_Elements(j-1,i2)%gg%RestrGrain%New - FGR_Elements(j-1,i2)%gg%RestrGrain%Old)
                FGR_Elements(j-1,i2)%gb%RestrGrain%New = FGR_Elements(j-1,i2)%gb%RestrGrain%Old + delgb
            END SELECT
            ! calculation of the grain boundary saturation concentration, gs.
            ! saturation concentration in moles / m^2, assuming bubble size is 0.5 mi
            ! dihedral half-angle = 50 degrees, and surface tension = 0.6 j / m^2.
            rns = (4.36e-8_r8k / tempk) * (0.48e6_r8k + rpext * 1.013e5_r8k)
            gs = 3.0_r8k * rns / (2.0_r8k * grn)
            ! Test for grain boundary gas saturation
            SELECT CASE (k2)
            CASE (1)
                IF (.NOT. FGR_Elements(j-1,i2)%openp(1)) THEN
                    IF (FGR_Elements(j-1,i2)%gb%StdGrain%New <= gs) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = FGR_Elements(j-1,i2)%gb%StdGrain%New - 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%gb%StdGrain%New = 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%openp(1) = .TRUE.
                    END IF
                ELSE
                    IF (FGR_Elements(j-1,i2)%gb%StdGrain%New <= gs * 0.65_r8k) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = FGR_Elements(j-1,i2)%gb%StdGrain%New - 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%gb%StdGrain%New = 0.65_r8k * gs
                    END IF
                END IF
                ! combine release from standard grains and restructured grains
                FGR_Elements(j-1,i2)%rls%new = FGR_Elements(j-1,i2)%rls%old + &
                  &                            delrls *(1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure))
            CASE (2)
                IF (.NOT. FGR_Elements(j-1,i2)%openp(2)) THEN
                    IF (FGR_Elements(j-1,i2)%gb%RestrGrain%New <= gs) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = FGR_Elements(j-1,i2)%gb%RestrGrain%New - 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%gb%RestrGrain%New = 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%openp(2) = .TRUE.
                    END IF
                ELSE
                    IF (FGR_Elements(j-1,i2)%gb%RestrGrain%New <= gs * 0.65_r8k) THEN
                        delrls = 0.0_r8k
                    ELSE
                        delrls = FGR_Elements(j-1,i2)%gb%RestrGrain%New - 0.65_r8k * gs
                        FGR_Elements(j-1,i2)%gb%RestrGrain%New = 0.65_r8k * gs
                    END IF
                END IF
                ! combine release from standard grains and restructured grains
                FGR_Elements(j-1,i2)%rls%new = FGR_Elements(j-1,i2)%rls%new + delrls * SQRT(FGR_Elements(j-1,i2)%restructure)
            END SELECT
        END DO
        gbtot = (1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * FGR_Elements(j-1,i2)%gb%StdGrain%New + &
          &      SQRT(FGR_Elements(j-1,i2)%restructure) * FGR_Elements(j-1,i2)%gb%RestrGrain%New
        ! Calculate fractional release and rdot
        fr(i2) = FGR_Elements(j-1,i2)%rls%new / FGR_Elements(j-1,i2)%gp%new
        rdot = rdot + fr(i2) * FGR_Elements(j-1,i2)%burnup%new / EOSNodeburnup(j-1) / REAL(ngasr)
        rdot2 = rdot2 + gbtot / FGR_Elements(j-1,i2)%gp%new * FGR_Elements(j-1,i2)%burnup%new / &
          &     EOSNodeburnup(j-1) / REAL(ngasr)
        ! Calculate gas available for Transient Release
        ! Grain boundary gas
                                  ! Unrestructured grain boundaries
        TR_gasavail(j-1,i2,1) = (((1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * (FGR_Elements(j-1,i2)%gb%StdGrain%new)) + &
                                  ! HBU restructured grain boundaries
          &                       (SQRT(FGR_Elements(j-1,i2)%restructure) * (FGR_Elements(j-1,i2)%gb%RestrGrain%new))) * &
          &                       (FGR_Elements(j-1,i2)%burnup%new / FGR_Elements(j-1,i2)%gp%new / EOSNodeburnup(j-1) / &
          &                        REAL(ngasr))
        ! Within HBU restructured grains
        TR_gasavail(j-1,i2,2) = (SQRT(FGR_Elements(j-1,i2)%restructure) * (FGR_Elements(j-1,i2)%gg%RestrGrain%new)) * &
          &                      FGR_Elements(j-1,i2)%burnup%new / FGR_Elements(j-1,i2)%gp%new / EOSNodeburnup(j-1) / REAL(ngasr)
        ! Within Unrestructured grains
        TR_gasavail(j-1,i2,3) = ((1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * &
          &                      (0.05_r8k * FGR_Elements(j-1,i2)%gg%StdGrain%new)) * &
          &                       FGR_Elements(j-1,i2)%burnup%new / FGR_Elements(j-1,i2)%gp%new / EOSNodeburnup(j-1) / REAL(ngasr)
        ! Combine gas in grains, on grain boundaries, and resolved from restructured and non-restrucutred grains for plotting
        FGR_Elements(j-1,i2)%gg%Net%New = (1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * &
          &                                FGR_Elements(j-1,i2)%gg%StdGrain%new + SQRT(FGR_Elements(j-1,i2)%restructure) * &
          &                                FGR_Elements(j-1,i2)%gg%RestrGrain%new
        FGR_Elements(j-1,i2)%gb%Net%New = (1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * &
          &                                FGR_Elements(j-1,i2)%gb%StdGrain%new + SQRT(FGR_Elements(j-1,i2)%restructure) * &
          &                                FGR_Elements(j-1,i2)%gb%RestrGrain%new
        FGR_Elements(j-1,i2)%grs%Net%New = (1.0_r8k - SQRT(FGR_Elements(j-1,i2)%restructure)) * &
          &                                 FGR_Elements(j-1,i2)%grs%StdGrain%new + SQRT(FGR_Elements(j-1,i2)%restructure) * &
          &                                 FGR_Elements(j-1,i2)%grs%RestrGrain%new
        ! Check for convergence on rod pressure
        Refab_Step = .FALSE.
        IF (it == irefab) Refab_Step = .TRUE.
        ! Update old values for next time step
        IF (gasflg) CALL FGR_Elements(j-1,i2)%Store (grnfuel, Refab_Step, tempk)
    END DO
    IF (it == irefab) EOSNodeburnuprefab(j-1) = EOSNodeburnup(j-1)
    !
    ! Added gas release to account for high-burnup, low-power lwr gas release
    ! An additional 1 percent gas release is accumulated for every 10 GWd/MTU above 40 GWd/MTU
    !
    IF (rdot <= 0.05_r8k .AND. EOSNodeburnup(j-1) > 40000.0_r8k) THEN
        IF (it < irefab + 1) THEN
            rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - 40000.0_r8k) / 10000.0_r8k
        ELSE
            rdot = rdot + 0.01_r8k * (EOSNodeburnup(j-1) - MAX(40000.0_r8k, EOSNodeburnuprefab(j-1))) / 10000.0_r8k
        END IF
    END IF
    !
    !IF (nplot > 1) CALL FGR_Elements(j-1,:)%scangas (j-1)
    IF (nplot > 1) CALL scangas (FGR_Elements(j-1,:),j-1)
    !
    CONTAINS
        REAL(r8k) FUNCTION graingro (pow, delh, grnin, tempK, dfs, rc, den)
        IMPLICIT NONE
        !>@brief
        !> Function graingo calculates the grain growth in UO2 fuel
        !>@author
        !> Ken Geelhood, PNNL
        !>@date
        !> October 23, 2007
        !
        ! Input
        !
        ! pow      - Local power, kW/ft
        ! delh     - Time incrament, hours
        ! grnin    - Grain size (radius) from previous time step, m
        ! tempK    - Temperature, K
        ! dfs      - Pellet outer diameter, in
        ! rc       - Pellet inner radius, in
        ! den      - Percent theoretical density
        !
        ! Output
        !
        ! graingro - Grain size (radius) at end of time step, m
        !
        ! Internal
        !
        ! fdot     - Fission rate (MW/tU)
        !
        ! Reference:
        ! 1) New Model of Equiaxial Growth of Uranium Dioxide Grains Under Irradiation Conditions
        !    S. Yu. Kurchatov, V. V. Likhanskii, and O. V. Khoruzhii. Atomic Energy, Vol. 84, No. 4, 1998
        !
        ! da / dt = K (1/a - 1/a_max - 1/a_irrad)
        !
        INTEGER(ipk) :: i, j, itime
        REAL(r8k) :: fdot, grn, grnold, k, inva, invamax, invair, dadt, dtime, frden
        REAL(r8k), INTENT(IN) :: pow, delh, grnin, tempK, dfs, rc, den
        REAL(r8k), PARAMETER :: F0_dot = 50.0_r8k ! Fission intensity, MW/ton
        REAL(r8k), PARAMETER :: T0 = 1400.0_r8k   ! Temperature
        REAL(r8k), PARAMETER :: A = 326.5         ! microns
        REAL(r8k), PARAMETER :: E = 5620.0_r8k    ! Temperature (K)
        !
        frden = den / 100.0_r8k
        ! Convert grain size in meters and radius to microns and diameter
        grnold = grnin * 1.0e6_r8k * 2.0_r8k
        ! Fission rate (MW/ton)
        fdot = pow * 0.6713_r8k / frden / (dfs ** 2.0_r8k - (rc * 2.0_r8k) ** 2.0_r8k)
        ! time increment
        itime = MAX(NINT(delh), 1)
        dtime = delh / REAL(itime)
        ! (micron^2 / hour)
        k = 5.24e7_r8k * EXP(-32100.0_r8k / tempK)
        ! Calculate 1 / a_max (micron)
        ! a_max = 2.23e3_r8k * EXP(-7620.0_r8k * temp_K)
        invamax = 1.0_r8k / 2.23e3_r8k * EXP(7620.0_r8k / tempK)
        ! Calculate 1 / a_irrad
        ! a_irrad = ((F0_dot / fdot) * (T0 / temp_K)) * A * EXP(-E / temp_K)
        invair = fdot / F0_dot * tempK / T0 * 1.0_r8k / A * EXP(E / tempK)
        
        DO i = 1, itime
            ! Calculate 1 / a
            ! a = grnold
            inva = 1.0_r8k / grnold
            dadt = MAX(0.0_r8k, k * (inva - invamax - invair))
            grn = grnold + dtime * dadt
            grnold = grn
        END DO
        ! Convert output from microns to meters (1.0e-6) and diameter to radius (0.5)
        graingro = grn * 1.0e-6_r8k * 0.5_r8k
        !
        END FUNCTION graingro
    END SUBROUTINE FRAPFGR_FGR
    !
    !
    !
    SUBROUTINE ans54_2011_FGR (PelAvgBu, flxfc, FGR_Elements)
    USE Conversions, ONLY : Pi, tfk, oneday, intoft, intocm, in3tocm3, fttoin, ftocm, btuhtoW, btuhtokW, kgtog, m3tocm3
    USE Variables, ONLY : jst, qmpy, prdct, ounit, crad, nr, qaxnorm, ngasr, na, rc, tfuelr, dp, it, j, dcoBOL, &
      &                   prdct, totl, deltaz, na, RB_axial, RB_rod, den, dt, im, ProblemTime, ansd, rdot, bup
    USE FGR_Mesh, ONLY : ans54_node
    USE MatLib, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> The subroutine ans54_2011 implements the ANS 5.4 2011 standard to calculate fission gas release.
    !> The source term with isotopic concentrations for the "gap activity" is provided in the output file.
    !>@author
    !> Dr. Gianluca Longoni, PNNL
    !> Updated by Ian Porter, NRC
    !>@date
    !> July 2015
    !> 02/07/2017
    !
    ! Input
    !
    ! PelAvgBu       - Mid time-step value for pellet average burnup at axial node, (MWd/mtU)
    !
    ! Output
    !
    ! Internal
    !
    ! D       - Diffusion coefficient, (cm^2/sec)
    ! Fdot    - Fission density, (fissions/(cm^3*sec))
    !
    !
    CLASS (ANS54_Node), DIMENSION(:,:), INTENT(INOUT) :: FGR_Elements
    INTEGER(ipk) :: ir, n, n1, itime, i1, i2, i3, i4, i5, i6, jj, i, jx, ii, total
    REAL(r8k), INTENT(IN) :: PelAvgBu
    REAL(r8k), PARAMETER :: pi2 = pi ** 2
    REAL(r8k), PARAMETER :: pi4 = pi2 ** 2
    REAL(r8k) :: dvoid, ansda, tempf, tempk, Tlink, S_V, fdot, D, Pave, Paxial, Pring, Vnode, Vrod, Vring, F_scaling, &
      &          totb, preex, x, y, summ, powr, rdott, pftot, frden, pfave, flow, fbuaxl, fburad, buring, a1, a5, a6, &
      &          delex, deltim, rtime, xmu, taut, arg, a, z, c, p, e, g, frac, ii2, arg2, h, o, sp, flowr, fuelTD, temp
    REAL(r8k), DIMENSION(2) :: tau, gtau
    REAL(r8k), DIMENSION(ngasr) :: flxfc, f, flxfacb
    REAL(r8k), DIMENSION(nnuclides) :: xrdot
    ! Precursor coefficients for radioactive nuclides
    ! Short-lived nuclides half-life < 6h
    ! 1 = Xe-135m
    ! 2 = Xe-137
    ! 3 = Xe-138
    ! 4 = Xe-139
    ! 5 = Kr-85m
    ! 6 = Kr-87
    ! 7 = Kr-88
    ! 8 = Kr-89
    ! 9 = Kr-90
    ! 10 = I-132
    ! 11 = I-134
    REAL(r8k), DIMENSION(nshortnuclides), PARAMETER :: alpha_s = [ 23.5_r8k,  1.07_r8k, 1.00_r8k, 1.00_r8k, &
      &                                                            1.31_r8k,  1.25_r8k, 1.03_r8k, 1.21_r8k, &
      &                                                            1.11_r8k, 137.0_r8k, 4.40_r8k ]
    REAL(r8k), DIMENSION(nshortnuclides), PARAMETER :: lambda_s = [ 7.55e-4_r8k, 3.02e-3_r8k, 8.19e-4_r8k, 1.75e-2_r8k, &
      &                                                             4.30e-5_r8k, 1.52e-4_r8k, 6.78e-5_r8k, 3.35e-3_r8k, &
      &                                                             2.15e-2_r8k, 8.44e-5_r8k, 2.20e-4_r8k ]
    ! Long-lived nuclides half-life > 6h < 60 days
    ! 1 = Xe-133
    ! 2 = Xe-135
    ! 3 = I-131
    ! 4 = I-133 (missing I-135 and Xe-131m)
    REAL(r8k), DIMENSION(nlongnuclides), PARAMETER :: alpha_l = [ 1.25_r8k, 1.85_r8k, 1.0_r8k, 1.21_r8k ]
    ! Decay constant per each nuclide in sec^-1
    REAL(r8k), DIMENSION(nlongnuclides), PARAMETER :: lambda_l = [ 1.53e-6_r8k, 2.12e-5_r8k, 9.98e-7_r8k, 9.26e-6_r8k ]
    ! Short and long-lived
    REAL(r8k), DIMENSION(nnuclides), PARAMETER :: alpha = [ alpha_s(1:nshortnuclides), alpha_l(1:nlongnuclides) ]
    REAL(r8k), DIMENSION(nnuclides), PARAMETER :: lambda = [ lambda_s(1:nshortnuclides), lambda_l(1:nlongnuclides) ]
    
    !
    fuelTD = MatProp (Mat_Type='FUEL', Property='TDENSITY') * kgtog / m3tocm3
    frden = den / 100.0_r8k
    
    ! Check to see if case is within subroutine's limits
    IF (na < 11) WRITE (ounit,350) (na-1)
    ! Time incriment, (s)
    dt(1) = 0.0_r8k
    DO n1 = 2, im
        ! dt(n1) is the time increment for step n1 (units are seconds)
        dt(n1) = ProblemTime(n1) - ProblemTime(n1-1)
    END DO
    ! j-1 and now i1 are the local axial region numbers
    i1 = j - 1
    ! check on the size of problem
    ! ** top of the radial region gas release loop ******************
    totb = 0.0_r8k
    !
    ! Rod average (Pave) and node average (Paxial) LHGR in W/cm at each time step
    Pave = (qmpy(it) * BTUhtoW) * (dcoBOL * intoft / ftocm) * pi
    Paxial = Pave * qaxnorm(j-1,jst(it))
    !
    ! Note: IP 7/13/2015
    !
    ! This does not need to be re-calculated every timestep becuse dp is as-fabricated
    ! Also, this does not take into account the dish or chamfer
    ! Why not use values calculated in Initialization, lines ~ 700 - 750?
    !
    ! Calculate fuel volumes (cm^3)
    dvoid = 2.0_r8k * rc(j-1)
    ansda = (dp(j-1) ** 2 - dvoid ** 2) / 4.0_r8k / REAL(ngasr)
    Vring = (deltaz(j-1) * fttoin) * pi * ansda * in3tocm3
    Vnode = (deltaz(j-1) * fttoin) * pi * (dp(j-1) / 2.0_r8k) ** 2 * in3tocm3
    Vrod = (totl * fttoin) * (pi / 4.0_r8k) * dp(j-1) ** 2 * in3tocm3
    !
    ! End of note
    !
    xrdot = 0.0_r8k
    
    
    GO TO 69
    
    ! This section is currently here for comparison to the previous ANS correlation.
    !        Final code will not have the GOTO statement above!
    
    DO i2 = 1, ngasr
        ! when it eq 1 time is assumed to be zero so skip gas release
        IF (it == 1) EXIT
        ! Calculate ring radial power factor
        prdct(i2,j-1,it) = flxfc(i2) * REAL(ngasr)
        ! Get ring temperature, (K)
        tempf = terp(FGR_Elements(j-1,i2)%ansr,crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)

        ! obtain mid-time step pellet ring burnup
        fbuaxl = qmpy(1) * qaxnorm(j-1, jst(1)) * ProblemTime(1)
        fburad = qmpy(1) * qaxnorm(j-1, jst(1)) * prdct(i2,i1,1) * ProblemTime(1)
        IF (it > 2) THEN
            DO itime = 2, (it - 1)
                fbuaxl = fbuaxl + qmpy(itime) * qaxnorm(j-1,jst(itime)) * dt(itime)
                fburad = fburad + qmpy(itime) * qaxnorm(j-1,jst(itime)) * prdct(i2,i1,itime) * dt(itime)
            END DO
        END IF
        fbuaxl = fbuaxl + qmpy(it) * qaxnorm(j-1,jst(it)) * dt(it) / 2.0_r8k
        fburad = fburad + qmpy(it) * qaxnorm(j-1,jst(it)) * prdct(i2,i1,it) * dt(it) / 2.0_r8k
        buring = PelAvgBu * fburad / fbuaxl
        
        bup = PelAvgBu * prdct(i2,j-1,it)
        ! Diffusion parameter (ansd)
        ! ansd at 1400 deg.c =2.2e-10=d*EXP(-72300./(1673r))
        ! solving for d gives 0.61377
        ! so ansd(i2,i1,it) = 0.61377*EXP(-72300./(1.987*tempk))
        !
        ! d' = d0/a^2 * exp(-Q/RT) * 100^(Bu/B)
        !
        ! Where
        !         Q = 72,300 cal/mol
        !         R = 1.987(2036) cal/K*mol
        !         B = 28,000 MWd/t
        !    d0/a^2 = 0.61 /sec
        ansd(i2,i1,it) = 0.61377_r8k * EXP(-36386.0_r8k / tempk) * 100.0_r8k ** (buring / 28000.0_r8k)
        
        a6 = 0.0_r8k
        preex = 0.0_r8k
        f(i2) = 0.0_r8k
        
        
        
        ! *** top of the time step loop *********
        DO i3 = 2, it
            ! See if timestep is greater than 4 hours
            IF (dt(i3) >= 14400.0_r8k) THEN
                ! convert surface heat flux to kw/ft
                powr = (qmpy(i3) * BTUhtokW * qaxnorm(j-1,jst(i3))) * (dcoBOL * intoft) * pi
                tau(1) = SUM(ansd(i2,i1,i3:it)*dt(i3:it))
                !
                tau(2) = tau(1) - ansd(i2,i1,i3) * dt(i3)
                a1 = 0.0_r8k
                ! The following cards are for the finite sum
                DO i5 = 1, 2
                    gtau(i5) = 0.0_r8k
                    IF (i5 /= 2 .OR. i3 /= it) THEN
                        IF (tau(i5) <= 0.1_r8k) THEN
                            ! Eq. (23)
                            gtau(i5) = 1.0_r8k - 4.0_r8k * SQRT(tau(i5) / pi) + 1.5_r8k * tau(i5)
                        ELSE
                            ! Eq. (24)
                            summ = 0.0_r8k
                            DO i6 = 1, 3
                                x = REAL(i6)
                                x = x * x
                                y = -(x * pi2 * tau(i5))
                                IF (y >= -200.0_r8k) summ = summ + EXP(y) / (x * x * pi4)
                            END DO
                            gtau(i5) = (1.0_r8k / (15.0_r8k * tau(i5)) - (6.0_r8k / tau(i5) * summ ))
                        END IF
                    END IF
                END DO
                a1 = tau(1) * gtau(1) - tau(2) * gtau(2)
                a5 = a1 * prdct(i2,j-1,i3) * powr / ansd(i2,i1,i3)
                delex = prdct(i2,j-1,i3) * dt(i3) * powr
                ! The following card is to obviate the need for double precision
                IF (a5 > delex) a5 = delex
                a6 = a6 + a5
                preex = preex + delex
            END IF
        END DO
        ! *** bottom of the time step loop *********
        IF (preex == 0.0_r8k) THEN
            preex = 1.0_r8k
            a6 = 1.0_r8k
        END IF
        f(i2) = 1.0_r8k - a6 / preex
        totb = totb + preex
        flxfacb(i2) = preex
    END DO
    !
    IF (it > 1) THEN
        rdott = 0.0_r8k
        DO i = 1, ngasr
            flxfacb(i) = flxfacb(i) / totb
            rdott = rdott + flxfacb(i) * f(i)
        END DO
        rdot = rdott
        ! bottom of the radial region gas release loop ******************
        ! low temperature release
        flow = 0.7e-7_r8k * PelAvgBu
        IF (flow > rdot) rdot = flow
    END IF
    !
    ! The remainder of the subroutine is for the calculation of the radioactive gas release fraction
    ! for a series of different half-lives.
    ! The statements calculate the release averaged over the whole rod.
    ! The ans short half-lived release standard is valid only for when no previous buildup is present.  
    ! This requires a shutdown of a 4*half-life period of time. The Subroutine does not consider this problem.
    IF (j == 2) THEN
        IF (it == 1 .OR. jst(it) /= 1) THEN
            ! Calculate gas production factors to use as weighting factors for average release fraction for the rod
            pftot = 0.0_r8k
            DO jj = 1, (na - 1)
                DO i = 1, ngasr
                    FGR_Elements(jj,i)%pf = qaxnorm(jj,jst(it)) * flxfc(i)
                    pftot = pftot + FGR_Elements(jj,i)%pf
                END DO
            END DO
            pfave = pftot / ((na - 1) * ngasr)
            ! Normalize the power factor
            CALL FGR_Elements%pf_norm (pfave)
        END IF
        deltim = 0.5_r8k
        DO i = 1, nshortnuclides
            rtime = 1.0_r8k + (i-1) * deltim
            ! Half is the half lives of the isotopes ranging on a log scale from 10 to 1000000 seconds
            ANS54%half(i) = 10.0_r8k ** rtime
            ANS54%decay(i) = 1.0_r8k / ANS54%half(i)
            ANS54%release(i) = 0.0_r8k
        END DO
    END IF
    IF (ProblemTime(it) < oneday) RETURN
    DO i = 1, ngasr
        DO jx = 1, nshortnuclides
            xmu = ANS54%decay(jx) / ansd(i,j-1,it)
            taut = ansd(i,j-1,it) * ProblemTime(it)
            arg = taut * xmu
            arg = MIN(arg, 675.0_r8k)
            IF (taut - 0.1_r8k <= 0.0_r8k) THEN
                a = 3.0_r8k / (1.0_r8k - EXP(-arg))
                z = ERF(SQRT(xmu * taut))
                c = 2.0_r8k * SQRT(xmu * taut / pi) * EXP(-arg)
                p = (z - c) / SQRT(xmu)
                e = (1.0_r8k + xmu * taut) * EXP(-arg)
                g = (1.0_r8k - e) / xmu
                frac = a * (p - g)
            ELSE
                a = 1.0_r8k / TANH(SQRT(xmu))
                ! R_B
                z = 3.0_r8k * (a / SQRT(xmu) - 1 / xmu)
                c = 6.0_r8k * xmu / (EXP(arg) - 1.0_r8k)
                p = 0.0_r8k
                DO ii = 1, 3
                    ii2 = ii * ii
                    arg2 = ii2 * pi2 * taut
                    IF (arg2 > 675.0_r8k) arg2 = 675.0_r8k
                    h = 1.0_r8k - EXP(-arg2)
                    o = ii2 * pi2 * (ii2 * pi2 + xmu)
                    p = p + h / o
                END DO
                frac = z - c * p
                ! frac is the release fraction at any one node(radial,axial,time step)
            END IF
            frac = frac * FGR_Elements(j-1,i)%pf
            ! release sums the fraction released for each axial node during a time step
            ANS54%release(jx) = ANS54%release(jx) + frac
        END DO
    END DO
    !IF (j /= na) RETURN
    ! The following statements will be executed only once each time step
    total = ngasr * (na - 1)
    DO i = 1, nshortnuclides
        ANS54%release(i) = ANS54%release(i) / total
        ! convert surface heat flux to kw/ft
        powr = (qmpy(it) * BTUhtokW) * (dcoBOL * intoft) * pi
        ! convert kw/ft to mw/mtm (specific power)
        sp = powr * 0.001_r8k * (1.0_r8k / (fttoin * ((dp(j-1) / 2.0_r8k) ** 2 * pi)) * &
          &  1.0_r8k / (frden * fuelTD)) * 1.0e6_r8k * 6.102e-2_r8k
        flowr = (0.7e-7_r8k * SQRT(ANS54%decay(i)) + 2.0e-12_r8k * sp) / ANS54%decay(i)
        ANS54%release(i) = MAX(ANS54%release(i), flowr)
    END DO
    
    
    
    
    
    
    
    
    
    ! Ian's implementation
    
    
69    DO ir = 1, ngasr
!        ! Ring fuel temperature (K)
        tempf = terp(FGR_Elements(j-1,ir)%ansr,crad(:,j-1),tfuelr,nr)
        tempk = tfk(tempf)
        
        ! Ring node average LHGR in W/cm
        prdct(ir,j-1,it) = flxfc(ir) * REAL(ngasr)
        Pring = prdct(ir,j-1,it) * Paxial
        
        ! Temperature (K) at which bubbles become interlinked to grain boundaries
        ! PelAvgBu is pellet average burnup, MWd/mtU
        IF (PelAvgBu * 1.0e-3_r8k <= 1.0e-2_r8k) THEN
            Tlink = 5000.0_r8k
        ELSE IF (PelAvgBu * 1.0e-3_r8k <= 18.2_r8k) THEN
            Tlink = 9800.0_r8k / LOG(176.0_r8k * PelAvgBu * 1.0e-3_r8k) + 273.0_r8k
        ELSE
            Tlink = 1434.0_r8k - 12.85_r8k * PelAvgBu * 1.0e-3_r8k + 273.0_r8k
        END IF
        
        ! Surface area to volume ratio (S_V, units = cm^-1)
        IF (tempk <= Tlink) THEN
            S_V = 120.0_r8k
        ELSE
            S_V = 650.0_r8k
        END IF
        
        ! Fission rate, fissions / (cm^3 * sec)
        Fdot = 4.0e10_r8k * Paxial / ((dp(j-1) * intocm) ** 2 - (dvoid * intocm) ** 2)
        ! Diffusion coefficient, (cm^2/s)
        D = 7.6e-7_r8k * EXP(-35000.0_r8k / tempk) + 1.41e-18_r8k * SQRT(Fdot) * EXP(-13800.0_r8k / tempk) + &
          & 2.0e-30_r8k * Fdot ! cm^2/sec
        DO n = 1, nnuclides
            IF (n <= nshortnuclides) THEN
                ! Short-lived nuclides half-life < 6h
                ! R_B
                xrdot(n) = xrdot(n) + ((Pring * Vring) / (Paxial * Vnode)) * S_V * SQRT(alpha(n) * D / lambda(n))
            ELSE
                ! Long-lived nuclides half-life > 6h < 60 days
                ! R_B
                F_scaling = ((alpha(n) * lambda(5)) / (lambda(n) * alpha(5))) ** 0.25_r8k
                xrdot(n) = xrdot(n) + ((Pring * Vring) / (Paxial * Vnode)) * F_scaling * S_V * &
                  &        SQRT(alpha(5) * D / lambda(5))
            END IF
        END DO
    END DO
    
    ! Gianluca's implementation:
!    RB_axial(:,j-1) = RB_axial(:,j-1) + ((Paxial * Vnode) / (Pave * Vrod)) * xrdot(:)
    ! Ian's implementation:
    RB_axial(:,j-1) = (Paxial * Vnode) / (Pave * Vrod) * xrdot(:)
    
    DO n = 1, nnuclides
        RB_rod(n,it) = SUM(RB_axial(n,:))
    END DO
!    rdot = SUM(RB_rod(:,it))
350 FORMAT (1x,100('*')/5x,'Problem may not satisfy conditions for ans-5.4 fission gas release standard'/5x, &
      &                    'number of axial regions only',i3/1x,100('*'))
    
    END SUBROUTINE ans54_2011_FGR
    !
    !
    !
    SUBROUTINE Update_Flux_Depression (Node, dv, rv, flxfc)
    USE Functions, ONLY : terp
    USE Variables, ONLY : ngasr, nr
    USE FGR_Mesh, ONLY : FGR_Node
    IMPLICIT NONE
    !>@brief
    !> Subroutine updates the flux depression arrays for FGR calculations
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/15/2016
    !
    ! Input
    !
    ! ngasr - # of radial nodes used in FGR solution
    ! nr    - # of radial nodes used in thermal solution
    ! ansr  - Radial node location used in FGR solution
    !
    ! Output
    !
    ! flxfc
    !
    ! Internal
    !
    ! ansdia - Diameter term
    !
    CLASS (FGR_Node), DIMENSION(:), INTENT(IN) :: Node
    INTEGER(ipk) :: r_fgr
    REAL(r8k) :: flxtot, ansdia
    REAL(r8k), DIMENSION(nr), INTENT(IN) :: dv, rv
    REAL(r8k), DIMENSION(ngasr), INTENT(OUT) :: flxfc
    
    flxtot = 0.0_r8k
    DO r_fgr = 1, ngasr
        ansdia = Node(r_fgr)%ansr * 2.0_r8k
        flxfc(r_fgr) = terp(ansdia, dv, rv, nr)
        flxtot = flxtot + flxfc(r_fgr)
    END DO
    IF (flxtot == 0.0_r8k) THEN
        flxfc(1:ngasr) = 0.0_r8k
    ELSE
        ! normalize ring radial power factors to total = 1.0
        flxfc(1:ngasr) = flxfc(1:ngasr) / flxtot
    END IF
    !
    END SUBROUTINE Update_Flux_Depression
    !f
    !
    !
    SUBROUTINE Burst_Release (AxNode, FGR_Elements)
    USE Conversions, ONLY : tfk
    USE Variables, ONLY : crad, tfuelr, nr, ngasr, TR_gasavail
    USE FGR_Mesh, ONLY : FRAPFGR_Node
    IMPLICIT NONE
    !>@brief
    !> This is the burst release fission gas model.
    !> At 2000F (1366.483K), all grain boundary gas is released.
    !> At 3300F (2088.706K), all gas in restructured grains of HB structure is released
    !> At 3300F (2088.706K), 5% of gas in unrestructured grains (non-HBS) is released
    !>@author
    !> Re-Written by Ian Porter, NRC
    !>@date
    !> 12/12/2016
    !
    ! Input
    !
    ! ansr    - Radius term for FGR nodalization
    ! crad    - Radius corresponding to thermal nodalization
    ! EOSTemp - Array of nodal temperatures (K)
    ! ngar    - # of radial gas release nodes
    !
    ! Output
    !
    CLASS (FRAPFGR_Node), DIMENSION(:), INTENT(INOUT) :: FGR_Elements
    INTEGER(ipk) :: i2
    INTEGER(ipk), INTENT(IN) :: AxNode
    REAL(r8k) :: rdot, temp_F, temp_K!, relfract
    
    ! Works only with FRAPFGR model
    rdot = 0.0_r8k
    DO i2 = 1, ngasr
        ASSOCIATE (Node => FGR_Elements(i2))
            ! Determine fuel node temperature
            temp_F = terp(Node%ansr, crad(:,AxNode), tfuelr, nr)
            temp_K = tfk(temp_F)
            ! Grain boundary gas release (NOTE: FRAPTRAN used 1950F rather than 2000F)
            IF (temp_F >= 1366.483_r8k .OR. Node%BurstRelease(1)) THEN
                rdot = rdot + TR_gasavail(AxNode,i2,1)
                Node%BurstRelease(1) = .TRUE.
            END IF
            ! Restructured grains (matrix) gas release
            IF (temp_F >= 2088.706_r8k .OR. Node%BurstRelease(2)) THEN
                rdot = rdot + TR_gasavail(AxNode,i2,2)
                Node%BurstRelease(2) = .TRUE.
            END IF
            ! 5% of gas in unrestructured grains (matrix) should be released
            IF (temp_F >= 2088.706_r8k .OR. Node%BurstRelease(3)) THEN
                rdot = rdot + TR_gasavail(AxNode,i2,3)
                Node%BurstRelease(3) = .TRUE.
            END IF
        END ASSOCIATE
    END DO
    
    ! These need to be updated
    ! :molrel & relfract
    ! Moles of FGR released
!    molrel(AxNode) = rdot * fmgp(AxNode)
    ! Release fraction
!    relfract = SUM(molrel) / SUM(fmgp)
    
    ! Calculate released gas in cm3 and convert to moles (22.412 liter/g-mole)
!    gmolxe = relfract * prodXe / 1000.0_r8k / 22.412_r8k
!    gmolkr = relfract * prodKr / 1000.0_r8k / 22.412_r8k
    ! Add fission gas release to TotalGasMoles
!    TotalGasMoles = GasMoles0 + gmolxe + gmolkr
    ! Update moles of each species of gas
!    gases(1) = gsmol0(1) / TotalGasMoles
!    gases(2) = gsmol0(2) / TotalGasMoles
!    gases(3) = (gsmol0(3) + gmolkr) / TotalGasMoles
!    gases(4) = (gsmol0(4) + gmolxe) / TotalGasMoles
!    gases(5) = gsmol0(5) / TotalGasMoles
!    gases(6) = gsmol0(8) / TotalGasMoles ! Switched to model Nitrogen
!    gases(7) = gsmol0(6) / TotalGasMoles
!    gases(8) = gsmol0(7) / TotalGasMoles

!    From FRAPCON-to-FRAPTRAN file:
!
!    WRITE (ftunit,*) ngasr
!    WRITE (ftunit,241) (ansr(AxNode),AxNode=1,ngasr)
!    DO i2 = 1, nt
!        WRITE (ftunit,241) (TR_gasavail(AxNode,i2,1),AxNode=1,ngasr)
!        WRITE (ftunit,241) (TR_gasavail(AxNode,i2,2+3),AxNode=1,ngasr)
!        WRITE (ftunit,242) fgmgp(i2,2)
!    END DO
!241 FORMAT(50(2x,e11.4))
!242 FORMAT(e11.4)
!    
!     From FRAPTRAN restart file read:
!    
!     READ(fcunit,*) (ansr(j), j=1,ngasr)
!    DO k = 1, naxnfs
!        READ(fcunit,*) (Frapcon_gasavail1(k,j), j=1,ngasr)
!        READ(fcunit,*) (Frapcon_gasavail2(k,j), j=1,ngasr)
!        READ(fcunit,*) Frapcon_fmgp(k)
!    END DO
    
    END SUBROUTINE Burst_Release
    !
    !
    !
    SUBROUTINE totgas
    USE Conversions, ONLY : Pi, in3tocm3, fttoin, kgtog, m3tocm3
    USE Variables, ONLY : acmfg, acmn2, acmhe, acmh2, acmh2o, amgpt, hmgpt, he_ifba, imox, den, EOSNodeburnup, dp, &
      &                   deltaz, amfhe, amfh2, amfn2, amfarg, amfkry, amfxe, addgmles, angr, fmgr, hmgr, &
      &                   tn2fr, fgmgp, hemgp, ang, gasmo, nread, gases, it, kryin, h2in, th2ofr, airin, angi, &
      &                   an2in, argin, fgin, hein, h2omi, xein, tfgfr, thefr, amffg, sgapf, h2oin, ah2ogr, &
      &                   gmlesAsFab, gmlesReleased, jmin, jmax, UO2, MOX_NFI, MOX_Halden
    USE Refabrication, ONLY : irefab
    USE Mesh, ONLY : ZrB2
    USE MatLib, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> The Subroutine totgas calculates the cumulative gas release of fission gas, helium, and nitrogen for the entire rod.
    !> The total moles of gas and mole fractions are also computed.
    !>@author
    !> coded for frapcon by g a berna in october 1977 using the routine of the same name in fraps
    !
    ! Input
    !
    ! airin         - initial moles of air in the rod
    ! amfh2         - absolute mole fraction of hydrogen
    ! ang           - cumulative n2 concentration in fuel per power step (moles)
    ! angi          - initial nitrogen concentration in the fuel (moles)
    ! angr          - nitrogen release per node & power step (moles)
    ! an2in         - initial moles of n2 in the rod
    ! argin         - initial moles of argon in the rod
    ! EOSNodeburnup - burnup at each axial node at End of step (MWd/mtu)
    ! fgin          - initial moles of fission gas in the rod
    ! fgmgp         - fission gas production for each node (moles)
    ! fmgr          - cumulative fission gas release per node & time step(moles)
    ! fuelTD        - theoretical density (g/cm3)
    ! gases         - mole fractions of gas constituents
    !             1 - helium
    !             2 - argon
    !             3 - krypton
    !             4 - xenon
    !             5 - hydrogen
    !             6 - nitrogen
    !             7 - Air
    !             8 - water vapor
    ! hein          - initial moles of helium in the rod
    ! hemgp         - helium  production for each node (moles)
    ! hmgr          - cumulative helium release per node & time step (moles)
    ! h2omi         - initial water content in the fuel (moles)
    ! it            - power-time step index
    ! kryin         - initial moles of krypton in the rod
    ! ngasr         - number of radial segments for fission gas release model
    ! nread         - restart Read index
    ! xein          - initial moles of xenon in the rod
    ! fuelTD        - material theoretical density (g/cc)
    !
    ! Output
    !
    ! acmfg         - cumulative fission gas release (gram-moles)
    ! acmn2         - cumulative nitrogen release (gram-moles)
    ! acmhe         - cumulative helium release (gram-moles)
    ! acmh2         - cumulative hydrogen release (gram-moles)
    ! acmh2o        - cumulative water vapor release (gram-moles)
    ! amffg         - absolute mole fraction of fission gas (gram-moles)
    ! amfhe         - absolute mole fraction of helium
    ! amfkry        - absolute mole fraction of krypton
    ! amfn2         - absolute mole fraction of nitrogen
    ! amfxe         - absolute mole fraction of xenon
    ! amgpt         - cumulative fission gas production (gm-moles)
    ! angt          - nitrogen consentration (gm-moles)
    ! gasmo         - gram-moles of gas in rod
    ! hmgpt         - cumulative helium production (gm-moles)
    ! tfgfr         - cumulative fission gas fraction release
    ! thefr         - cumulative helium fraction release
    ! th2ofr        - cumulative water vapor fraction release
    ! tn2fr         - cumulative nitrogen fraction release
    !
    INTEGER(ipk) :: il, jj, lt, i
    REAL(r8k) :: factor, sprod, frac, volume, prod, fgr, fhr, amfair, fuelTD, angt
    REAL(r8k), DIMENSION(2) :: amgp, amgph, amgr, amgrh, amgrn2, amgrh2o, fjj, fjjh
    !
    fuelTD = MatProp (Mat_Type='FUEL', Property='TDENSITY') * kgtog / m3tocm3
    !
    il = MAX(it - 1, 1)
    IF (nread == 1) THEN
        il = it
        IF (it <= 3) THEN
            amgp(1) = 0.0_r8k
            amgph(1) = 0.0_r8k
            fjjh(1) = 0.0_r8k
            fjj(1) = 0.0_r8k
            amgrn2(1) = 0.0_r8k
            amgrh2o(1) = 0.0_r8k
        END IF
    END IF
    ! **********
    ! ans-5.4 modifications
    ! factor = conversion from mwd to moles fission gas
    ! frac   = fraction gas release at each axial node-cumulative
    ! prod   = moles fission gas produced at each axial node-cumulative
    ! volume = volume of fuel in axial node
    factor = sgapf / 100.0_r8k *  4.477e-3_r8k
    sprod = 0.0_r8k
    DO jj = jmin, jmax
        frac = fmgr(jj-1,2) / fgmgp(jj-1,2)
        volume = pi * ((dp(jj-1) / 2.0_r8k) ** 2 * (deltaz(jj-1) * fttoin)) * in3tocm3
        prod = volume * den / 100.0_r8k * fuelTD * 0.88_r8k / 1.0e6_r8k * EOSNodeburnup(jj-1) * factor
        sprod = sprod + prod
        fmgr(jj-1,2) = frac * prod
    END DO
    ! **********
    DO lt = il, it
        i = 2 + lt - it
        amgr(i) = 0.0_r8k
        amgrh(i) = 0.0_r8k
        amgrn2(i) = 0.0_r8k
        amgrh2o(i) = 0.0_r8k
        angt = 0.0_r8k
        amgp(i) = 0.0_r8k
        amgph(i) = 0.0_r8k
        DO jj = jmin, jmax
            amgr(i) = amgr(i) + fmgr(jj-1,i)
            amgrh(i) = amgrh(i) + hmgr(jj-1,i)
            amgp(i) = amgp(i) + fgmgp(jj-1,i)
            amgph(i) = amgph(i) + hemgp(jj-1,i)
            angt = angt + ang(jj-1,2)
            amgrn2(i) = amgrn2(i) + angr(jj-1,i)
            amgrh2o(i) = amgrh2o(i) + ah2ogr(jj-1,i)
        END DO
        fjj(i) = amgr(i) / amgp(i)
        fjjh(i) = amgrh(i) / amgph(i)
    END DO
    IF (it == 1) THEN
        amgpt(1) = amgp(2)
        hmgpt(1) = amgph(2)
        acmfg(it) = amgr(2)
        acmhe(it) = amgrh(2)
        acmn2(it) = amgrn2(2)
        acmH2O(it) = amgrh2o(2)
    ELSE
        amgpt(it) = amgpt(it-1) + amgp(2) - amgp(1)
        amgpt(it) = sprod
        hmgpt(it) = hmgpt(it-1) + amgph(2) - amgph(1)
        ! Calculation of cumulative gas releases in gram-moles
        ! Fission gas release
        fgr = 2.0_r8k * (fjj(2) * amgp(2) - fjj(1) * amgp(1)) / (amgp(2) * (1.0_r8k - fjj(2)) + amgp(1) * (1.0_r8k - fjj(1)))
        acmfg(it) = amgr(2)
        IF (acmfg(it) > amgpt(it)) acmfg(it) = amgpt(it)
        ! Helium release
        fhr = 2.0_r8k * (fjjh(2) * amgph(2) - fjjh(1) * amgph(1)) / (amgph(2) * (1.0_r8k - fjjh(2)) + &
          &   amgph(1) * (1.0_r8k - fjjh(1)))
        IF (it == irefab + 1) acmhe(it-1) = 0.0_r8k
        acmhe(it) = acmhe(it-1) + (hmgpt(it-1) + hmgpt(it) - 2.0_r8k * acmhe(it-1)) * fhr / (fhr + 2.0_r8k)
        IF (acmhe(it) > hmgpt(it)) acmhe(it) = hmgpt(it)
        ! Nitrogen release
        acmn2(it) = MIN((acmn2(it-1) + amgrn2(2) - amgrn2(1)), angi)
        ! Water release
        acmH2O(it) = MIN((acmH2O(it-1) + amgrh2o(2) - amgrh2o(1)), h2omi)
    END IF
    ! calculation of total moles of gas in gap
    he_ifba(it) = SUM(ZrB2(jmin-1:jmax-1)%IFBArel)
    ! Account for additional gmles of gas supplied by user, assuming that the additional moles of gas are Helium
    IF (it > 1) hein = hein + addgmles(it) - addgmles(it-1)
    ! Calculate total # of gas moles in rod (sum of as-fabricated and released)
    gmlesAsFab = hein + argin + fgin + h2in + an2in + airin + h2oin
    gmlesReleased = acmfg(it) + acmhe(it) + acmn2(it) + acmh2(it) + acmh2o(it) + he_ifba(it)
    gasmo(it-1) = gmlesAsFab + gmlesReleased
    ! Calculation of mole fractions of gap gases
    amffg = (acmfg(it) + fgin) / gasmo(it-1)
    amfair = airin / gasmo(it-1)
    amfn2 = (acmn2(it) + an2in) / gasmo(it-1)
    amfh2 = h2in / gasmo(it-1)
    gases(1) = (acmhe(it) + hein + he_ifba(it)) / gasmo(it-1)
    gases(2) = argin / gasmo(it-1)
    SELECT CASE (imox)
    CASE (UO2) ! Xe/Kr ratio for UO2 is 5.67
        gases(3) = (0.15_r8k * acmfg(it) + kryin) / gasmo(it-1)
        gases(4) = (0.85_r8k * acmfg(it) + xein) / gasmo(it-1)
    CASE (MOX_NFI, MOX_Halden) ! Xe/Kr ratio for MOX is 16.0
        gases(3) = (0.0588_r8k * acmfg(it) + kryin) / gasmo(it-1)
        gases(4) = (0.9412_r8k * acmfg(it) + xein) / gasmo(it-1)
    END SELECT
    gases(5) = amfh2
    gases(6) = amfn2
    gases(7) = amfair
    gases(8) = (acmH2O(it) + h2oin) / gasmo(it-1)
    amfhe = gases(1)
    amfarg = gases(2)
    amfxe = gases(4)
    amfkry = gases(3)
    ! calculation of cumulative fraction releases
    ! Fission Gas
    IF (ABS(amgpt(it)) > 0.0_r8k) tfgfr = acmfg(it) / amgpt(it)
    ! Helium
    IF (ABS(hmgpt(it)) > 0.0_r8k) thefr = acmhe(it)/ hmgpt(it)
    ! Helium from B-10 reaction
    ! Note: It is assumed that all helium produced from Rxn is released
    ! Nitrogen in the fuel
    IF (ABS(angi) > 0.0_r8k) tn2fr = acmn2(it) / angi
    ! Water in the fuel
    IF (ABS(h2omi) > 0.0_r8k) th2ofr = acmH2O(it) / h2omi
    !
    END SUBROUTINE totgas
    !
    !
    !
    SUBROUTINE gasplt
    USE Conversions, ONLY : sectoday
    USE Variables, ONLY : ProblemTime, ounit, it
    IMPLICIT NONE
    !>@brief
    !> gasplt should be used to output the fraction of shortlived radioactive gases released as predicted
    !> by the ANS-5.4 gas release model
    !>@author
    !> coded by w n rausch of battelle-northwest
    !
    ! Input
    !
    ! %release        - Fraction of released short-lived radioactive gases
    ! it              - current time step number + 1
    ! ProblemTime(it) - current time in seconds
    !
    ! Output
    !
    ! h      - half-life array
    ! td     - current time in days
    !
    INTEGER(ipk) :: i, j, k
    REAL(r8k) :: deltim, rtime, td, dif, p
    REAL(r8k), DIMENSION(nshortnuclides) :: h
    CHARACTER(LEN=5), DIMENSION(nshortnuclides) :: s, r
    !
    deltim = 0.5_r8k
    DO i = 1, nshortnuclides
        rtime = 1.0_r8k + (i - 1) * deltim
        h(i) = 10.0_r8k ** rtime
    END DO
    td = ProblemTime(it) * sectoday
    WRITE (ounit,180) (it-1), td
    WRITE (ounit,260)
    ! output table of half-lives vs fraction released
    WRITE (ounit,190) (h(i),i=1,nshortnuclides),(ANS54%release(i),i=1,nshortnuclides)
    WRITE (ounit,200)
    ! initialize plot to blanks
    ! r will be used to save values of s(symbol)
    dif = 1.0_r8k / 40.0_r8k
    DO i = 2, 10
        s(i) = '     '
        r(i) = s(i)
    END DO
    s(1) = '.    '
    s(11) = '.    '
    r(1) = s(1)
    r(11) = s(11)
    ! begin plot calculations one line at a time
    DO i = 1, 39
        p = 1.0_r8k - i * dif
        DO j = 1, nshortnuclides
            IF (ANS54%release(j) >= p) s(j) = '*    '
        END DO
        IF (i /= 20) THEN
            WRITE (ounit,210) (s(k),k=1,nshortnuclides)
        ELSE
            ! print label for vertical axis
            WRITE (ounit,220) (s(k),k=1,nshortnuclides)
        END IF
        DO j = 1, nshortnuclides
            s(j) = r(j)
        END DO
    END DO
    DO j = 1, 10
        ! print bottom axis of plot
        s(j) = '*....'
    END DO
    s(11) = '*    '
    WRITE (ounit,230) (s(j),j=1,nshortnuclides)
    ! print label for horizontal axis
    WRITE (ounit,240) (h(j),j=1,nshortnuclides,2)
    WRITE (ounit,250)
    !
180 FORMAT (/50x,'fraction radioactive gases released'//55x,'time step = ', &
      &     i3/55x,'time at end of step =',f6.1,' days'/)
190 FORMAT (/5x,'half-life(sec)',5x,11(1pe10.1),/5x,'fraction'/5x,'released',11x,11(1pe10.2))
200 FORMAT (/135('*')//40x,'1.0.',51('.'))
210 FORMAT (44x,11a5)
220 FORMAT (16x,'fraction released',7x,'0.5.',11a5)
230 FORMAT (40x,'0.0.',11a5)
240 FORMAT (44x,6('.',9x)/37x,3f10.0,2x,3f10.0)
250 FORMAT (/62x,'half-life(sec)'////)
260 FORMAT (29x,'release fraction - fraction of non-decayed inventory that resides in the gap'//135('*'))
    !
    END SUBROUTINE gasplt
    !
END MODULE FissionGas
