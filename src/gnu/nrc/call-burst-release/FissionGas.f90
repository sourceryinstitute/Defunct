MODULE FissionGas
    USE Kinds
    USE Functions, ONLY : terp
    USE Variables, ONLY : ounit, na, rc, ngasr, ANS54_1982, Massih, FRAPFGR, ANS54_2011
    USE FGR_Mesh, ONLY : scangas, IFBA_Coating
    IMPLICIT NONE
    
    PRIVATE
!   PUBLIC :: gaspro, fgasre, gasplt, totgas

CONTAINS
    
    SUBROUTINE fgasre
    USE Conversions, ONLY : tfk, Pi
    USE Variables, ONLY : ProblemTime, ngasmod, tfuelr, nr, ngasr, crad, deltaz, na, angr, fmgr, hmgr, hemgp, fgmgp, Power, &
      &                   ang, rdotwrt, it, j, totl, angi, delbp, gasflg, nvoid, rdot, ah2ogr, ah2og, h2omi, EOSNodeBurnup
    USE Mesh, ONLY : FGR_Elements, ZrB2
    USE FGR_Mesh, ONLY : ans54_node, Massih_Node, FRAPFGR_node, fgr_node
    IMPLICIT NONE
    
    INTEGER(ipk) :: i, l, il, n, m, nrings
    REAL(r8k) :: coeff, coef1, PelAvgBu, fl, flh, fnn, rr, rpstn, frd, frdh, frdn, flh2o, frdh2o
    REAL(r8k), DIMENSION(2) :: times
    REAL(r8k), DIMENSION(ngasr) :: flxfc
    REAL(r8k), DIMENSION(nr) :: dplh, dpn, dph2o, dv, rv
    REAL(r8k), DIMENSION(nr,2) :: fh, fn, fh2o
    !
   !IF (.NOT. ALLOCATED(EOSNodeburnuprefab)) THEN
   !    ALLOCATE (EOSNodeburnuprefab(1:na))
   !    EOSNodeburnuprefab = 0.0_r8k
   !END IF
    
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
                  CALL Burst_Release (j-1, FGR_Elements(j-1,:))
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
END MODULE FissionGas
