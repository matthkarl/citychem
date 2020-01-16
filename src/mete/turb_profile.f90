! <turb_profile.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************! 
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Gr�nskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Sl�rdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by H�vard Sl�rdal
!*
!* Gr�nskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Gr�nskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Sl�rdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Sl�rdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Gr�nskei, K.E. (1992) Spredningsberegninger for on-line overv�king i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestr�m, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Sl�rdal, L.H., Guerreiro, C., Gram, F., Gr�nskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      subroutine turb_profile(ANALYTIC,G,D_ADIAB,KAPPA,ZU2,U2,ZU1,U1, &
                              ZT2,T2,ZT1,T1,LOWER_LIM_L,UST,TST,CL)

! ----------------------------------------------------------------------
!     INPUT:
!           ANALYTIC    : If ANALYTIC is .TRUE. the Holtslag (1984) 
!                         analytic solution is applied with use of 
!                         the constant value of 0.08 K for theta_star.
!                         If ANALYTIC is .FALSE. the given LOWER_LIM_L
!                         is applied as a lower boundary of the positive 
!                         values of L. This value should be rather close
!                         to ZU2, since this will give Z/L close to 1,
!                         i.e. the border of the validity of the applied
!                         M-O theory.
!           G           : Acceleration due to gravity [9.81 m/s2]
!           D_ADIAB     : The dry_adiabatic temp. gradient [0.01 K/m]
!           KAPPA       : von Karmans constant [0.4]
!           U1, U2      : Wind speed at ZU1 and ZU2 [m/s]
!                         (e.g. U1 = 0. and U2 = U10 for ZU2 = 10.
!                          and ZU1 = Z0.)
!           ZU1, ZU2    : Observ. Heights for Wind (ZU2 > ZU1) [m]
!                         (e.g. Z0 and 10. respectively.)
!           T1,T2       : Temperature at ZT1 and ZT2    [in Kelvin!]
!           ZT1,ZT2     : Observ. heights for Temp. (ZT2 > ZT1) [m].
!           LOWER_LIM_L : User given lower boundary on the positive
!                         values of the Monin-Obukhov length [m].
!
!     OUTPUT:
!            UST        : Velocity scale                       [m/s]
!            TST        : Temperature scale                    [K]
!             CL        : Monin-Obukhov length                 [m]
!
!
!                                           11-09-2006  L. H. Sl�rdal
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!           2016  M. Karl: Functions PSIM,TST_EBUDGET,OBUK are now in mod_mete.f90
!                          Modifications to use double precision variables
!
! ----------------------------------------------------------------------------------

!MSK start
        use mod_mete
!MSK end

      implicit none

!     External functions:
!MSK      REAL PSIH,PSIM,OBUK
!MSK  functions are in mod_mete.for now

!     Global variables:
      logical,intent(IN) :: ANALYTIC
      real,intent(IN)    :: G,KAPPA
      real,intent(IN)    :: D_ADIAB,ZU2,U2,ZU1,U1,ZT2,T2,ZT1,T1,  &
                            LOWER_LIM_L

      real,intent(OUT)   :: UST,TST,CL

!     Local variables:
      integer :: N,NRIT
      real    :: ALFA,DTET,CLP,ZETA,CL0,TCL,CLN,DL,ADL,ADL_LIM
             
! ----------------------------------------------------------------------

!_ORG:      NRIT = 20
      NRIT    = 50
!_ORG:      ADL_LIM = 0.05      
      ADL_LIM = 0.005
      
      ALFA    = 5.0

      DTET = (T2-T1) + (D_ADIAB*(ZT2-ZT1))
!_ORG:      CL   = 36.0
      CL   = 1.0E5
!_ORG:      IF (DTET < 0.0) CL = -36.0
      IF (DTET < 0.0) CL = -1.0E5

!MSK      UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
!MSK      TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(ZT2/CL)+PSIH(ZT1/CL))
      UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))
      TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(DBLE(ZT2/CL))+PSIH(DBLE(ZT1/CL)))


      DO 20 N=1,NRIT
!_ORG:        UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
!_ORG:        TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(ZT2/CL)+PSIH(ZT1/CL))


        CLP  = CL
!MSK        CL   = OBUK(KAPPA,G,UST,TST,T2) 
        CL   = OBUK(UST,TST,T2) 
        ZETA = ZU2/CL
        IF(ANALYTIC)THEN
          IF(ZETA > 1.0) THEN
! ***       Very stable, critical CL
            TST = 0.08
            CL0 = ALFA*ZU2/ALOG(ZU2/ZU1) 
            CLN = KAPPA*U2*U2*T2/(2.0*G*TST*ALOG(ZU2/ZU1)*ALOG(ZU2/ZU1))
            TCL = 2.0*CL0
            IF(CLN >= TCL) THEN
              CL = (CLN-CL0) + SQRT(CLN*CLN - 2.0*CL0*CLN)
            ELSE
              CL = SQRT(0.5*CL0*CLN)
            ENDIF
! ****      Critical CL = CL0, No extrapolation below CL0
            IF(CL < CL0) CL = CL0
! ***       Updating UST with the final CL value (TST = 0.08):  
!MSK            UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))    
            UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))   
          ENDIF  ! IF(ZETA > 1.0)
        ELSE  ! Now we are not applying the analytic solution:
          IF(CL > 0 .AND. CL < LOWER_LIM_L) CL = LOWER_LIM_L
! ***     Updating UST and TST with the final CL value:  
!MSK          UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
!MSK          TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(ZT2/CL)+PSIH(ZT1/CL))
          UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(DBLE(ZU2/CL))+PSIM(DBLE(ZU1/CL)))
          TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(DBLE(ZT2/CL))+PSIH(DBLE(ZT1/CL)))
        ENDIF
        DL  = (CL-CLP)/CLP
        ADL = ABS(DL)
        IF(ADL < ADL_LIM) GO TO 25

  20  CONTINUE
!_LHS:      PRINT *, 'NOT ACHIEVED CONVERGENCE in TURB_PROFILE.'
!_LHS:      STOP

  25  CONTINUE
      
      RETURN
      end subroutine turb_profile
