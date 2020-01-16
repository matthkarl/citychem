! <cdrydepv.f90 - A component of the City-scale
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

      subroutine cdrydepv(Ra,Rb,Rc,Vs,V_drydep)
!
!     This subroutine calculates the Dry Deposition Velocity, D_drydep,
!     The unit of the Dry Deposition Velocity is specified by unit_out.
!
!     In the calculations V_drydep is given in: m/s. 
!
!
!     Written September 2004 by Leiv H�vard Sl�rdal (lhs@nilu.no)
!
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
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

!
      implicit none
!
!     GLOBAL VARIABLES:
!     *****************
!     Input:
!
      real :: Ra,Rb,Rc,Vs
!
!     Ra       - Aerodynamic resistance            (s/m)
!     Rb       - Quasi-laminar resistance          (s/m)
!     Rc       - Canopy resistance                 (s/m)
!     Vs       - Gravitational Settling Velocity   (m/s) 
!
!
!     Output:
!
      real :: V_drydep
!
!     V_drydep - The Dry-Deposition Velocity       (m/s)
!
!
! *********************************************************************
!
!
        IF(Vs .GT. 0)THEN
!
!        Dry-Deposition Velocites for Particles:
!
         V_drydep = Vs + (1.0/(Ra + Rb + Ra*Rb*Vs))
!
        ELSE
!
!        Gaseous compounds considered:
!
         V_drydep = 1.0 / (Ra + Rb + Rc)

        ENDIF

	
      RETURN

!     End of SUBROUTINE CDRYDEPV

      end subroutine cdrydepv
