! <r2dfld.f90 - A component of the City-scale
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
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      SUBROUTINE R2DFLD(UN,IRWF,TEXT1,TEXT2,NX,NY,MX,MY,FLD)

! The subroutine reads from fileunit UN a 2D-field FLD using read/write
! format (index) IRWF.
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

      USE mod_main

! Scalar arguments

      INTEGER UN,IRWF,NX,NY,MX,MY
      CHARACTER(len=10) TEXT1,TEXT2

! UN          - Fileunit
! IRWF        - Read/write format index
! NX,NY       - Dimensions of FLD
! MX,MY       - Declared dimensions of FLD in the calling program
! TEXT1,TEXT2 - Text strings read from file

! Array arguments

      REAL FLD(MX,MY)

! FLD - The field

! Local variables

      INTEGER I,II,J,JJ,IX,IY

! I,II,J,JJ - Indices
! IX,IY     - Dimensions read from file

! Read field data depending on read/write format

      IF (IRWF .LE. 0) THEN

! No data is read

          RETURN

      ENDIF

      IF (IRWF .EQ. 1 .OR. IRWF .EQ. 6) THEN

! Read data using simple ASCII  format
          READ (UN,2100,END=120) TEXT1,TEXT2,IX,IY
          !write(*,*) TEXT1,TEXT2,IX,IY
          DO 100 J = 1,IY
              DO 110 I = 1,IX
                  READ (UN,*,END=120) II,JJ,FLD(I,J)
                  !write(*,*) II,JJ,FLD(I,J)
  110         CONTINUE
  100     CONTINUE

       !write(*,*) 'Finished -1'
         READ (UN,*,END=120)
       !write(*,*) 'Finished 0'
         IF (IX .NE. NX .OR. IY .NE. NY) GOTO 130
      ENDIF

      IF (IRWF .EQ. 2) THEN

! Read data using simple binary format

          READ (UN,END=120) TEXT1,TEXT2,IX,IY,   &
                           ((FLD(I,J),I=1,IX),J=1,IY)

          IF (IX .NE. NX .OR. IY .NE. NY) GOTO 130

      ENDIF

      RETURN

  120 CONTINUE
      !write(*,*) 'Finished +1',MESSFE

! End of file
      IF (MESSFE) WRITE (MESSUN,2000) UN,TEXT1,TEXT2
      CALL STOPIT('R2DFLD: End of file!')
      !write(*,*) 'Finished +2'

  130 CONTINUE

! Checking field dimensions

      IF (IX .EQ. 1 .AND. IY .EQ. 1) THEN

! Homogeneous field

          DO 140 J = 1,NY
          DO 150 I = 1,NX
              FLD(I,J) = FLD(1,1)
  150     CONTINUE
  140     CONTINUE

          RETURN

      ELSE

! Field dimensions different from expected

          IF (MESSFE) WRITE (MESSUN,2010) UN,NX,NY,IX,IY
          CALL STOPIT('R2DFLD: Error in field dimensions!')

      ENDIF

 2000 FORMAT ('R2DFLD: End of file ',I2,' Last read ',2A10)
 2010 FORMAT ('R2DFLD: Error in field dimensions on file ',I2,  &
             ' expects ',2I4,' but reads ',2I4,'!')
 2100 FORMAT (2A10,2I3)

! End of subroutine R2DFLD

      END
