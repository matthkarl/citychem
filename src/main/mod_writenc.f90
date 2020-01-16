! <mod_writenc.f90 - A component of the City-scale
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
!* ========================================== 
!*
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*****************************************************************************!

      module mod_writenc

!*****************************************************************************!
!*     Module mod_writenc contains i/o utilities for 
!*     creating and opening of netCDF files in accord with CF-convention
!*     and writing of variables (concentration fields) to netCDF
!*****************************************************************************!
!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      contains


       subroutine check(status,routine,varname,action)

! *** The nf90 are functions which return 0 if no error occur.
! *** check is only a subroutine which check wether the function returns zero

          use netcdf
          implicit none
          integer, intent ( in) :: status
          character (len=*),intent(in)  ::varname
          character (len=30),intent(in) ::routine
          character (len=30),intent(in) ::action

          if(status /= nf90_noerr) then 
             print *, trim(nf90_strerror(status))
             print *, "routine ",routine
             print *, "variable ",varname
             print *, "action",action
             print *, "error in NetCDF_ml"
             call STOPIT('mod_writenc: NetCDF_ml: check status')
          end if

      end subroutine check



      subroutine datefromsecondssince1970(ndate,nseconds,printdate)
!*** calculate date from seconds that have passed since the start of the year 1970
          implicit none

          integer, intent(out) :: ndate(4)
          integer, intent(in) :: nseconds
          integer, intent(in) :: printdate

          integer :: n,nday,nmdays(12),nmdays2(13)
          nmdays = (/31,28,31,30,31,30,31,31,30,31,30,31/) 

          nmdays2(1:12)=nmdays
          nmdays2(13)=0
          ndate(1)=1969
          n=0
          do while(n<=nseconds)
            n=n+24*3600*365
            ndate(1)=ndate(1)+1
            if(mod(ndate(1),4)==0)n=n+24*3600
          enddo
          n=n-24*3600*365
          if(mod(ndate(1),4)==0)n=n-24*3600
          if(mod(ndate(1),4)==0)nmdays2(2)=29
          ndate(2)=0
          do while(n<=nseconds)
             ndate(2)=ndate(2)+1
             n=n+24*3600*nmdays2(ndate(2))
          enddo
          n=n-24*3600*nmdays2(ndate(2))
          ndate(3)=0
          do while(n<=nseconds)
             ndate(3)=ndate(3)+1
             n=n+24*3600
          enddo
          n=n-24*3600
          ndate(4)=-1
          do while(n<=nseconds)
             ndate(4)=ndate(4)+1
             n=n+3600
          enddo
          n=n-3600
          !    ndate(5)=nseconds-n
          if(printdate>0)then
          write(*,*)'year: ',ndate(1),', month: ',ndate(2),', day: ',ndate(3),', hour: ',ndate(4),', seconds: ',nseconds-n
          endif

      end subroutine datefromsecondssince1970


      subroutine datefromhourssince1900(ndate,nhours,printdate)
!*** Calculate date from hours that have passed since the start of the year 1900
!*** NB: 1900 is not a leap year

          implicit none

          integer, intent(out) :: ndate(4)
          integer, intent(in) :: nhours
          integer, intent(in) :: printdate

          integer :: n,nday,nmdays(12),nmdays2(13)
          nmdays = (/31,28,31,30,31,30,31,31,30,31,30,31/) 

          nmdays2(1:12)=nmdays
          nmdays2(13)=0
          ndate(1)=1899
          n=0
          do while(n<=nhours)
             n=n+24*365
             ndate(1)=ndate(1)+1
             if(mod(ndate(1),4)==0)n=n+24
             if(ndate(1)==1900)n=n-24
          enddo
          n=n-24*365
          if(mod(ndate(1),4)==0)n=n-24
          if(ndate(1)==1900)n=n+24
          if(mod(ndate(1),4)==0)nmdays2(2)=29
          if(ndate(1)==1900)nmdays2(2)=28
!  write(*,*)ndate(1),n
          ndate(2)=0
          do while(n<=nhours)
             ndate(2)=ndate(2)+1
             n=n+24*nmdays2(ndate(2))
          enddo
          n=n-24*nmdays2(ndate(2))
!  write(*,*)ndate(2),n
          ndate(3)=0
          do while(n<=nhours)
             ndate(3)=ndate(3)+1
             n=n+24
          enddo
          n=n-24
!  write(*,*)ndate(3),n
          ndate(4)=nhours-n

          if(printdate>0)then
             write(*,*)'year: ',ndate(1),', month: ',ndate(2),', day: ',ndate(3),', hour: ',ndate(4)
          endif

      end subroutine datefromhourssince1900


      subroutine secondssince1970(ndate,nseconds)
!*** calculate how many seconds have passed since the start of the year

          implicit none

          integer, intent(in) :: ndate(4)
          integer, intent(out) :: nseconds
          integer :: n,nday,nmdays(12)
          nmdays = (/31,28,31,30,31,30,31,31,30,31,30,31/) 
          n=ndate(1)
             if(4*(n/4)==n)nmdays(2)=29
          nday=0
          do n=1,ndate(2)-1
             nday=nday+nmdays(n)
          enddo
          nday=nday+ndate(3)

          nseconds=3600*(ndate(4)+24*(nday-1))

!***  Add seconds from each year since 1970
          do n=1970,ndate(1)-1
            nseconds=nseconds+24*3600*365
            if(4*(n/4)==n)nseconds=nseconds+24*3600
          enddo
!     write(*,*)ndate(1),ndate(2),ndate(3),ndate(4),nseconds

      end subroutine secondssince1970


!***************************************
!* Write concentrations to netCDF file
!* Now only 2D fields
!***************************************
      subroutine writeconcfield(fileName,varname,unitname,field,imax,jmax,kmax,Nhh_in,ndate, &
                                validity,dopacking,dofloat,domirror)

          use netcdf

          implicit none

          integer, intent(in) :: imax,jmax,kmax,Nhh_in,ndate(4,Nhh_in)
          character (len=*),intent(in)::fileName,varname,validity,unitname
          logical, intent(in) :: dopacking, dofloat, domirror

          double precision,intent(inout) :: field(imax,jmax,kmax)

          ! data packing parameters
          double precision ::  xmin,xmax
          double precision :: scale_factor, add_offset

          integer,  parameter :: Int1=1,Int2=2,Int4=3,Real4=4,Real8=5 !CDF typr for outp
          integer :: Wrec,ncFileID,idimID,jdimID,kdimID,VarID,tVarID
          integer :: GIMAX_old,GJMAX_old,kmax_old,OUTtype,ndim,nrecords,nseconds !,Nwrite
          character(len=8)  :: lastmodified_date
          character(len=10) :: lastmodified_hour

          integer:: i,j,k,ijk,ijkh,status,ihh,Nhh

          ! integer field in netCDF output; works well only with packing
          integer, allocatable, dimension(:,:,:)   :: Ifield_2D
          integer, allocatable, dimension(:,:,:,:) :: Ifield_3D
          real, allocatable, dimension(:,:,:)      :: Ffield_2D
          real, allocatable, dimension(:,:,:,:)    :: Ffield_3D

          ! character(len=100) ::grid_mapping  ! not used because not CF convention
          character(len=30) :: routine = 'writeconcfield'
          character(len=30) :: action

          Nhh=Nhh_in

          Wrec=0
          ndim=2
          if(kmax>1)ndim=3
          !Nwrite=imax*jmax*kmax*Nhh

          ! write(6,*)'opening file, dimensions', trim(fileName),ncFileID,imax,jmax,kmax

          action='open'
          call check(nf90_open(path=trim(fileName),mode=nf90_write,ncid=ncFileID),routine,varname,action)

!***   CF-1.0 definitions:
!***  No practical use for the variable grid_mapping and global attribute proj4_string
!***          grid_mapping=''
!***          call check(nf90_get_att(ncFileID,nf90_global,"proj4_string",grid_mapping),varname)

          !write(6,*)'projection: ',trim(grid_mapping)
          action='inqdim'
          call check(nf90_inq_dimid(ncid = ncFileID, name = "i", dimID = idimID),routine,varname,action)
          call check(nf90_inq_dimid(ncid = ncFileID, name = "j", dimID = jdimID),routine,varname,action)
          call check(nf90_inq_dimid(ncid = ncFileID, name = "k", dimID = kdimID),routine,varname,action)

          call check(nf90_inquire_dimension(ncid=ncFileID,dimID=idimID,len=GIMAX_old),routine,varname,action)
          call check(nf90_inquire_dimension(ncid=ncFileID,dimID=jdimID,len=GJMAX_old),routine,varname,action)
          call check(nf90_inquire_dimension(ncid=ncFileID,dimID=kdimID,len=KMAX_old),routine,varname,action)

          ! write(6,*)'dimensions new y x z',imax,jmax,kmax
          ! write(6,*)'dimensions old y x z',GIMAX_old,GJMAX_old,kmax_old

!*** test if the defined dimensions are compatible 
          if (GIMAX_old/=IMAX .or. GJMAX_old/=JMAX .or.  kmax_old<kmax)then
            write(6,*)'file ', trim(fileName),' has incompatible dimensions'
            write(6,*)GIMAX_old,IMAX,GJMAX_old,jmax,kmax_old,kmax
            call STOPIT('mod_writenc: incompatible dimensions')
          endif

!***   CF-1.0 definitions:
!***  No practical use for the variable grid_mapping and global attribute proj4_string
!***       ! UTM projections are on Transverse Mercator
!***       if (trim(grid_mapping)==trim('Transverse Mercator'))then
!***          grid_mapping='Transverse_Mercator'
!***       elseif(trim(grid_mapping)==trim('Stereographic'))then
!***          grid_mapping='Polar_Stereographic'
!***       elseif(trim(grid_mapping)==trim('lon lat'))then
!***          grid_mapping='Lon_Lat'
!***       else
!***          grid_mapping='General_projection'
!***       endif

!***   Test first if the variable is already defined:
          status = nf90_inq_varid(ncid = ncFileID, name = varname, varID = VarID)
          if(status == nf90_noerr) then     
!***        ok, expected answer if variable already in file
             !print *, 'variable exists: ',varname
          else
             write(6,*) 'creating variable: ',trim(varname)!,nf90_strerror(status)


             if (dofloat) then
                OUTtype=Real4     ! float field
             else
                OUTtype=Int2      ! short integer field
             endif

            call createNewVariable(ncFileID,varname,unitname,ndim,imax,ndate(:,1),OUTtype,validity)


          endif
       !      write(6,*) 'done creating variable: ',trim(varname)

!***   Pack the data in "short" type
!      unpacked_value = scale_factor * packed_value + add_offset
!***   NOTE: a different scaling factor for each level could be more
!      accurate for fields which vary much with height. But this may not be 
!      compatible with any conventions?

          xmax=maxval(field( 1:imax, 1:jmax, 1:kmax ))
          xmin=minval(field( 1:imax, 1:jmax, 1:kmax ))


          if (dopacking) then
             add_offset=(xmax+xmin)/2.
             scale_factor=(xmax-xmin)/65532.
          else
             add_offset=0.0
             scale_factor=1.0
          endif

          !get variable id
          action='varid'
          call check(nf90_inq_varid(ncid = ncFileID, name = varname, varID = VarID),routine,varname,action)

          !find the number of records already written
          action='getatt'
          call check(nf90_get_att(ncFileID, VarID, "numberofrecords",   nrecords),routine,varname,action)
!  write(*,*)'number of dataset already saved: ',nrecords
  !increase the last coordinate by one, to define position of new data
          if(Wrec>0)then
             nrecords=Wrec
          else
             nrecords=nrecords+1
          endif

          ! write(*,*)'writing on records: ',nrecords,'to ',nrecords+Nhh-1

          if (kmax==1)then
             action='put2d'
             if (dofloat) then
               if (domirror) then
                 allocate(Ffield_2D(jmax,imax,Nhh))
               else
                 allocate(Ffield_2D(imax,jmax,Nhh))
               endif
             else
                allocate(Ifield_2D(imax,jmax,Nhh))
             endif
             ijkh=0
             do ihh=1,Nhh
                 do j=1,jmax
                    do i=1,imax
                       ijkh=ijkh+1
                       if (dofloat) then
                         if (domirror) then
                           Ffield_2D(j,i,ihh)=(field(i,j,1)-add_offset)/scale_factor
                         else
                           Ffield_2D(i,j,ihh)=(field(i,j,1)-add_offset)/scale_factor
                         endif
                       else
                         Ifield_2D(i,j,ihh)=nint((field(i,j,1)-add_offset)/scale_factor)
                       endif
                    enddo
                 enddo
             enddo

             if (dofloat) then
                call check(nf90_put_var(ncFileID,VarID,Ffield_2D,start=(/1,1,nrecords/)),routine,varname,action)
             else
                call check(nf90_put_var(ncFileID,VarID,Ifield_2D,start=(/1,1,nrecords/)),routine,varname,action)
             endif

            !print *,'mod_writenc: writeconcfield after put Ifield_2D'           
             if (dofloat) then
               deallocate(Ffield_2D)
             else
               deallocate(Ifield_2D)
             endif

          else if(ndim==3)then
             action='put3d'
             if (dofloat) then
               if (domirror) then
                 allocate(Ffield_3D(jmax,imax,kmax,Nhh))
               else
                 allocate(Ffield_3D(imax,jmax,kmax,Nhh))
               endif
             else
               allocate(Ifield_3D(imax,jmax,kmax,Nhh))
             endif
             ijkh=0
             do ihh=1,Nhh
                 do k=1,kmax
                    do j=1,jmax
                       do i=1,imax
                          ijkh=ijkh+1
                          if (dofloat) then
                            if (domirror) then
                              Ffield_3D(j,i,k,ihh)=(field(i,j,k)-add_offset)/scale_factor
                            else
                              Ffield_3D(i,j,k,ihh)=(field(i,j,k)-add_offset)/scale_factor
                            endif
                          else
                            Ifield_3D(i,j,k,ihh)=nint((field(i,j,k)-add_offset)/scale_factor)
                          endif
                       enddo
                    enddo
                 enddo
             enddo

             if (dofloat) then
                call check(nf90_put_var(ncFileID, VarID,Ffield_3D, start=(/ 1,1,1,nrecords /)) ,routine,varname,action)
             else
                call check(nf90_put_var(ncFileID, VarID,Ifield_3D, start=(/ 1,1,1,nrecords /)) ,routine,varname,action)
             endif

             if (dofloat) then
               deallocate(Ffield_3D)
             else
               deallocate(Ifield_3D)
             endif

          else
             call STOPIT('mod_writenc: writeconcfield')
          endif


          action='redef'
!MSK   Enter define mode so we can add the attribute
          call check(nf90_redef(ncid = ncFileID),routine,varname,action)

          action='putatt'
!***   Packing variables
          call check(nf90_put_att(ncFileID, VarID, "scale_factor", scale_factor ),routine,varname,action)
          call check(nf90_put_att(ncFileID, VarID, "add_offset",   add_offset   ),routine,varname,action)

!MSK  04.10.2017 Update of global attributes does not work in long runs
!MSK!***   Update dates in the global attributes
!MSK          call Date_And_Time(date=lastmodified_date,time=lastmodified_hour)
!MSK          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_date", lastmodified_date),routine,varname,action)
!MSK          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_hour", lastmodified_hour),routine,varname,action)

          action='conclast'
          call check(nf90_put_att(ncFileID, VarID, "conc_date_last",ndate(:,Nhh)),routine,varname,action)

!MSK    End the define mode
          action='enddef'
          call check(nf90_enddef(ncid = ncFileID),routine,varname,action)

!***    Define time of new records
          action='putvar'
          nrecords=nrecords-1
          do ihh=1,Nhh

              nrecords=nrecords+1
              call check(nf90_inq_varid(ncid = ncFileID, name = "time", varID = tVarID),routine,varname,action)
              call secondssince1970(ndate(:,ihh),nseconds)
              call check(nf90_put_var(ncFileID, tVarID, nseconds, start = (/nrecords/) ) ,routine,varname,action)

          enddo

          action='nofr'
          call check(nf90_put_att(ncFileID, VarID, "numberofrecords",   nrecords),routine,varname,action)

          action='close'
          call check(nf90_close(ncFileID),routine,varname,action)

      end subroutine writeconcfield



!***************************************
!* Create new Variable in netCDF file
!***************************************
      subroutine  createNewVariable(ncFileID,varname,unitname,ndim,imax,ndate,OUTtype,validity)

!***   Create new netCDF variable

          use netcdf

          implicit none

          character (len = *),intent(in) ::varname,validity,unitname
          integer ,intent(in) ::  ndim,ncFileID,OUTtype
          integer ,intent(in) ::  imax
          integer, intent(in) ::  ndate(4)

          integer :: iDimID,jDimID,kDimID,timeDimID
          integer :: varID,nrecords,OUTtypeCDF
          integer :: chunkalg,shuffle,deflate,deflate_level
          integer,    parameter :: Int1=1,Int2=2,Int4=3,Real4=4,Real8=5 !CDF type for output
          integer, dimension(4) :: chunksizesd3
          integer, dimension(3) :: chunksizesd2
          real    :: scale,offset
          character(len=30) :: routine = 'createNewVariable'
          character(len=30) :: action

           ! write(6,*)'create', ncFileID,varname,unitname,ndim,imax,ndate,OUTtype,validity

          if(OUTtype==Int1)then
             OUTtypeCDF=nf90_byte
          elseif(OUTtype==Int2)then
             OUTtypeCDF=nf90_short
          elseif(OUTtype==Int4)then
             OUTtypeCDF=nf90_int
          elseif(OUTtype==Real4)then
             OUTtypeCDF=nf90_float
          elseif(OUTtype==Real8)then
             OUTtypeCDF=nf90_double
          else
             call STOPIT('mod_writenc: NetCDF_ml: undefined datatype')
         endif

         action='redef'
         call check(nf90_redef(ncid = ncFileID),routine,varname,action)

         action='inqdim'
         call check(nf90_inq_dimid(ncid = ncFileID, name = "i", dimID = idimID),routine,varname,action)
         call check(nf90_inq_dimid(ncid = ncFileID, name = "j", dimID = jdimID),routine,varname,action)
         call check(nf90_inq_dimid(ncid = ncFileID, name = "k", dimID = kdimID),routine,varname,action)
         call check(nf90_inq_dimid(ncid = ncFileID, name = "time", dimID = timeDimID),routine,varname,action)


         action='def'
!***   Define new variable
         if(ndim==3)then
                call check(nf90_def_var(ncid = ncFileID, name = varname, xtype = OUTtypeCDF,     &
                     dimids = (/ iDimID, jDimID, kDimID , timeDimID/), varID=varID ) ,routine,varname,action)
         elseif(ndim==2)then

                call check(nf90_def_var(ncid = ncFileID, name = varname, xtype = OUTtypeCDF,     &
                     dimids = (/ iDimID, jDimID , timeDimID/), varID=varID ) ,routine,varname,action)

         elseif(ndim==1)then
                call check(nf90_def_var(ncid = ncFileID, name = varname, xtype = OUTtypeCDF,     &
                     dimids = (/ iDimID, jDimID , timeDimID/), varID=varID ) ,routine,varname,action)
         else
                 print *, 'createnewvariable: unexpected ndim ',ndim   
         endif

         action='putatt'
!***    Define attributes of new variable
         call check(nf90_put_att(ncFileID, varID, "long_name",  trim(varname) ),routine,varname,action)

!***   CF-1.0 definitions:
!***  No practical use for the variables coordinates and grid_mapping
!***         call check(nf90_put_att(ncFileID, varID, "coordinates", "y  x"),varname)
!***         call check(nf90_put_att(ncFileID, varID, "grid_mapping", trim(grid_mapping)),varname)
!***
         nrecords=0
         call check(nf90_put_att(ncFileID, varID, "numberofrecords", nrecords),routine,varname,action)
         call check(nf90_put_att(ncFileID, varID, "units", unitname),routine,varname,action)


         scale  = 1.
         offset = 0.

!***    Check what the fill value is. We want -9999. in CityChem
         if(OUTtype==Int1)then
            call check(nf90_put_att(ncFileID, varID, "_FillValue", nf90_fill_byte  ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "scale_factor",  scale ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "add_offset",  offset ),routine,varname,action)
         elseif(OUTtype==Int2)then
            call check(nf90_put_att(ncFileID, varID, "_FillValue", nf90_fill_short  ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "scale_factor",  scale ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "add_offset",  offset ),routine,varname,action)
         elseif(OUTtype==Int4)then
            call check(nf90_put_att(ncFileID, varID, "_FillValue", nf90_fill_int   ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "scale_factor",  scale ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "add_offset",  offset ),routine,varname,action)
         elseif(OUTtype==Real4)then
            call check(nf90_put_att(ncFileID, varID, "_FillValue", nf90_fill_float  ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "scale_factor",  scale ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "add_offset",  offset ),routine,varname,action)
         elseif(OUTtype==Real8)then
            call check(nf90_put_att(ncFileID, varID, "_FillValue", nf90_fill_double  ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "scale_factor",  scale ),routine,varname,action)
            call check(nf90_put_att(ncFileID, varID, "add_offset",  offset ),routine,varname,action)
         endif

         call check(nf90_put_att(ncFileID, varID, "conc_date_first",ndate ),routine,varname,action)
         call check(nf90_put_att(ncFileID, varID, "conc_date_last",ndate ),routine,varname,action)
         call check(nf90_put_att(ncFileID, VarID, "validity", trim(validity)),routine,varname,action)

!***    Compression parameters
!***    The function nf90_def_var_deflate sets the deflate parameters for a variable in a
!***    netCDF-4 file. It does not work on netCDF-3 files.
!***    The chunksize is to be used for each dimension of the dataset.
!***    A maximum size of 10 is used for the chunksizes

         deflate_level = 9  ! highest compression level
         deflate       = 1
         shuffle       = 0
         chunkalg      = 0  ! turn chunking on
         chunksizesd3(1) = 5
         chunksizesd3(2) = 5
         chunksizesd3(3) = 1
         chunksizesd3(4) = 1
         ! chunksizes for 2-D fields
         chunksizesd2(1) = 5
         chunksizesd2(2) = 5
         chunksizesd2(3) = 1

         action='chunk'
! compression  only if imax > 10
         if (imax.gt.10) then

           if(ndim==3)then
             call check(nf90_def_var_chunking(ncFileID, VarID, chunkalg, chunksizesd3),routine,varname,action)
           elseif(ndim==2)then
             call check(nf90_def_var_chunking(ncFileID, VarID, chunkalg, chunksizesd2),routine,varname,action)
           else
                 print *, 'createnewvariable: no chunking'
           endif

           call check(nf90_def_var_deflate( ncFileID, VarID, shuffle,  deflate, deflate_level),routine,varname,action)

         endif


!***    End of definition mode
         action='enddef'         
         call check(nf90_enddef(ncid = ncFileID),routine,varname,action)


      end subroutine  createNewVariable



!***************************************
!* Create netCDF for the Main Grid
!***************************************
subroutine CreateNCfileGrid( fileName, gimax, gjmax, kmax,      &
                              gl,gb, sigma_mid,          &
                              GRIDWIDTH_M, UTMZONE_N, EPSGN, SITEX0,SITEY0)

!***  Create the netCDF file
          use netcdf

          implicit none
          integer, intent(in) :: gimax,gjmax,kmax
          real, intent(in) ::  GRIDWIDTH_M
          real, intent(in) ::  SITEX0,SITEY0
          real, intent(in) ::  sigma_mid(kmax)
          real, intent(in) ::  gl(gjmax,gimax)    !x
          real, intent(in) ::  gb(gjmax,gimax)    !y
          character(len=*), intent(in) :: UTMZONE_N
          character(len=*), intent(in) :: EPSGN
          character(len=*), intent(in) :: fileName 

          character (len=*), parameter :: vert_coord='vertical coordinates = (p-p(top))/(p(surf)-p(top))'



          real :: xcoord(gimax),ycoord(gjmax),kcoord(kmax)

          character(len=8)   :: created_date,lastmodified_date
          character(len=10)  :: created_hour,lastmodified_hour
          character(len=256) :: projection
          character(len=256) :: projection4

          integer :: ncFileID,iDimID,jDimID,kDimID,timeDimID,VarID,iVarID,jVarID,kVarID,i,j,k
          integer :: iEMEPVarID,jEMEPVarID,latVarID,longVarID
          integer :: xmiVarID,xmjVarID
          real :: izero,jzero,scale_at_projection_origin
          real :: e2,EARTH_RADIUS
          real :: great_circle_distance
          real,allocatable :: xm(:,:),gl_tmp(:,:),gb_tmp(:,:)
          real, parameter :: PI=3.14159265358979323
          character(len=30) :: routine = 'CreateNCfileGrid'
          character(len=30) :: action

!***    Define the projection

!           projection = trim('WGS_1984_UTM_Zone_'//trim(UTMZONE_N))
         projection4 = trim('+proj=utm +zone='//trim(UTMZONE_N(1:2))//' +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

  ! fileName: Name of the new created file 
  ! nf90_clobber: protect existing datasets
  ! ncFileID: netcdf ID


      write(*,*)'create ',trim(fileName)
      write(*,*)'projection ',trim(projection4)
      write(*,*)'with sizes (IMAX,JMAX,KMAX) ',gimax,gjmax,kmax
!           call check(nf90_create(path = trim(fileName), cmode = nf90_clobber, ncid = ncFileID))
          action='create'  
          call check(nf90_create(path=trim(fileName),cmode=or(nf90_clobber,nf90_netcdf4),ncid=ncFileID),routine,fileName,action)

!***    Define the dimensions
          action='def'  
          call check(nf90_def_dim(ncid = ncFileID, name = "i", len = gimax, dimid = iDimID),routine,fileName,action)
          call check(nf90_def_dim(ncid = ncFileID, name = "j", len = gjmax, dimid = jDimID),routine,fileName,action)
          call check(nf90_def_dim(ncid = ncFileID, name = "k", len = kmax,  dimid = kDimID),routine,fileName,action)

          call check(nf90_def_dim(ncid = ncFileID, name = "time", len = nf90_unlimited, dimid = timeDimID),routine,fileName,action)

          call Date_And_Time(date=created_date,time=created_hour)
  write(6,*) 'created_date: ',created_date
  write(6,*) 'created_hour: ',created_hour

!***    Write global attributes
          action='putatt'  
          call check(nf90_put_att(ncFileID, nf90_global, "Conventions", "CF-1.0" ),routine,fileName,action)

!***  EPSG standard projection attributes
          call check(nf90_put_att(ncFileID, nf90_global, "proj4_string",projection4),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "EPSG",EPSGN ),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "XORIG",SITEX0 ),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "YORIG",SITEY0 ),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "XCELL",GRIDWIDTH_M ),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "YCELL",GRIDWIDTH_M ),routine,fileName,action)          
          call check(nf90_put_att(ncFileID, nf90_global, "NCOL",gimax ),routine,fileName,action) 
          call check(nf90_put_att(ncFileID, nf90_global, "NROW",gjmax ),routine,fileName,action)

          call check(nf90_put_att(ncFileID, nf90_global, "vert_coord", vert_coord),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "created_date", created_date),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "created_hour", created_hour),routine,fileName,action)
          lastmodified_date = created_date
          lastmodified_hour = created_hour
          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_date", lastmodified_date),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_hour", lastmodified_hour),routine,fileName,action)
  
!***    Define coordinate variables
          call check(nf90_def_var(ncFileID, "i", nf90_float, dimids = iDimID, varID = iVarID),routine,fileName,action)
          call check(nf90_put_att(ncFileID, iVarID, "standard_name", "projection_x_coordinate"),routine,fileName,action)
!*** not CF conform         call check(nf90_put_att(ncFileID, iVarID, "coord_axis", "x"),fileName)
          call check(nf90_put_att(ncFileID, iVarID, "long_name", "grid x coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, iVarID, "units", "m"),routine,fileName,action)


          call check(nf90_def_var(ncFileID, "j", nf90_float, dimids = jDimID, varID = jVarID) ,routine,fileName,action)
          call check(nf90_put_att(ncFileID, jVarID, "standard_name", "projection_y_coordinate"),routine,fileName,action)
!*** not CF conform         call check(nf90_put_att(ncFileID, jVarID, "coord_axis", "y"),fileName)
          call check(nf90_put_att(ncFileID, jVarID, "long_name", "grid y coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, jVarID, "units", "m"),routine,fileName,action)

          call check(nf90_def_var(ncFileID, "k", nf90_float, dimids = kDimID, varID = kVarID) ,routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "coord_alias", "sigma_z_coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "long_name", "vertical sigma coordinates"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "units", "m"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "positive", "up"),routine,fileName,action)

          call check(nf90_def_var(ncFileID, "time", nf90_int, dimids = timeDimID, varID = VarID),routine,fileName,action )

          call check(nf90_put_att(ncFileID, VarID, "units", "seconds since 1970-1-1 00:00:00.0 +00:00"),routine,fileName,action)

!***   CF-1.0 definitions:
!***  No practical use for the variable "General"
!***          call check(nf90_def_var(ncid = ncFileID, name = "General", xtype = nf90_int, varID=varID ) )
!***          call check(nf90_put_att(ncFileID, VarID, "grid_mapping_name", "General"))


!***    Leave define mode
          action='enddef'  
          call check(nf90_enddef(ncFileID),routine,fileName,action)

          action='open'
          call check(nf90_open(path = trim(fileName), mode = nf90_write, ncid = ncFileID),routine,fileName,action)

!***    Define horizontal distances

!***  Try to add the real utm coordinates in gb and gl

  !Define longitude and latitude
  !call check(nf90_put_var(ncFileID, latVarID, gb(1:gimax,1:gjmax)) )
  !call check(nf90_put_var(ncFileID, longVarID, gl(1:gimax,1:gjmax)) )


!x-coordinate
!          call check(nf90_put_var(ncFileID, iVarID, gl(1:gjmax,1:gimax) ) )
!y-coordinate
!          call check(nf90_put_var(ncFileID, jVarID, gb(1:gjmax,1:gimax) ) )

          action='putvar'
! It is sufficient to use one-dimnesional coordinates because
! the model domain is a regular grid on UTM projection
          do i=1,gimax
              xcoord(i)=gl(1,i)
          enddo
          call check(nf90_put_var(ncFileID, iVarID, xcoord(1:gimax)) ,routine,fileName,action)

          do j=1,gjmax
              ycoord(j)=gb(j,1)
          enddo
          call check(nf90_put_var(ncFileID, jVarID, ycoord(1:gjmax)) ,routine,fileName,action)

!***   Define vertical levels
          do k=1,kmax
             kcoord(k)=sigma_mid(k)
          enddo

          call check(nf90_put_var(ncFileID, kVarID, kcoord(1:kmax)) ,routine,fileName,action)

          action='close'
          call check(nf90_close(ncFileID),routine,fileName,action)

          write(*,*)'main grid netcdf file created'

  end subroutine CreateNCfileGrid


!***************************************
!* Create netCDF for the Stations
!***************************************
subroutine CreateNCfileStations( fileName, smax, kmax,      &
                              gl,gb, sigma_mid,             &
                              UTMZONE_N)

!***  Create the netCDF file
          use netcdf

          implicit none
          integer, intent(in) :: smax,kmax
          real, intent(in)    ::  sigma_mid(kmax)
          real, intent(in)    ::  gl(smax)
          real, intent(in)    ::  gb(smax)
          character(len=*), intent(in) :: UTMZONE_N
          character(len=*), intent(in) :: fileName 

          character (len=*), parameter :: vert_coord='vertical coordinates = (p-p(top))/(p(surf)-p(top))'

          real :: xcoord(smax),ycoord(smax),kcoord(kmax)

          character(len=8)   :: created_date,lastmodified_date
          character(len=10)  :: created_hour,lastmodified_hour
          character(len=256) :: projection,projection4

          integer :: ncFileID,iDimID,jDimID,kDimID,timeDimID,VarID,iVarID,jVarID,kVarID,i,j,k
          integer :: iEMEPVarID,jEMEPVarID,latVarID,longVarID
          integer :: xmiVarID,xmjVarID
          real :: izero,jzero,scale_at_projection_origin
          real :: e2,EARTH_RADIUS
          real :: great_circle_distance
          real,allocatable :: xm(:,:),gl_tmp(:,:),gb_tmp(:,:)
          real, parameter :: PI=3.14159265358979323
          character(len=30) :: routine = 'CreateNCfileStations'
          character(len=30) :: action

!***    Define the projection

         !projection = trim('WGS_1984_UTM_Zone_'//trim(UTMZONE_N))
         projection4 = trim('+proj=utm +zone='//trim(UTMZONE_N(1:2))//' +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
         
  ! fileName: Name of the new created file 
  ! nf90_clobber: protect existing datasets
  ! ncFileID: netcdf ID


  write(*,*)'create ',trim(fileName)
  write(*,*)'projection ',trim(projection4)
  write(*,*)'with sizes (smax,kmax) ', smax, kmax
!           call check(nf90_create(path = trim(fileName), cmode = nf90_clobber, ncid = ncFileID))
           action='create'  
           call check(nf90_create(path=trim(fileName),cmode=or(nf90_clobber,nf90_netcdf4),ncid=ncFileID),routine,fileName,action)

!***    Define the dimensions
          action='defdim'  
          call check(nf90_def_dim(ncid = ncFileID, name = "i", len = smax, dimid = iDimID),routine,fileName,action)
          call check(nf90_def_dim(ncid = ncFileID, name = "j", len = smax, dimid = jDimID),routine,fileName,action)
          call check(nf90_def_dim(ncid = ncFileID, name = "k", len = kmax, dimid = kDimID),routine,fileName,action)

          call check(nf90_def_dim(ncid = ncFileID, name = "time", len = nf90_unlimited, dimid = timeDimID),routine,fileName,action)

          call Date_And_Time(date=created_date,time=created_hour)
  write(6,*) 'created_date: ',created_date
  write(6,*) 'created_hour: ',created_hour

!***    Write global attributes
          action='putatt'
          call check(nf90_put_att(ncFileID, nf90_global, "Conventions", "CF-1.0" ),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "proj4_string",projection4),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "vert_coord", vert_coord),routine,fileName,action)

          call check(nf90_put_att(ncFileID, nf90_global, "created_date", created_date),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "created_hour", created_hour),routine,fileName,action)
          lastmodified_date = created_date
          lastmodified_hour = created_hour
          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_date", lastmodified_date),routine,fileName,action)
          call check(nf90_put_att(ncFileID, nf90_global, "lastmodified_hour", lastmodified_hour),routine,fileName,action)
  
!***    Define coordinate variables
          call check(nf90_def_var(ncFileID, "i", nf90_float, dimids = iDimID, varID = iVarID) ,routine,fileName,action)
          call check(nf90_put_att(ncFileID, iVarID, "standard_name", "projection_x_coordinate"),routine,fileName,action)
!*** not CF conform          call check(nf90_put_att(ncFileID, iVarID, "coord_axis", "x"),fileName)
          call check(nf90_put_att(ncFileID, iVarID, "long_name", "grid x coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, iVarID, "units", "m"),routine,fileName,action)


          call check(nf90_def_var(ncFileID, "j", nf90_float, dimids = jDimID, varID = jVarID) ,routine,fileName,action)
          call check(nf90_put_att(ncFileID, jVarID, "standard_name", "projection_y_coordinate"),routine,fileName,action)
!*** not CF conform          call check(nf90_put_att(ncFileID, jVarID, "coord_axis", "y"),fileName)
          call check(nf90_put_att(ncFileID, jVarID, "long_name", "grid y coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, jVarID, "units", "m"),routine,fileName,action)



          call check(nf90_def_var(ncFileID, "k", nf90_float, dimids = kDimID, varID = kVarID) ,routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "coord_alias", "sigma_z_coordinate"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "long_name", "vertical sigma coordinates"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "units", "m"),routine,fileName,action)
          call check(nf90_put_att(ncFileID, kVarID, "positive", "down"),routine,fileName,action)

          call check(nf90_def_var(ncFileID, "time", nf90_int, dimids = timeDimID, varID = VarID),routine,fileName,action)

          call check(nf90_put_att(ncFileID, VarID, "units", "seconds since 1970-1-1 00:00:00.0 +00:00"),routine,fileName,action)

!***   CF-1.0 definitions:
!***  No practical use for the variable "General"
!***          call check(nf90_def_var(ncid = ncFileID, name = "General", xtype = nf90_int, varID=varID ) )
!***          call check(nf90_put_att(ncFileID, VarID, "grid_mapping_name", "General"))


!***    Leave define mode
          action='enddef'  
          call check(nf90_enddef(ncFileID),routine,fileName,action)

          action='open'
          call check(nf90_open(path = trim(fileName), mode = nf90_write, ncid = ncFileID),routine,fileName,action)

!***    Define horizontal distances

          !xcoord(1)=GRIDWIDTH_M/2.0!NB: middle of cells
          !do i=2,gimax
          !   xcoord(i)=xcoord(i-1)+GRIDWIDTH_M
          !   !     print *, i,xcoord(i)
          !enddo
          do i = 1,smax
             xcoord(i)=gl(i)
          enddo

          action='putvar'
          call check(nf90_put_var(ncFileID, iVarID, xcoord(1:smax)) ,routine,fileName,action)

          !ycoord(1)=GRIDWIDTH_M/2.0!NB: middle of cells
          !do j=2,gjmax
          !   ycoord(j)=ycoord(j-1)+GRIDWIDTH_M
          !enddo
          do i = 1,smax
             ycoord(i)=gb(i)
          enddo
          call check(nf90_put_var(ncFileID, jVarID, ycoord(1:smax)),routine,fileName,action )


!***   Define vertical levels
          do k=1,kmax
             kcoord(k)=sigma_mid(k)
          enddo

          call check(nf90_put_var(ncFileID, kVarID, kcoord(1:kmax)),routine,fileName,action )

          action='close'
          call check(nf90_close(ncFileID),routine,fileName,action)

          write(*,*)'station netcdf file created'

  end subroutine CreateNCfileStations


      end module mod_writenc
