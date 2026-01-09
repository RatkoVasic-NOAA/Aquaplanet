 program sfc_profile
 implicit none
 integer, parameter :: im=3072,jm=1536,ivars=60
 integer :: ivar
 integer, dimension(2) :: start, count
 character *20, dimension(ivars) :: cvar

   cvar=(/'alnsf', 'alnwf', 'alvsf', 'alvwf', 'cnwat', 'crain', 'f10m', &
         'facsf', 'facwf', 'icec', 'icetk', 'land', 'orog', 'sfcr', &
         'shdmax', 'shdmin', 'sltyp', 'snoalb', 'snod', 'soill1', &
         'soill2', 'soill3', 'soill4', 'soilt1', 'soilt2', 'soilt3', &
         'soilt4', 'soilw1', 'soilw2', 'soilw3', 'soilw4', 'sotyp', &
         'spfh2m', 'tisfc', 'tmp2m', 'tmpsfc', 'tprcp', 'veg', &
         'vtype', 'weasd', 'c0', 'cd', 'dconv', 'dtcool', &
         'qrain', 'tref', 'w0', 'wd', 'xs', 'xt', 'xtts', &
         'xu', 'xv', 'xz', 'xzts', 'zc', 'ffhh', 'ffmm', 'fricv', 'tg3'/)

   start = (/1, 1/)
   count = (/im, jm/)
   do ivar=1,ivars
     call rw_slice(start,count,im,jm,1,cvar(ivar),ivar)
   enddo

 end program

 subroutine rw_slice(start,count,im,jm,lm,cvar,ivar)
 use netcdf
 implicit none
 integer, intent(in):: im,jm,lm,ivar
 integer :: i, j
 integer :: ncId, VarId, status
 integer, dimension(2), intent(in) :: start, count
 real, allocatable :: dummy(:,:)
 real*4 :: dumr4
 real*8 :: dumr8
 character *20 :: cname,cvar
 logical :: debug=.false.

   cname='gfs.t06z.sfcanl.nc'
   allocate (dummy(im,jm))
   print*,'allocate ********  ',trim(cvar),' ********'
! nc open
   status = nf90_open(trim(cname),nf90_Write,ncid)
   if(debug) call netcdf_err(status, 'open nc file')
! inquire variable cvar
   status = nf90_inq_varid(ncid,trim(cvar),VarId)
   if(debug) call netcdf_err(status, 'inq varid')
! get variable cvar
   status = nf90_get_var(ncid,VarId,dummy,start,count)
   if(debug) call netcdf_err(status, 'get variable value')

! get new values of surface variables (tmpsfc, land, orog, ...)
   print*,trim(cvar),' minval maxval: ',minval(dummy),maxval(dummy)

   if (ivar.le.4) then
     dummy(:,:)=0.06
   elseif (ivar.eq.5.or.ivar.eq.6) then
     dummy(:,:)=0.0
   elseif (ivar.eq.7) then
     dummy(:,:)=0.995
   elseif (ivar.ge.8.and.ivar.le.23) then
     dummy(:,:)=0.0
   elseif (ivar.ge.24.and.ivar.le.27) then
     dummy(:,:)=288.0
   elseif (ivar.ge.28.and.ivar.le.32) then
     dummy(:,:)=0.0
   elseif (ivar.eq.34) then
     dummy(:,:)=288.0
   elseif (ivar.ge.37.and.ivar.le.40) then
     dummy(:,:)=0.0
   elseif (ivar.ge.57.and.ivar.le.58) then
     dummy(:,:)=12.0
   elseif (ivar.eq.59) then
     dummy(:,:)=0.1
   elseif (ivar.eq.60) then
     dummy(:,:)=288.0
   else ! zonal profile for records 33, 35, 36, and 41-56
     call meridional_profile(dummy,ivar)
   endif

   print*,trim(cvar),' minval maxval: ',minval(dummy),maxval(dummy)

! put new value back in nc file
   status = nf90_put_var(ncid, VarId, dummy, start, count)
   if(debug) call netcdf_err(status, 'after put var')
! close nc file
   status = nf90_close(ncid)
   if(debug) call netcdf_err(status, 'after nc close')
   deallocate(dummy)
   print*,'deallocate ******  ',trim(cvar),' ********'
   return
 end subroutine

 subroutine netcdf_err(ierr,ctext)
   implicit none
   character(len=*)  :: ctext
   integer:: ierr
   print*,trim(ctext),ierr
   if(ierr.ne.0) stop
   return
 end subroutine netcdf_err

 subroutine meridional_profile(data,ivar)
 implicit none
 integer, parameter :: im=3072,jm=1536,imjm=im*jm
 integer :: i,j,ivar
 real*4, dimension (im,jm) :: data
 real*8, dimension (im,jm) :: latitude
 real*8 :: varmin,varmax,delt,temp,phimin,delphi,phi,pi,d2r
!
 varmin=minval(data)
 varmax=maxval(data)
!
!   33 spfh2m
!   35 tmp2m
!   36 tmpsfc
!   41 c0
!   42 cd
!   43 dconv
!   44 dtcool
!   45 qrain
!   46 tref
!   47 w0
!   48 wd
!   49 xs
!   50 xt
!   51 xtts
!   52 xu
!   53 xv
!   54 xz
!   55 xzts
!   56 zc

 if(ivar==33) then ! spfh2m
   varmin=0.0
   varmax=varmax/2.d0
 elseif(ivar==35) then ! tmp2m
   varmin=varmin
   varmax=varmax
 elseif(ivar==36) then ! tmpsfc
   varmin=varmin
   varmax=varmax
 elseif(ivar==41) then ! c0
   varmin=0.0
   varmax=varmax/2.d0
 elseif(ivar==42) then ! cd
   varmin=varmin
   varmax=varmax
 elseif(ivar==43) then ! dconv
   varmin=0.0
   varmax=varmax/100.d0
 elseif(ivar==44) then ! dtcool
   varmin=0.0
   varmax=varmax/6.d0
 elseif(ivar==45) then ! qrain
   varmin=0.0
   varmax=varmax/500.d0
 elseif(ivar==46) then ! tref
   varmin=varmin
   varmax=varmax
 elseif(ivar==47) then ! w0
   varmin=0.0
   varmax=varmax/10.d0
 elseif(ivar==48) then ! wd
   varmin=0.0
   varmax=varmax/10.d0
 elseif(ivar==49) then ! xs
   varmin=0.0
   varmax=varmax
 elseif(ivar==50) then ! xt
   varmin=0.0
   varmax=varmax
 elseif(ivar==51) then ! xtts
   varmin=0.0
   varmax=varmax
 elseif(ivar==52) then ! xu
   varmin=0.0
   varmax=varmax
 elseif(ivar==53) then ! xv
   varmin=0.0
   varmax=varmax
 elseif(ivar==54) then ! xz
   varmin=0.0
   varmax=varmax
 elseif(ivar==55) then ! xzts
   varmin=0.0
   varmax=varmax/100000.d0
 elseif(ivar==56) then ! zc
   varmin=0.0
   varmax=varmax/5.d0
 endif
!
 pi=2.d0*asin(1.d0)
 d2r=pi/180.0d0
 delt=varmax-varmin
!
 delphi=2.d0*90.d0/jm
 phimin=-90.0d0-delphi/2.d0
 temp=phimin
 do j=1,jm/2
   temp=temp+delphi
   latitude(:,j)=temp*d2r
   latitude(:,jm+1-j)=-temp*d2r
 enddo
 do i=1,im
 do j=1,jm
!r --- gradually from -90 to +90 deg latitude (not -60 to +60)
!r if ( abs(latitude(i,j)) < pi/3.0d0) then
     data(i,j)=0.5*(2-(sin(latitude(i,j)))**4-(sin(latitude(i,j)))**2)*delt+varmin
     if(ivar.eq.42) then
        data(i,j)=min(0.0,data(i,j))
     else
        data(i,j)=max(0.0,data(i,j))
     endif
!r else
!r   data(i,j)=varmin
!r endif
 enddo
 enddo
 return
 end subroutine meridional_profile
