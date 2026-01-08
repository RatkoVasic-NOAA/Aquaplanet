 program vertical_profile
 implicit none
 integer, parameter :: im=3072,jm=1536
 integer :: ivar
 integer, dimension(3) :: start, count
 character *8, dimension(13) :: cvar
 character *8,               :: cvarsfc

 cvar=(/'clwmr','grle','icmr','o3mr','rwmr','snmr' &
       ,'spfh','delz','dpres','tmp','dzdt','ugrd','vgrd'/)

   do ivar=1,13
!------- first pass (slice 3D)
     start = (/1, 1, 1/)
     count = (/im, jm, 63/)
     call rw_slice(start,count,im,jm,63,cvar(ivar),ivar)
!------- second pass (slice 3D)
     start = (/1, 1, 64/)
     count = (/im, jm, 64/)
     call rw_slice(start,count,im,jm,64,cvar(ivar),ivar)
   enddo
!------- hgtsfc (1D)
   start = (/1, 1, 1/)
   count = (/im, jm, 1/)
   cvarsfc='hgtsfc'
   call rw_slice(start,count,im,jm,1,cvarsfc,14)
!------- pressfc (1D)
   start = (/1, 1, 1/)
   count = (/im, jm, 1/)
   cvarsfc='pressfc'
   call rw_slice(start,count,im,jm,1,cvarsfc,15)

 end program

 subroutine rw_slice(start,count,im,jm,lm,cvar,ivar)
 use netcdf
 implicit none
 integer, intent(in):: im,jm,lm,ivar
 integer :: i, j, k
 integer :: ncId, VarId, status
 integer, dimension(3), intent(in) :: start, count
 real, allocatable :: dummy(:,:,:)
 real*4 :: dumr4
 real*8 :: dumr8
 character *8 :: cvar
 character *18 :: cname
 logical :: debug=.true.

   cname='gfs.t06z.atmanl.nc'
   allocate (dummy(im,jm,lm))
   print*,'allocate ********  ',trim(cvar),' ********'
! nc open
   status = nf90_open(trim(cname),nf90_Write,ncid)
   if(debug) call netcdf_err(status, 'open nc file')
! inquire variable cvar
   status = nf90_inq_varid(ncid,trim(cvar),VarId)
   if(debug) call netcdf_err(status, 'inq varid')
! get slice of variable cvar
   status = nf90_get_var(ncid,VarId,dummy,start,count)
   if(debug) call netcdf_err(status, 'get variable value')

! get zonal mean of certain variables (tmp, rh, sh, ...)
   print*,trim(cvar),' minval maxval: ',minval(dummy),maxval(dummy)

   if (ivar .le. 10) then
     do k=1,lm
       call meridional_profile(dummy(:,:,k),ivar,k)
     enddo
   endif
   if (ivar .eq. 11) dummy(:,:,:)=0.
   if (ivar .eq. 12) dummy(:,:,:)=0.
   if (ivar .eq. 13) dummy(:,:,:)=0.
   if (ivar .eq. 14) dummy(:,:,:)=0.
   if (ivar .eq. 15) dummy(:,:,:)=101325.

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

 subroutine meridional_profile(data,ivar,k)
 implicit none
 integer, parameter :: im=3072,jm=1536,imjm=im*jm
 integer :: i,j,ivar,k
 real*4, dimension (im,jm) :: data
 real*8, dimension (im,jm) :: latitude
 real*8 :: varmin,varmax,delt,temp,phimin,delphi,phi,pi,d2r
!
 varmin=minval(data)
 varmax=maxval(data)
!
 if(ivar==1) then ! clwmr
   varmin=0.0
   varmax=varmax/325.d0
 elseif(ivar==2) then ! grle
   varmin=0.0
   varmax=varmax/15000.d0
 elseif(ivar==3) then ! icmr
   varmin=0.0
   varmax=varmax/200.d0
 elseif(ivar==4) then ! o3mr
   varmin=0.0
   varmax=varmax
 elseif(ivar==5) then ! rwmr
   varmin=0.0
   varmax=varmax/800.d0
 elseif(ivar==6) then ! snmr
   varmin=0.0
   varmax=varmax/800.d0
 elseif(ivar==7) then ! spfh
   varmin=0.0
   varmax=varmax/1.7d0
 elseif(ivar==8) then ! delz
   temp=0.d0
   do i=1,im
   do j=1,jm
     temp=temp+data(i,j)
   enddo
   enddo
     data(:,:)=temp/imjm
   return
 elseif(ivar==9) then ! dpres
   temp=0.d0
   do i=1,im
   do j=1,jm
     temp=temp+data(i,j)
   enddo
   enddo
     data(:,:)=temp/imjm
   return
 elseif(ivar==10) then ! tmp
   varmin=varmin
   varmax=varmax
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
     data(i,j)=max(0.0,data(i,j))
!r else
!r   data(i,j)=varmin
!r endif
 enddo
 enddo
 return
 end subroutine meridional_profile
