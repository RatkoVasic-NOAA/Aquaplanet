 program orog
 implicit none
 integer, parameter :: im=48,jm=48
 integer :: ivar
 integer, dimension(2) :: start, count
 character *50, dimension(7) :: cvar
 character *50 :: cname

   if (IARGC().ne.1) then
     print*,'Use ./profile.x filename '
     stop
   endif
   call getarg(1, cname)
   cvar=(/'slmsk', 'land_frac', 'orog_raw', 'orog_filt', 'elvmax', &
          'lake_depth', 'lake_frac'/)
   start = (/1, 1/)
   count = (/im, jm/)
   do ivar=1,13
     call rw_slice(start,count,im,jm,cvar(ivar),trim(cname))
   enddo

 end program

 subroutine rw_slice(start,count,im,jm,cvar,cname)
 use netcdf
 implicit none
 integer, intent(in):: im,jm
 integer :: i, j, k
 integer :: ncId, VarId, status
 integer, dimension(2), intent(in) :: start, count
 real, allocatable :: dummy(:,:)
 character *50 :: cname,cvar
 logical :: debug=.false.

   allocate (dummy(im,jm))
   print*,'FILE: ********  ',trim(cname),' ********'
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

! set to zero slmask and hgt
   print*,trim(cvar),' minval maxval: ',minval(dummy),maxval(dummy)
   dummy(:,:)=0.
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
   return
 end subroutine netcdf_err
