      program read_write_grib1
!
      implicit none
      integer i,j,iret,jret,lugb,lugi,mon,lskip,lgrib,ndata
      character*500 fngrib1,fngrib2
      real*4,  allocatable :: data(:)
      logical*1, allocatable :: lbms(:)
      integer kpds(100),kgds(100),jpds(100),jgds(100)
!
      lugb=9998
      fngrib1="RTGSST.1982.2012.monthly.clim.grb"
      fngrib2="new-RTGSST.1982.2012.monthly.clim.grb"
      mon=3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call baopenr(lugb,fngrib1,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print*,'IRET: ',iret
!
      lugi = 0
      lskip   = -1
      jpds    = -1
      jgds    = -1
      jpds(5) = 11
      jpds(7) = -1
      kpds    = jpds
      kgds    = jgds
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,lskip,kpds,kgds,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print*,'IRET: ',iret
      print*,'NDATA: ',ndata
!
      write(6,100) ' kpds( 1-10)=',(kpds(j),j= 1,10)
      write(6,100) ' kpds(11-20)=',(kpds(j),j=11,20)
      write(6,100) ' kpds(21-  )=',(kpds(j),j=21,22)
 100  format(A14,x,10(i4))
!
      lskip   = -1
      jpds(4)  = -1
      jpds(9) = mon
      jpds(18) = -1
      if(jpds(9).eq.13) jpds(9) = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      allocate(data(1:ndata))
      allocate(lbms(ndata))
      lbms(:)=.true.
      call getgb(lugb,lugi,ndata,lskip,jpds,jgds,ndata,lskip,kpds,kgds,lbms,data,jret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print*,'GET JRET: ',jret
      print*,'BOUNDS: ',lbound(data),ubound(data)
      print*,'MIN-MAX val: ',minval(data),maxval(data)
      call baclose(lugb,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call create_SST(data)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call baopenw(lugb,fngrib2,iret)

      do mon=1,12
        kpds(9)=mon
        call putgb(lugb,ndata,kpds,kgds,lbms,data,jret)
      enddo

      print*,'PUT IRET JRET: ',jret
      print*,'BOUNDS: ',lbound(data),ubound(data)
      print*,'MIN-MAX val: ',minval(data),maxval(data)
      call baclose(lugb,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      end
      subroutine create_SST(sst)
      implicit none
      integer, parameter :: im=4320,jm=2160
      integer :: i,j
      real*4, dimension (im,jm) :: sst
      real*8, dimension (im,jm) :: latitude
      real*8 :: tmin=0.00,tmax=27.00,delt,temp,phimin,delphi,phi,pi,d2r
      sst(:,:)=0.
!
      pi=2.d0*asin(1.d0)
      d2r=pi/180.0d0
      delt=tmax-tmin
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
        if ( abs(latitude(i,j)) < pi/3.0d0) then
          sst(i,j)=0.5*(2-(sin(latitude(i,j)))**4-(sin(latitude(i,j)))**2)*delt+tmin
        else
          sst(i,j)=0.
        endif
        sst(i,j)=sst(i,j)+273.15
      enddo
      enddo
      return
      end subroutine create_SST
