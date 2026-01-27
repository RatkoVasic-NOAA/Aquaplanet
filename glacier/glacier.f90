      program read_write_grib1
!
      implicit none
      integer i,j,iret,jret,lugb,lugi,lskip,lgrib,ndata
      character*500 fngrib1,fngrib2
      real*4,  allocatable :: data(:)
      logical*1, allocatable :: lbms(:)
      integer kpds(100),kgds(100),jpds(100),jgds(100)
!
      lugb=9998
      fngrib1="global_glacier.2x2.grb"
      fngrib2="new-global_glacier.2x2.grb"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call baopenr(lugb,fngrib1,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print*,'IRET: ',iret
!
      lugi = 0
      lskip   = -1
      jpds    = -1
      jgds    = -1
      jpds(5) = 238
      jpds(6) =  1
      jpds(7) =  0
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
      jpds(9) = 1
      jpds(18) = -1
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
      data=0.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call baopenw(lugb,fngrib2,iret)

      call putgb(lugb,ndata,kpds,kgds,lbms,data,jret)

      print*,'PUT IRET JRET: ',jret
      print*,'BOUNDS: ',lbound(data),ubound(data)
      print*,'MIN-MAX val: ',minval(data),maxval(data)
      call baclose(lugb,iret)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      end
