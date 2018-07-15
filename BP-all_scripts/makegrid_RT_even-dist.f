      character*80 ctmp

c set a grid that has the even distance between the grid points

c az doesn't work here!!!!!!!!!!!!!!!!!!!!!!!!!!
      az= 10  !angle from north, the positive dir along strike
 
      gridint = 15.0 !space interval between points
c      npts=20
 
c      open(20,file='FK_folder/Triggering')
c        read(20,*) hypolon,hypolat, ctmp
c        write(*,*) hypolon,hypolat, ctmp
c      close(20)
       hypolon=75
       hypolat=20

c based on the equations in http://www.opensha.org/glossary-magScalingRelation
      hypodepth = 15    ! used in the travel time table 
      tlength=10**(-3.22+0.69*tmag)
c      npts=int(tlength*1.0*1.5/gridint)
c       npts=60
      
c      gridint=int(tlength*1.0*1.5/npts)
      write(*,*) gridint
c  no. of grid points in strike dir is nstrike1 + nstrike2 + 1
c  no. of grid points in perpendicula dir is ndip1 + ndip2 + 1
      nstrike1 = 10 ! no. grid points in positive dir along strike (north)
      nstrike2 = 25 ! no. grid points in negative dir along strike (south) 
      nperp1 = 5   ! no. grid points eastward to hypocenter 
      nperp2 = 15  ! no.  grid points westward to  hypocenter
c-------------------------------------------------------------
      pi= 3.1416
c      az = az*pi/180.
c      dip = dip*pi/180.
      itmp=(nstrike1+1+nstrike2)*(nperp1+1+nperp2)+1    
      int_itmp = int(itmp/10)
      jtmp=(int_itmp+1)*10-itmp

      write(3,*) itmp+jtmp
      write(3,*) hypolon,hypolat 

      do i=1,jtmp
        write(3,*) hypolon,hypolat 
      end do
       
c      write(*,*) hypolon,hypolat,'ok?'
c       hypolon=15.514; hypolat=0.00


      a0=hypolon;b0=hypolat
      k=0
      do i=1,nstrike2+nstrike1+1
        iraw=i-nstrike2-1
        dist=gridint*iraw
ccccccccccccccccccccccccccc calculate a2
         eplat=b0;eplon=a0
         RAD=0.0174533
         rup_dir1=az+(180/2.) 
         tmp=cos(eplat*RAD)*sin((dist/111.195)*RAD)*cos(rup_dir1*RAD)
     &+cos((dist/111.195)*RAD)*sin(eplat*RAD)
c         b1 = (asin(tmp))/RAD !subevent latitude  
         tmp=sin((dist/111.195)*RAD)*sin(rup_dir1*RAD)/sqrt(1-tmp**2)
         a2 = (asin(tmp))/RAD + eplon
c         write(*,*) a2, a0,rup_dir1,iraw,dist 
ccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccc calculate b2
        do j=1,nperp2+nperp1+1
          icol=j-nperp2-1
          dist=gridint*icol
	  b2=b0+dist*cos(az*RAD)/111.
ccccccccccccccccccccccccccccccccccccccccc       
	  write(3,*) a2,b2,j,i,icol,iraw
          k=K+1
c	  write(*,*) a2,b2,K
        end do
      end do

 800     format(2i3,4f9.4,f9.1)
      call system('mv fort.3 subs.dat')
         stop
         end
