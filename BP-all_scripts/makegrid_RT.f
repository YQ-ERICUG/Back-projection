      character*80 ctmp
 
      open(20,file='BP_para.txt',status='old')
        read(20,*) hypolon,hypolat,az
        read(20,*) gridint
        read(20,*) nstrike1,nstrike2,nperp1,nperp2 
      close(20)


c based on the equations in http://www.opensha.org/glossary-magScalingRelation

      hypodepth = 15    ! used in the travel time table 

c  no. of grid points in strike dir is nstrike1 + nstrike2 + 1
c  no. of grid points in perpendicula dir is ndip1 + ndip2 + 1
c-------------------------------------------------------------

      pi= 3.1416
      az = az*pi/180.
c     dip = dip*pi/180.
      itmp=(nstrike1+1+nstrike2)*(nperp1+1+nperp2)+1    
      int_itmp = int(itmp/10)
      jtmp=(int_itmp+1)*10-itmp

      write(3,*) itmp+jtmp
      write(3,*) hypolon,hypolat 

      do i=1,jtmp
        write(3,*) hypolon,hypolat 
      end do


      a0=hypolon;b0=hypolat
      k=0
      do i=1,nstrike2+nstrike1+1
        iraw=i-nstrike2-1
        dist=gridint*iraw
	b1=b0+dist*cos(az)/111.
        a1=a0+180*dist*sin(az)/(6371*pi)
c	write(3,*) a1,b1
        do j=1,nperp2+nperp1+1
          icol=j-nperp2-1
          dist=gridint*icol
          az2=pi/2.+az
	  b2=b1+dist*cos(az2)/111.
          a2=a1+180*dist*sin(az2)/(6371*pi)            
	  write(3,*) a2,b2
          k=K+1
c	  write(*,*) a2,b2,K
        end do
      end do

800   format(2i3,4f9.4,f9.1)
      call system('mv fort.3 subs.dat')

      end
