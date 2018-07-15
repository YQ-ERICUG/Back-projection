      parameter(nsta=800,nnpt=1000000,npt=200000, nmax=200000)

      dimension seis(nsta,npt), seis2(nsta,npt), seis3(nsta,npt)
      dimension tt(160000), tdiff(nsta),stlat(nsta),stlon(nsta)
      dimension x(npt),tot(npt),xm(npt),x2(npt),yarray(npt)
      dimension xx(nnpt)
      dimension glat(5000), glon(5000)
      dimension stacks(5000,200)
      double precision dtemp1,dtemp2,dtemp3,dtemp4

      character*80 file1,file2(nsta),file22,fin
 
      character*4 stanm(nsta),csta,net
      character*8 filter_indicator,file3
      integer  ista_tag(nsta),nzmin(nsta),nzsec(nsta),
     &nzmsec(nsta),nzhour(nsta)

      dimension dist(nsta)


 900  format(a72)
 901  format(a72)
        
      call system('rm -rf *.V *.K *.M')
      open(8,file='stack.out')


c  set basic parameters

      open(41,file='BP_para.txt', status='old')
        read(41,*) evlo,evla
        read(41,*) pstart,imod,xlon1,xlat1
        read(41,*) tomhh,tommm,tomss,tomms
        read(41,*) cl,ch,nsam,P_length
        read(41,*) window_length,lag_lenth,cc_min
      close(41)
      
c filter parameters

      ih= 2
      il= 2 

      write(*,*) 'Filter the data? [Y/N]'
      filter_indicator='Y'  
      write(*,*) filter_indicator


c cross correlation parameters

       istxm = pstart*nsam         ! start of model
       istx  = pstart*nsam         ! start of data
       nwind = window_length*nsam  ! length of window
       nlag  = lag_lenth*nsam      ! no of lags


c other parameters

      ini_ttime=0     !set the binginning of the ttime data
      sta_min = 0.003 !set station minmum distance 

c  read travel time data

      open(31,file='ttime_taup-p-15km-high.out', status='old')
      read(31,*) nttime
      do i=1,nttime
         read(31,*)temp1, tt(i)
      end do
      close(31)

c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


c read grid points

      open(32,file='subs.dat', status='old')
      read(32,*)ngrid
      do i=1,ngrid
         read(32,*) glon(i),glat(i)
      end do
      write(*,*)ngrid,'grid'
      close(32)

c read data
   
      write(*,*)'All File names listed in the stations.txt'
      read(*,900)file1
      open(33,file=file1, status='old')


      read(33,*)nsta2
      do ista=1,nsta2
         read(33,901)file2(ista)
         call rsac1(file2(ista),xx,ndata,beg,dt,nmax,nerr)

c check sampling 

	 if (((dt*nsam)-1) .gt. 0.00001) then
           write(*,*)'something wrong with delta'
           stop
         end if


c retrieve P wave only

        ndata2 =  P_length*nsam
  
        if(ndata*dt .gt. P_length) then
          do i=1,ndata2
            x(i)=xx(i)
          end do
        else
          do i=1,ndata
            x(i)=xx(i)
          end do           
          do i=ndata,ndata2
           x(i)=0
          end do           
        end if
        ndata=ndata2

c read refrence time

	 call getnhv('NZHOUR',nzhour(ista),nerr)
	 call getnhv('NZMIN',nzmin(ista),nerr)
	 call getnhv('NZSEC',nzsec(ista),nerr)
	 call getnhv('NZMSEC',nzmsec(ista),nerr)

c read station name
         
c         call getkhv('KSTNM',stanm(ista),nerr)   !sometimes wrong
         CALL netsta_modified(file2(ista),net,stanm(ista))

c remove mean

         TOTs = 0.0
         DO 60 I=1,NDATA
  60	 TOTs=TOTs + X(I)
         TOTs = TOTs/NDATA
         DO 61 I=1,NDATA
  61	 X(I)=X(I) - TOTs

c taper

         call windo2(x,ndata,2,0.1)

c filter

         if(filter_indicator .eq. 'N')go to 112 !skip the filtering if required
         if(ch.eq.0.0.and.ih.eq.0)go to 110                                
         call bwthhi(x,x,ndata,ih,dt,ch)                                   
         call revrs(x,ndata)                                               
         call bwthhi(x,x,ndata,ih,dt,ch)                                   
         call revrs(x,ndata)                                               
 110     if(cl.eq.0.0.and.il.eq.0) go to 112                                
         call bwthlo(x,x,ndata,il,dt,cl)                                   
         call revrs(x,ndata)                                               
         call bwthlo(x,x,ndata,il,dt,cl)                                   
         call revrs(x,ndata)                                               
 112     call windo2(x,ndata,2,amt)           
         
         call getfhv('STLA',stlat(ista),nerr)
         call getfhv('STLO',stlon(ista),nerr)


c    delete the station over 98.3 degrees

           call dist_calculation(stlat(ista),stlon(ista)
     &,evla,evlo,temp)
c         if (temp .lt. 30)  ista_tag(ista)=1  
         if (temp .gt. 98.3)  then
          write(*,*) file2(ista),temp 
          n=len_trim(file2(ista))
          write(fin,*) 'rm '(1:3),file2(ista)(1:n)
          write(*,*) fin
          call system(fin) 
         end if

c    take off some dense stations
	  
	 do i=1,ista-1
           call dist_calculation(stlat(ista),stlon(ista)
     &,stlat(i),stlon(i),tdist)
	   if(tdist.lt.sta_min .and.ista_tag(i).eq.0) exit
	 end do

	 if(i .ne. ista) ista_tag(ista)=1  
	 if (ista .eq. imod ) ista_tag(ista)=0

c normalize

         dmin=0.
	 dmax=0.

         do i=1,ndata
	   if (x(i) .gt. dmax ) then 
	     dmax=x(i)
	   else if (x(i) .lt. dmin ) then
	     dmin=x(i)
	   end if	 
	 end do  
         if(abs(dmin).gt.dmax)dmax = abs(dmin)
         do i=1,ndata
            x(i) = x(i)/dmax
         end do   

         do i=1,ndata
            seis(ista,i)=x(i)
         end do   


      end do
      close(33)

c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


c loop for each grid point

      do igrid = 1, ngrid
         write(*,*)igrid,'st grid'
         plat1 = glat(igrid)
         plon1 = glon(igrid)

         pi=3.1415926
         dtemp1=cos((90-plat1)*pi/180)
     &*cos((90-xlat1)*pi/180)
         dtemp2=sin((90-plat1)*pi/180)*sin((90-xlat1)*pi/180)
         dtemp3=dtemp2*cos((xlon1-plon1)*pi/180)
         distc=(180./pi)*acos(dtemp1+dtemp3)   ! distance from grid point to model station        

         idistc=nint(distc*1000)-ini_ttime+1
c         idistc = nint(distc) - 3448 + 1
         ttc = tt(idistc)  ! P arrival time from model station in theory

c set delays

      do ista=1,nsta2  
         
         dtemp1=cos((90-plat1)*pi/180)
     &*cos((90-stlat(ista))*pi/180)
        
         dtemp2=sin((90-plat1)*pi/180)*sin((90-stlat(ista))*pi/180)
         dtemp3=dtemp2*cos((stlon(ista)-plon1)*pi/180)

         dist1=(180/pi)*acos(dtemp1+dtemp3)   ! distance from grid point to station        
c         idist1 = nint(dist1) -3448 + 1
         idist1=nint(dist1*1000)-ini_ttime+1
         ttime = tt(idist1)

         tdiff(ista) = ttime-ttc -
     &(((nzhour(ista)-tomhh)*60+nzmin(ista)-tommm)*60+ 
     &nzsec(ista)-tomss+ 
     &(nzmsec(ista)-tomms)/1000.)     ! theoretic time interval between model station and other station 

         itdiff = nint(tdiff(ista)/dt)

c time shift according to theoretic time interval     

         k=1
         do i=1, ndata
            j = i + itdiff
            if(j.ge.1) then
              x(k) = seis(ista,j)
            else
              x(k) = 0.0
            end if  
            k = k + 1
         end do            
         do i = 1,ndata
            seis2(ista,i)=x(i)
         end do   
      end do         
 


c 1st cross correlate with model waveform for 1st grid point

      if(igrid.eq.1)then
       open(40,file='cc30_raw.out') 
        write(*,*)'cross correlate waveforms' 
        do ista = 1, nsta2
          do i = 1, ndata
             x(i) = seis2(ista,i)
             xm(i) = seis2(imod,i)                    !model station
          end do   
         
c caculate cross correlate
  
          call COHERE(XM,X,ISTXM,ISTX,NWIND,NLAG,
     1        COHMIN, IMIN, COHMAX,IMAX)

c          if (ista_tag(ista) .eq. 0) then
          write(*,'(I8,a7,f18.6)')imax,stanm(ista),cohmax
          write(40,'(I8,a7,f18.6)')imax,stanm(ista),cohmax
c           write(*,'(I8,a7)')imax,stanm(ista)
c          end if 
          if (cohmax .lt. cc_min ) ista_tag(ista)=1  !mark bad data
          if (ista_tag(ista) .eq. 1) cycle           !skip bad data
          
c time shift according to Maximum cross-correlation and save as '.M' format

          k=1
          do i=1,ndata
            j=i+imax
            if(j.gt.0) then
              x2(k)=seis(ista,j)
              seis2(ista,k)= x(j)
            else
              x2(k)=0.0
              seis2(ista,k)=0.0
            end if
            k = k + 1
          end do 
          do i=1,ndata
             seis(ista,i)= x2(i)
          end do   
          
  
          do i=1,ndata
            x(i) = seis2(ista,i)
          end do
          do i=ndata+1,npt   !to prevent the strange tail that stopped me serveral days
            x(i)=0
          end do
c          k=10+ista
c          write(k,*)(x(i),i=1,ndata)
c	  if (ista_tag(ista) .eq. 1) cycle
c       call rsac1(file2(ista),yarray,ndata2,beg,dt,nmax,nerr)
          call setnhv('NPTS',ndata2,nerr) 
          call setkhv('KSTNM',stanm(ista),nerr)
          call setfhv('STLA',stlat(ista),nerr)
          call setfhv('STLO',stlon(ista),nerr)
          csta=stanm(ista)
          file3=csta(1:len_trim(csta))//'.M'
c          write(*,*)csta,file3,'/////////'

c          write(file3,*)csta(1:4),'.V'(1:2)  !VERY BAD!!!!!!!!!     
           call wsac0(file3,temp,x,nerr)
          if (nerr .ne. 0) stop

        end do
        close(40)

       write(*,*) '1st grid match has been finished' 
       end if 

       stop
         
      end do

      
      close(8)
      stop
      end

c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
c_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



	SUBROUTINE APART (PLAT1, PLON1, PLAT2, PLON2, DIST, AZ)

C	Determine the distance (DIST), in KM, between two points
C	and the azimuth (AZ) from point1 to point2

	CALL DEG_KM (PLAT2, PLON2, PLAT1, PLON1, X, Y)

	DIST = SQRT(X**2 + Y**2)

	AZ = AZIMUTH (0.0,0.0,X,Y)

	RETURN
	END
C ----------------------------------------------------------------------
	REAL FUNCTION AZIMUTH (PLAT1, PLON1, PLAT2, PLON2)

C	Determine azimuth from point1 to point2 in positive degrees
C	measured clockwise from north = 0

	IF (PLAT1 .EQ. PLAT2 .AND. PLON1 .EQ. PLON2) THEN
		AZIMUTH = 0.0		!special case, same point
	ELSE
c		AZIMUTH = ATAN2D(PLAT2 - PLAT1, PLON2 - PLON1)
	        temp1 = PLAT2 - PLAT1
		temp2 = PLON2 - PLON1
		azimuth = atan2(temp1,temp2)
                azimuth = azimuth * 180.0/3.1416
		IF (AZIMUTH .LT. 0.0) AZIMUTH = AZIMUTH + 360.
	END IF

	RETURN
	END

C ----------------------------------------------------------------------
	SUBROUTINE DEG_KM
	1		(PLAT, PLON,	!Coords of the point
	1		 CLAT, CLON,	!Coords of the origin
	1		 X, Y)		!x-y of the point in km from origin

c ------------------------------
c	Given a latitude and a longitude, and the lat and long of the origin
c	return the x and y offset of the point from the origin on the surface
c	in kilometers.  
c ------------------------------

	PARAMETER (R = 6371.0,		!radius of the earth
	1	 FAC = 0.01745329)	!degrees to radians

c --	convert to radians (don't convert (contaminate) original values)

	  DLAT = (PLAT - CLAT) * FAC
c	  DLON = (PLON - CLON) * FAC

	  olat = clat * FAC
	  olon = clon * FAC

c	  rlat = plat * FAC
	  rlon = plon * FAC

	  Y = R * DLAT
	  X = (RLON - OLON) * R * COS((DLAT/2.0)+OLAT)

	 RETURN
	  END

C ----------------------------------------------------------------------

	SUBROUTINE COHERE(XM,X,ISTXM,ISTX,NWIND,NLAG,
     1     COHMIN, IMIN, COHMAX,IMAX)


C      CROSS CORRELATE XM AND X (XM IS MODEL, X IS DATA TO TEST)
C      ISTXM, ISTX = STARTING INDICES TO CROSS CORRELATE
C      NWIND = NO. OF SAMPLES IN WINDOW TO CORRELATE
C      NLAG = +/- NO. OF LAGS TO TRY, SO 2*NLAGS + 1 ARE TESTED 

C      OUTPUT:	
C	  COHMIN = SMALLEST COHERENCE AT LAG IMIN
C         COHMAX = LARGEST COHERENCE AT LAG IMAX
C
C

	DIMENSION XM(1), X(1)

	COHMIN = 1000000.
	COHMAX = -1000000.
	NLAGS = 2*NLAG + 1
	DO 120 I=1,NLAGS
	  ILAG = (-1 * NLAG) + I - 1
	  TOT = 0.
	  FNORM1=0.
	  FNORM2=0.
	  DO 125 J = 1,NWIND
	    IF((J+ILAG+ISTX).LT.1) GOTO 125
	    TOT = TOT + XM(J+ISTXM)*X(J+ILAG+ISTX)        
	    FNORM1 = FNORM1 + XM(J+ISTXM)**2.
	    FNORM2 = FNORM2 + X(J+ILAG+ISTX)**2.
 125	  CONTINUE	

	  TOT = TOT/(FNORM1*FNORM2)**0.5

	  IF(TOT.GT.COHMAX) THEN
	    COHMAX=TOT
	    IMAX = I
	  ENDIF

	  IF(TOT.LT.COHMIN) THEN
	    COHMIN=TOT
	    IMIN = I
	  ENDIF

 120	CONTINUE

	IMIN = IMIN - NLAG - 1
	IMAX = IMAX - NLAG - 1

	RETURN
	END 


C ----------------------------------------------------------------------

      subroutine bwthhi(x,y,n,iord,dt,w3db)                             
c                                                                       
c   11 apr 79                                                           
c                                                                       
c   butterworth high-pass filter.    see gold & rader 1969, p.72.       
c                                                                       
c   unity gain                                                          
c   input array     x(n)                                                
c   output array    y(n)    (may be the same as x)                      
c   iord     order of filter   may be 1, 2 or 3.                        
c   dt       timestep of series                                         
c   w3db     3db point of filter                                        
c                                                                       
c   iord .gt. 0   designs new filter and filters data.                  
c        .lt. 0   uses old filter and filters new data.                 
c        .eq. 0   uses old filter and carries on.                       
c                                                                       
      dimension x(10000),y(10000)
      data pi/3.1415926/                                                
c                                                                       
      is=1                                                              
      if(iord.eq.0) go to (13,23,33), iordl                             
      iordm=-iord                                                       
      if(iord.lt.0) go to (12,22,32), iordm                             
      iordl=iord                                                        
c                                                                       
      wc=tan(pi*dt*w3db)                                                
      go to (10,20,30), iord                                            
c                                                                       
c   1st order filter                                                    
10    a=1./(1.+wc)                                                      
      b=-1.0 * (1.-wc)*a                                                      
100   format(/' a, b = ',2g15.7)                                        
c                                                                       
12    x1=x(1)                                                           
      y(1)=a*x(1)                                                       
      y1=y(1)                                                           
      is=2                                                              
13    do 11 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0-x1) - b*y1                                             
      y1=y(i)                                                           
      x1=x0                                                             
11    continue                                                          
c                                                                       
      return                                                            
c                                                                       
c   2nd order filter                                                    
20    wcr2=wc*sqrt(2.)                                                  
      wc2=wc*wc                                                         
      a=1./(1.+wcr2+wc2)                                                
      b=-2.*(1.-wc2)/(1.+wcr2+wc2)                                      
      c=(1.-wcr2+wc2)/(1.+wcr2+wc2)                                     
200   format(/' a, b, c = ',3g15.7)                                     
c                                                                       
22    x0=x(1)                                                           
      y(1)=a*x0                                                         
      y1=y(1)                                                           
      x1=x0                                                             
      x0=x(2)                                                           
      y(2)=a*(x0-x1-x1) - b*y1                                          
      y2=y1                                                             
      y1=y(2)                                                           
      x2=x1                                                             
      x1=x0                                                             
      is=3                                                              
23    do 21 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0-x1-x1+x2) - b*y1 - c*y2                                
      y2=y1                                                             
      y1=y(i)                                                           
      x2=x1                                                             
      x1=x0                                                             
21    continue                                                          
c                                                                       
      return                                                            
c                                                                       
c   3rd order filter                                                    
c                                                                       
30    wc2=wc*wc                                                         
      wca=1.+wc+wc2                                                     
      wcb=1.-wc+wc2                                                     
      wcc=1.-wc2                                                        
      wcd=wca*(1.+wc)                                                   
      a=1./wcd                                                          
      b=(wca*(-1.+wc) - 2.*(1.+wc)*wcc)/wcd                             
      c=(wcb*(1.+wc)  + 2.*(1.-wc)*wcc)/wcd                             
      d=wcb*(+wc-1.)/wcd                                                
300   format(/' a, b, c, d = ',4g15.7)                                  
c                                                                       
32    x0=x(1)                                                           
      y(1)=a*x0                                                         
      y1=y(1)                                                           
      x1=x0                                                             
      x0=x(2)                                                           
      y(2)=a*(x0-x1-x1-x1) - b*y1                                       
      y2=y1                                                             
      y1=y(2)                                                           
      x2=x1                                                             
      x1=x0                                                             
      x0=x(3)                                                           
      y(3)=a*(x0-x1-x1-x1+x2+x2+x2) - b*y1 - c*y2                       
      y3=y2                                                             
      y2=y1                                                             
      y1=y(3)                                                           
      x3=x2                                                             
      x2=x1                                                             
      x1=x0                                                             
      is=4                                                              
33    do 31 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0-x1-x1-x1+x2+x2+x2-x3) - b*y1 - c*y2 - d*y3             
      y3=y2                                                             
      y2=y1                                                             
      y1=y(i)                                                           
      x3=x2                                                             
      x2=x1                                                             
      x1=x0                                                             
31    continue                                                          
c                                                                       
      return                                                            
      end       
                                                        
C ----------------------------------------------------------------------
                                                                       
      subroutine bwthlo(x,y,n,iord,dt,w3db)                             
c                                                                       
c   11 apr 79                                                           
c                                                                       
c   butterworth high-pass filter.    see gold & rader 1969, p.72.       
c                                                                       
c   unity gain                                                          
c   input array     x(n)                                                
c   output array    y(n)    (may be the same as x)                      
c   iord     order of filter   may be 1, 2 or 3.                        
c   dt       timestep of series                                         
c   w3db     3db point of filter                                        
c                                                                       
c   iord .gt. 0   designs new filter and filters data.                  
c        .lt. 0   uses old filter and filters new data.                 
c        .eq. 0   uses old filter and carries on.                       
c                                                                       
      dimension x(10000),y(10000)                                               
      data pi/3.1415926/                                                
c                                                                       
      is=1                                                              
      if(iord.eq.0) go to (13,23,33), iordl                             
      iordm=-iord                                                       
      if(iord.lt.0) go to (12,22,32), iordm                             
      iordl=iord                                                        
c                                                                       
      wc=tan(pi*dt*w3db)                                                
      go to (10,20,30), iord                                            
c                                                                       
c   1st order filter                                                    
10    a=wc/(wc+1.)                                                      
      b=(wc-1.)/(wc+1.)                                                 
100   format(/' a, b = ',2g15.7)                                        
c                                                                       
12    x1=x(1)                                                           
      y(1)=a*x(1)                                                       
      y1=y(1)                                                           
      is=2                                                              
13    do 11 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0+x1) - b*y1                                             
      y1=y(i)                                                           
      x1=x0                                                             
11    continue                                                          
c                                                                       
      return                                                            
c                                                                       
c   2nd order filter                                                    
20    wcr2=wc*sqrt(2.)                                                  
      wc2=wc*wc                                                         
      a=wc2/(1.+wcr2+wc2)                                               
      b=-2.*(1.-wc2)/(1.+wcr2+wc2)                                      
      c=(1.-wcr2+wc2)/(1.+wcr2+wc2)                                     
200   format(/' a, b, c = ',3g15.7)                                     
c                                                                       
22    x0=x(1)                                                           
      y(1)=a*x0                                                         
      y1=y(1)                                                           
      x1=x0                                                             
      x0=x(2)                                                           
      y(2)=a*(x0+x1+x1) - b*y1                                          
      y2=y1                                                             
      y1=y(2)                                                           
      x2=x1                                                             
      x1=x0                                                             
      is=3                                                              
23    do 21 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0+x1+x1+x2) - b*y1 - c*y2                                
      y2=y1                                                             
      y1=y(i)                                                           
      x2=x1                                                             
      x1=x0                                                             
21    continue                                                          
c                                                                       
      return                                                            
c                                                                       
c   3rd order filter                                                    
c                                                                       
30    wc2=wc*wc                                                         
      wc3=wc*wc2                                                        
      wca=1.+wc+wc2                                                     
      wcb=1.-wc+wc2                                                     
      wcc=1.-wc2                                                        
      wcd=wca*(1.+wc)                                                   
      a=wc3/wcd                                                         
      b=(wca*(-1.+wc) - 2.*(1.+wc)*wcc)/wcd                             
      c=(wcb*(1.+wc)  + 2.*(1.-wc)*wcc)/wcd                             
      d=wcb*(+wc-1.)/wcd                                                
300   format(/' a, b, c, d = ',4g15.7)                                  
c                                                                       
32    x0=x(1)                                                           
      y(1)=a*x0                                                         
      y1=y(1)                                                           
      x1=x0                                                             
      x0=x(2)                                                           
      y(2)=a*(x0+x1+x1+x1) - b*y1                                       
      y2=y1                                                             
      y1=y(2)                                                           
      x2=x1                                                             
      x1=x0                                                             
      x0=x(3)                                                           
      y(3)=a*(x0+x1+x1+x1+x2+x2+x2) - b*y1 - c*y2                       
      y3=y2                                                             
      y2=y1                                                             
      y1=y(3)                                                           
      x3=x2                                                             
      x2=x1                                                             
      x1=x0                                                             
      is=4                                                              
33    do 31 i=is,n                                                      
      x0=x(i)                                                           
      y(i)=a*(x0+x1+x1+x1+x2+x2+x2+x3) - b*y1 - c*y2 - d*y3             
      y3=y2                                                             
      y2=y1                                                             
      y1=y(i)                                                           
      x3=x2                                                             
      x2=x1                                                             
      x1=x0                                                             
31    continue                                                          
c                                                                       
      return                                                            
      end                                                               
c                                                                       
c                                                                       
      subroutine revrs(x,n)                                             
c                                                                       
c     this subroutine reverses the order of the array x                 
c                                                                       
      dimension x(1)                                                    
      nn=int(n/2)                                                       
      do 1 i=1,nn                                                       
      j=n-i+1                                                           
      x1=x(i)                                                           
      x2=x(j)                                                           
      x(i)=x2                                                           
   1  x(j)=x1                                                           
      return                                                            
      end                                                               
c                                                                       
C ----------------------------------------------------------------------
                                                                       
      subroutine windo2 (y, np, lw, amt)                                
      dimension y(np)                                                   
c                                                                       
c  amt is the fraction of sample to taper                               
c                                                                       
       if(amt.eq.0.0) return                                            
        ffl=np*amt                                               
        l=ffl                                                     
      do 60 i = 1, l                                                    
      if (lw - 2) 10, 20, 29                                            
   29 if (lw - 4) 30, 40, 40                                            
c        10 is for a gate                                               
   10 f    = 1.0                                                        
      go to 50                                                          
c        20 is a bartlett window                                        
  20  f=1.*i/(l+1.)                                                     
      go to 50                                                          
c        30 is a tukey window                                           
  30  f=1.-.5*(1.+cos(3.14159*i/(l+1.)))                                
      go to 50                                                          
c        40 is a parzen window                                          
  40  x=1.*i/(l+1.)                                                     
      if (x - 0.5) 41, 41, 42                                           
   41 f      = 6.0 * x **2 - 6.0 * x ** 3                               
      go to 50                                                          
   42 f    = 1.0 - 2.0 * (1.0 - x)** 3                                  
   50 y(i) = y(i) * f                                                   
      m = np - i + 1                                                    
      y(m) = y(m) * f                                                   
   60 continue                                                          
      return                                                            
      end                                                               
c
C----------------------------------------------------------------------                                                                      
c                                                                       
      subroutine windo3 (y, np, lw, amt, IEND)                                
      dimension y(1)                                                   
c                                                                       
c  amt is the fraction of sample to taper                               
c  IF IEND = 0 TAPERS FRONT END ONLY
c  IF IEND = 1 TAPERS BACK END ONLY	                                                                      
       if(amt.eq.0.0) return                                            
        ffl=np*amt                                               
        l=ffl                                                     
      do 60 i = 1, l                                                    
      if (lw - 2) 10, 20, 29                                            
   29 if (lw - 4) 30, 40, 40                                            
c        10 is for a gate                                               
   10 f    = 1.0                                                        
      go to 50                                                          
c        20 is a bartlett window                                        
  20  f=1.*i/(l+1.)                                                     
      go to 50                                                          
c        30 is a tukey window                                           
  30  f=1.-.5*(1.+cos(3.14159*i/(l+1.)))                                
      go to 50                                                          
c        40 is a parzen window                                          
  40  x=1.*i/(l+1.)                                                     
      if (x - 0.5) 41, 41, 42                                           
   41 f      = 6.0 * x **2 - 6.0 * x ** 3                               
      go to 50                                                          
   42 f    = 1.0 - 2.0 * (1.0 - x)** 3                                  
   50 continue

      if(iend.eq.0)then
        y(i) = y(i) * f                                                   
      else if(iend.eq.1) then  
        m = np - i + 1	                         
        y(m) = y(m) * f
      end if

   60 continue                                                          
      return                                                            
      end                                                               

C ----------------------------------------------------------------------

c  subroutines      
      subroutine netsta_modified(ss,net,sta)
      character*80 ss, onsett,bandw
      character*4  net, sta
      integer      ndotpoint(20)

      ndot = 0
      do i=1, 80
          if (ss(i:i).eq.'.') then
              ndot = ndot + 1
              ndotpoint(ndot) = i
          end if
      end do
 
      net = ss(ndotpoint(6)+1:ndotpoint(7)-1)
      sta = ss(ndotpoint(7)+1:ndotpoint(8)-1)
      
      end

      subroutine dist_calculation(xlat1,xlon1,plat1,plon1,dist_degree)

         pi=3.1415926
         dtemp1=cos((90-plat1)*pi/180)
     &*cos((90-xlat1)*pi/180)
         dtemp2=sin((90-plat1)*pi/180)*sin((90-xlat1)*pi/180)
         dtemp3=dtemp2*cos((xlon1-plon1)*pi/180)
         dist_degree=(180./pi)*acos(dtemp1+dtemp3)    

         end 






 

