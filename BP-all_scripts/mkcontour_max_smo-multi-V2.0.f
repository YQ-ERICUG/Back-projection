c1234&
      parameter(nbp=10, npt=20000, nmax=500000 )
      character*10 ctemp
      character*80 ss,filename(nbp),fin
      real  flat(nbp,300,npt), flon(nbp,300,npt),
     &amp(nbp,300,npt),sm_amp(nbp,300,npt),
     &maxamp(nbp,300),maxlat(nbp,300),maxlon(nbp,300),maximum(nbp),
     &temp1(300),temp2(300),energy(npt)
 
 900  format(a10)


c epicenter & array center information

      open(41,file='BP_model_para.txt', status='old')
        read(41,*) evlo,evla
        read(41,*) pstart,imod,xlon1,xlat1
        read(41,*) tomhh,tommm,tomss,tomms
      close(31)

      elon = evlo
      elat = evla
      elat2= elat
      elon2= elon 


      arrlon=xlon1
      arrlat=xlat1

c grid information

      open(20,file='subs.dat')
        read(20,*) nsub
      close(20)

c  set parameters

      open(21,file='mkcontour.txt')
        read(21,*)nwin,ntimch 
      close(21)


c read *-stack.out files 

      call system('ls stack.out > stackoutfiles')
      open (10, file='stackoutfiles')
          nth_BP = 0
          do
            read(10,'(a80)',iostat=iostat) ss; 
            if (iostat.ne.0) exit
            nth_BP = nth_BP + 1
	    filename(nth_BP) = ss
          end do
      close(10)


c smooth the time series    
      do ith_BP=1,nth_BP
        write(fin,'(a80)') filename(ith_BP)
        write(*,*) filename(ith_BP)
        open(9,file=fin)


c      nsub = 2250
c      smooth=2.99             !2.99 for 3 windows; 4.99 for 5 windows;8000 for no smoothing
      smooth=0


      do j=1,nwin
 
        read(9,*)temp1(j), temp2(j)

        do i = 1,nsub
         read(9,*)flat(ith_BP,j,i), flon(ith_BP,j,i), amp(ith_BP,j,i)
c         amp = amp/100.
c         amp1(i) = amp**2/fmax	 !using absolute beam power
c         write(k,*)flat(i), flon(i), 10*log(amp1(i))  !using beam power DB

         amp(ith_BP,j,i)=amp(ith_BP,j,i)**2
        end do          
        read(9,900)ctemp
      end do

c normalize the amplitudes with previous window stacking amplitudes

         if (ith_BP .gt. 1) then
           temp_maxamp=0
           temp_maxamp2=0
           do j=60*3+1,60*4
             do i=1,nsub
              if (amp(ith_BP-1,j,i) .gt.temp_maxamp ) then 
               temp_maxamp= amp(ith_BP-1,j,i)
              end if
             end do
           end do
           
           do j=1,60*1
             do i=1,nsub
              if (amp(ith_BP,j,i) .gt.temp_maxamp2 ) then
               temp_maxamp2= amp(ith_BP,j,i)
              end if
             end do
           end do
           
           do j=1,nwin
             do i=1,nsub
               amp(ith_BP,j,i)= 
     & temp_maxamp*amp(ith_BP,j,i)/(temp_maxamp2*1.0)
             end do
           end do
         end if
         
c  smooth the figures

        do j=1,nwin
          do i=1,nsub

            sm_amp(ith_BP,j,i)=amp(ith_BP,j,i)
            if (j .gt. int(smooth/2) .and. j .lt. nwin-int(smooth/2))
     &then
              temp3=0.
              do jj=j-int(smooth/2),j+int(smooth/2)                !using 6 windows to smooth
c               temp3=temp3+amp(ith_BP,jj,i)/(2**abs(j-jj))
               temp3=temp3+amp(ith_BP,jj,i)
              end do
              sm_amp(ith_BP,j,i)=temp3
             end if
           end do
         end do
          
c     calculate the maximun amplitude point

         do j=1,nwin
          maxamp(ith_BP,j)=0
         end do

         maximum(ith_BP)=0
         do j=1,nwin
           do i=1,nsub
             if (sm_amp(ith_BP,j,i) .gt. maxamp(ith_BP,j)) then
               maxamp(ith_BP,j)= sm_amp(ith_BP,j,i)
               maxlat(ith_BP,j)=flat(ith_BP,j,i)
               maxlon(ith_BP,j)=flon(ith_BP,j,i)              
             end if
           end do           

           if(maxamp(ith_BP,j) .gt. maximum(ith_BP)) then
             maximum(ith_BP) = maxamp(ith_BP,j)
           end if             

         end do   
       end do 



       open(11,file='mkrup_sm_CBP.out_calibrated')


c       open(12,file='maximum_points.out')
       do ith=1,nth_BP
cc         do j=1,180
         do j=1,nwin
c           do i=1,nsub
c            write(12,*) sm_amp(ith_BP,jj,i)
c           end do
          if(j .le. ntimch) then           
            dist = sqrt(
     &(maxlat(ith,j)-elat)**2+((maxlon(ith,j)-elon)*
     &cos((maxlat(ith,j)+elat)*3.1415926/(2*180.)))**2)*111.195
            
            if ( dist .lt. 0.001 ) then
               dt = 0
            else
              call station_corr(arrlon,arrlat,elon,elat,
     &maxlon(ith,j),maxlat(ith,j),dt)
            end if
            if(j .eq. ntimch) tdist=dist
          else
            dist = sqrt(
     &(maxlat(ith,j)-elat2)**2+((maxlon(ith,j)-elon2)*
     &cos((maxlat(ith,j)+elat2)*3.1415926/(2*180.)))**2)*111.195
c            dist=tdist+dist
            dist=50+dist
c            write(*,*) tdist,dist,'okok'
            call station_corr(arrlon,arrlat,elon,elat,
     &maxlon(ith,j),maxlat(ith,j),dt)
          endif

            write(11,*) maxlon(ith,j),maxlat(ith,j),
     &maxamp(ith,j)*1.0/maximum(ith),(temp1(j)+temp2(j))/2.-dt,dist
         end do
       end do
       close(11)

c  calculate the overall energy


      open(12,file='all_energy.out')
       do i=1,nsub
         energy(i)=0
       end do


       do i=1,nsub
         do ith=1,nth_BP
           do j=1,nwin
            energy(i)=energy(i)+ sm_amp(ith,j,i)*1.0/maximum(ith)
           end do   
         end do
       end do


       temp=0
       do i=1,nsub
         if (energy(i) .gt. temp) temp=energy(i)
       end do
       
       do i=1,nsub   !!!!!!!!!!Be careful, all back projections are used the same grid files here
         write(12,*)  flon(1,1,i),flat(1,1,i),energy(i)*1.0/temp
       end do
      close(12)

      
       end

C ----------------------------------------------------------------------

       subroutine dist_calculation(xlat1,xlon1,plat1,plon1,dist_degree)

         pi=3.1415926
         dtemp1=cos((90-plat1)*pi/180)
     &*cos((90-xlat1)*pi/180)
         dtemp2=sin((90-plat1)*pi/180)*sin((90-xlat1)*pi/180)
         dtemp3=dtemp2*cos((xlon1-plon1)*pi/180)
         dist_degree=(180./pi)*acos(dtemp1+dtemp3)    

         end 

      subroutine azimuth_cal(eplon1,eplat1,stlon1,stlat1,angle_degree)
          pi=3.1415926    
	    
          eplon=eplon1*pi/180.    
	  eplat=eplat1*pi/180.  
	  stlon=stlon1*pi/180.
	  stlat=stlat1*pi/180.
	  
      	  e1 = cos(eplon)*cos(eplat)
	  e2 = sin(eplon)*cos(eplat)
	  e3 = sin(eplat)
	  
	  s1 = cos(stlon)*cos(stlat)
	  s2 = sin(stlon)*cos(stlat)
	  s3 = sin(stlat) 
	  
	  temp = e1*s1+e2*s2+e3*s3
          angle= atan2(cos(stlat)*cos(eplat)*sin(stlon-eplon)
     &,sin(stlat)-temp*sin(eplat))
          angle_degree=angle*180/pi
      end

      subroutine station_corr(arrlon,arrlat,epilon,epilat,
     &stoplon,stoplat,dt)
       real slowlist(300)  
       pi=3.1415926    

c        arrlon=138    ! Longitude and latitude of the array
c        arrlat=36
c      epilon,epilat   !Longitude and latitude of the epicenter
c      stoplon,stoplat !location of the last pulse
       open(20,file='slowtable.dat')
       read(20,*) nslow
       read(20,*) tmp
       do i=1,nslow-1
        read(20,*) slowlist(i)
       end do
       close(20)
      
      call azimuth_cal(epilon,epilat,stoplon,stoplat,angle_degree)  
           rupdir=angle_degree  
	          
c      write(*,*) 'Rupture direction in degree?'
c      write(*,*) rupdir
      
c      write(*,*) 'Rupture Length in km?'
      call dist_calculation(epilat,epilon,stoplat,stoplon,dist_degree)
      ruplen=dist_degree*111.19      
c      write(*,*) 'Rupture Length',ruplen
      
      call azimuth_cal(epilon,epilat,arrlon,arrlat,angle_degree)
      call dist_calculation(epilat,epilon,arrlat,arrlon,dist_degree)
c      write(*,*) '----,angle_degree,rupdir,----'
c      write(*,*) angle_degree,rupdir
c      write(*,*) epilon,epilat,arrlon,arrlat,stoplon,stoplat
            
c      write(*,*) 'Appartent slowness at a distance of ',dist_degree
      i=dist_degree
      slowness=slowlist(i)
      
      dt=-ruplen*cos((angle_degree-rupdir)*pi/180.)*slowness/111.19

      end
