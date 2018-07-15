      parameter(nsta=800,nnpt=1000000,npt=50000, nmax=50000)
      dimension seis(nsta,npt), seis2(nsta,npt), seis3(nsta,npt)
      dimension tt(100000), tdiff(nsta),stlat(nsta),stlon(nsta)
      dimension x(npt),tot(npt),xm(npt),x2(npt),yarray(npt)
      dimension xx(nnpt)
      dimension glat(5000), glon(5000)
      dimension stacks(5000,200)
      dimension dist(nsta)
      double precision dtemp1,dtemp2,dtemp3,dtemp4
      character*80 file1,file2(nsta),file22,fin
      character*4 stanm(nsta),csta,net
      character*8 filter_indicator,file3
      integer  ista_tag(nsta),nzmin(nsta),nzsec(nsta),
     &nzmsec(nsta),nzhour(nsta)



 900  format(a72)
 901  format(a72)

      write(*,*)'All File names listed in the stations.txt' 
      read(*,900)file1
      open(33,file=file1, status='old') !_______________________________________________33
      read(33,*)nsta2
      do ista=1,nsta2                     !___________________________________________do
         read(33,901)file2(ista)
         call rsac1(file2(ista),xx,ndata,beg,dt,nmax,nerr) !kname, yarray, nlen, beg, del, MAX, nerr

c check and change sampling 
	 if (abs(dt-0.025) .gt. 0.00001) then         !dt:delta
           open(20,file='temp.csh')
            n=len_trim(file2(ista))
           if(abs(dt-0.02) .lt. 0.00001) then        !delta is 0.02_______
            write(20,*) 'echo bd sgf > sac.com'          
            write(20,*) 'echo qdp off >> sac.com'
            write(20,*) 'echo r '(1:7),file2(ista)(1:n),
     &' >> sac.com'(1:11)
            write(20,*) 'echo stretch 4 >> sac.com'   !Stretches (upsamples) data       
            write(20,*) 'echo decimate 5 >> sac.com'  !Decimates (downsamples) data 
            write(20,*) 'echo w over >> sac.com'      !overwrite headers and data
            write(20,*) 'echo quit >> sac.com' 
            write(20,*) 'sac < sac.com'
           close(20)
           call system('csh temp.csh')
           else if(abs(dt-0.05) .lt. 0.00001) then   !delta is 0.05_______
            write(20,*) 'echo bd sgf > sac.com'
            write(20,*) 'echo qdp off >> sac.com'
            write(20,*) 'echo r '(1:7),file2(ista)(1:n),
     &' >> sac.com'(1:11)
            write(20,*) 'echo stretch 2 >> sac.com'   !Stretches (upsamples) data   
            write(20,*) 'echo w over >> sac.com'   
            write(20,*) 'echo quit >> sac.com' 
            write(20,*) 'sac < sac.com'
           close(20)
c           call system('csh temp.csh')
c           else if(abs(dt-0.04) .lt. 0.00001) then   !delta is 0.04_______
c            write(20,*) 'echo bd sgf > sac.com'
c            write(20,*) 'echo qdp off >> sac.com'
c            write(20,*) 'echo r '(1:7),file2(ista)(1:n),
c     &' >> sac.com'(1:11)
c            write(20,*) 'echo stretch 4 >> sac.com' 
c            write(20,*) 'echo decimate 5 >> sac.com'      
c            write(20,*) 'echo w over >> sac.com'   
c            write(20,*) 'echo quit >> sac.com' 
c            write(20,*) 'sac < sac.com'
c           close(20)
c           call system('csh temp.csh')
c           else if(abs(dt-0.025) .lt. 0.00001) then  !delta is 0.025_______
c            write(20,*) 'echo bd sgf > sac.com'
c            write(20,*) 'echo qdp off >> sac.com'
c            write(20,*) 'echo r '(1:7),file2(ista)(1:n),
c     &' >> sac.com'(1:11)
c            write(20,*) 'echo decimate 2 >> sac.com'    
c            write(20,*) 'echo w over >> sac.com'   
c            write(20,*) 'echo quit >> sac.com' 
c            write(20,*) 'sac < sac.com'
c           close(20)
c           call system('csh temp.csh')
c           else if(abs(dt-0.0125) .lt. 0.00001) then  !delta is 0.0125_______
c            write(20,*) 'echo bd sgf > sac.com'
c            write(20,*) 'echo qdp off >> sac.com'
c            write(20,*) 'echo r '(1:7),file2(ista)(1:n),
c     &' >> sac.com'(1:11)
c            write(20,*) 'echo decimate 4 >> sac.com'    
c            write(20,*) 'echo w over >> sac.com'   
c            write(20,*) 'echo quit >> sac.com' 
c            write(20,*) 'sac < sac.com'
c           close(20)


           call system('csh temp.csh')
          else 
c             write(*,*)  file2(ista),dt
            n=len_trim(file2(ista))
            write(fin,*) 'rm '(1:3),file2(ista)(1:n)
            write(*,*) fin
            close(20)
            call system(fin)             
          end if                   
         end if
      end do                                 !______________________________end do
      close(33)			      !____________________________close 33
      end

