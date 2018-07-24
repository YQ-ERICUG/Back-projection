      parameter(nsta=1800,nnpt=1000000,npt=50000, nmax=50000)

      dimension seis(nsta,npt), seis2(nsta,npt), seis3(nsta,npt)
      dimension tt(10000), tdiff(nsta),stlat(nsta),stlon(nsta)
      dimension x(npt),temp(npt),tot(npt),xm(npt),x2(npt),yarray(npt)
      dimension xx(nnpt)
      dimension glat(4600), glon(4600)
      dimension stacks(4600,200)
      double precision dtemp1,dtemp2,dtemp3,dtemp4

      character*80 file1,file2(nsta),file22
 
      character*5 stanm(nsta),csta,net
      character*8 filter_indicator,file3
      integer  ista_tag(nsta),nzmin(nsta),nzsec(nsta),nzmsec(nsta)
      dimension dist(nsta)

      character*80  fin


 900  format(a72)
 901  format(a72)
        
c      call system('rm -rf *.V')
 


c  read  data
   
c      write(*,*)' enter name of file with file names'
      write(*,*)'All File names listed in the stations.txt'
      read(*,900)file1
      open(33,file=file1, status='old')


      read(33,*)nsta2
      do ista=1,nsta2
         read(33,901)file2(ista)
         call rsac1(file2(ista),xx,ndata,beg,dt,nmax,nerr)
c         call getkhv('KSTNM',stanm(ista),nerr)
         CALL netsta_modified(file2(ista),net,stanm(ista))
         write(*,*)stanm(ista)
      end do
      close(33)
         
c    check repeating stations

      call  bubble_sort(stanm,file2,nsta2)
c      do i=1,nsta2
c        write(*,'(a5,a60)') stanm(i),file2(i)
c      end do

      njump=0
      do ista=1,nsta2-1
c      do ista=1,20
        write(*,*) stanm(ista),njump
        if(stanm(ista) .eq. stanm(ista+1) .and. njump.eq.0) then
         do i=ista+1,nsta2
           if(stanm(i) .ne. stanm(ista)) exit
         end do
         njump=i-1-ista
         write(*,*) ',,,,',njump
        open(20,file='pick.csh')
          write(20,*) 'echo bd sgf > sac.com'
          write(20,*) 'echo qdp off >> sac.com'
          do k=ista,i-1
           write(20,'(a12,a50,a11)') 'echo r more ',
     &file2(k),' >> sac.com'
          end do
          write(20,*) 'echo ppk >> sac.com'
          write(20,*) 'echo writehdr >> sac.com'
          write(20,*) 'echo exit >> sac.com'
          write(20,*) 'sac < sac.com > /dev/null'
         close(20)
         call system('csh pick.csh')
        end if
        if(stanm(ista) .eq. stanm(ista+1)) njump=njump-1
      end do
            
      do ista=1,nsta2
         call rsac1(file2(ista),xx,ndata,beg,dt,nmax,nerr)
	 call getfhv('A',apick,nerr)
c         if (nerr .ne. 0) then
c           write(*,*) file2(ista)  
c         end if 
	 call getfhv('F',fpick,nerr)
c         if (nerr .ne. 0) write(*,*) file2(ista)

	 if (apick .gt. fpick .and. nerr .eq. 0) then
          ntemp=len_trim(file2(ista))
          write(fin,*) 'rm '(1:3),file2(ista)(1:ntemp)
          write(*,*) fin
          call system(fin)
         end if
      end do
           

      end

                       

c  subroutines      

      subroutine bubble_sort(a,b,n)
       implicit none
       integer n
       character*5 a(n),temp
       character*80 b(n),temp2
       integer i,j
       do i=n-1,1,-1  
       do j=1,i       
        if ( a(j) .gt. a(j+1) ) then 
         temp=a(j)
         a(j)=a(j+1)
         a(j+1)=temp
         
         temp2=b(j)
         b(j)=b(j+1)
         b(j+1)=temp2

        end if
       end do
       end do
      end 


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


 

