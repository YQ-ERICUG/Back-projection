      parameter(nsta=1000,nnpt=200000,npt=200000, nmax=200000)

      dimension seis(nsta,npt), seis2(nsta,npt), seis3(nsta,npt)
      dimension tt(100000), tdiff(nsta),stlat(nsta),stlon(nsta),
     &stlon_hi(nsta),stlat_hi(nsta)
      dimension x(npt),tot(npt),xm(npt),x2(npt),yarray(npt)
      dimension xx(nnpt)
      dimension glat(9000), glon(9000)
      dimension stacks(9000,200)
      double precision dtemp1,dtemp2,dtemp3,dtemp4,lon_max,lon_min,
     &lat_max,lat_min

      character*80 file1,file2(nsta),file22,fin
 
      character*4 stanm(nsta),csta,net
      character*8 filter_indicator,file3
      integer  ista_tag(nsta),nzmin(nsta),nzsec(nsta),
     &nzmsec(nsta),nzhour(nsta)

      dimension dist(nsta)


 900  format(a72)
 901  format(a72)


c for all stations
      lon_max = -1000
      lon_min =  1000
      lat_max = -1000
      lat_min =  1000


 

       write(*,*)'All File names listed in the stations.txt'
       read(*,900)file1
 
       open(33,file=file1, status='old')

       read(33,*) nsta2
       do j=1,nsta2
         read(33,901) file2(j)
         call rsac1(file2(j),xx,ndata,beg,dt,nmax,nerr)
         call getfhv('STLA',stlat(j),nerr)
         call getfhv('STLO',stlon(j),nerr)

            if (lon_max .lt. stlon(j)) lon_max=stlon(j)  
            if (lon_min .gt. stlon(j)) lon_min=stlon(j)  
            if (lat_max .lt. stlat(j)) lat_max=stlat(j)  
            if (lat_min .gt. stlat(j)) lat_min=stlat(j)   
 
       end do
       close(33)  

c   calculate the array center
       open(31,file='mod_station.txt')
       dtmp=10000
       ktmp=0
       cen_lon=(lon_max+lon_min)/2.0
       cen_lat=(lat_max+lat_min)/2.0
       do j=1,nsta2
         call dist_calculation(stlat(j),stlon(j)
     &,cen_lat,cen_lon,temp)
         if (temp .lt. dtmp) then
           dtmp=temp
           ktmp=j
         end if
       end do
       write(*,'(a50, 4f11.5)')
     & file2(ktmp),stlon(ktmp),stlat(ktmp), cen_lon,cen_lat
       write(31,'(a50, 4f11.5)')
     & file2(ktmp),stlon(ktmp),stlat(ktmp), cen_lon,cen_lat
       close(31)

       end   




                      

c  subroutines      
 

      subroutine dist_calculation(xlat1,xlon1,plat1,plon1,dist_degree)

         pi=3.1415926
         dtemp1=cos((90-plat1)*pi/180)
     &*cos((90-xlat1)*pi/180)
         dtemp2=sin((90-plat1)*pi/180)*sin((90-xlat1)*pi/180)
         dtemp3=dtemp2*cos((xlon1-plon1)*pi/180)
         dist_degree=(180./pi)*acos(dtemp1+dtemp3)    

         end 






 

