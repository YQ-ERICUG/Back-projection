#!/bin/csh -f

###  Only six basic parameters are needed to set up. 
###  First, earthquake's location (longitude,latitude), whick is the base of all grid points.
###  Then you need set up the interval between points.
###  Last, they are number of grid ponits in four directions (along and perpendicula strike).
  

    set hypolon = 142.498      # epicenter's longitude
    set hypolat = 38.2963      # epicenter's latitude
    set az      = 23           # angle from north, the positive dir along strike ([Gloabla CMT](http://www.globalcmt.org/))

    set gridint = 10      # space interval between points             

    set nstrike1 = 20     # no. grid points in positive dir along strike 
    set nstrike2 = 30     # no. grid points in negative dir along strike 
    set nperp1   = 10     # no. grid points eastward to hypocenter (east)
    set nperp2   = 20     # no. grid points westward to hypocenter (west)



echo $hypolon $hypolat $az > BP_para.txt
echo $gridint >> BP_para.txt
echo $nstrike1 $nstrike2 $nperp1 $nperp2 >> BP_para.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

###  Next we use Fortran script to invoke SAC(http://ds.iris.edu/files/sac-manual/).

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals.

gfortran -o makegrid_RT.exe makegrid_RT.f 
./makegrid_RT.exe

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
rm BP_para.txt
