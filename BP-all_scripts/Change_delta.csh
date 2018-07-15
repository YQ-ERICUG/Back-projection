#!/bin/csh -f


ls *.SAC > stations.txt                                # list all SAC files'name into stations.txt 
set nline = ` wc -l stations.txt | awk '{print $1}' `  # count stations' number (nline) 
sed '1i '$nline'' stations.txt > station_names         # insert nline into the first row in stations.txt 
mv station_names stations.txt 
ls stations.txt > input_to_Pick_repeat.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

###  Next we use Fortran script to invoke SAC(http://ds.iris.edu/files/sac-manual/)

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals

###  Using the SAC I/O library, ${SACHOME}/lib/libsacio.a, one can write stand-alone codes in C or FORTRAN to read 
###  and write SAC formatted data files. Here '/usr/local/sac/lib/sacio.a' is the path of libsacio.a in my computer 

gfortran -o Change_delta.exe Change_delta.f /usr/local/sac/lib/sacio.a
./Change_delta.exe < input_to_Pick_repeat.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

rm stations.txt temp.csh
