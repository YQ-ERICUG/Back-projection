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

###  Click SAC image window, if tap 'A or a' in front of 'F or f', the file will not be deleted, and if tap 'A or a' 
###  behind 'F or f', the file will be deleted

###  Data downloaded from IRIS(http://ds.iris.edu/ds/) often has the format that station name is the 7th part if 
###  dot(.) as delimiter to separate SAC format's name. Look out data's name, and modify 'subroutine netsta_modified'
###  in file 'Pick_repeat.f' if needed.   

gfortran -o Pick_repeat.exe Pick_repeat.f /usr/local/sac/lib/sacio.a 
./Pick_repeat.exe < input_to_Pick_repeat.txt   
                 

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

ls *.SAC > stations1.txt 
set nline1 = ` wc -l stations1.txt | awk '{print $1}' ` 
set tmp    = ` echo $nline-$nline1 | bc -l`
echo 'Remove '$tmp' stations, and now there are '$nline1' stations' 

rm stations* pick.csh *.txt
