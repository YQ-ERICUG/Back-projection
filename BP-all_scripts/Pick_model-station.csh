#!/bin/csh -f

ls *.SAC > stations.txt                                # list all SAC files'name into stations.txt 
set nline = ` wc -l stations.txt | awk '{print $1}' `  # count stations' number (nline) 
sed '1i '$nline'' stations.txt > station_names         # insert nline into the first row in stations.txt 
mv station_names stations.txt 
ls stations.txt > input_station_names.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

###  Next we use Fortran script to invoke SAC(http://ds.iris.edu/files/sac-manual/)

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals

###  Using the SAC I/O library, ${SACHOME}/lib/libsacio.a, one can write stand-alone codes in C or FORTRAN to read 
###  and write SAC formatted data files. Here '/usr/local/sac/lib/sacio.a' is the path of libsacio.a in my computer 

gfortran -o Pick_sub-array_station_lat-layer.exe Pick_sub-array_station_lat-layer.f /usr/local/sac/lib/sacio.a
./Pick_sub-array_station_lat-layer.exe < input_station_names.txt


###  Information of model station's name and location has been saved in 'mod_station.txt'

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

###  Here, we want to set some parameters for next program. (1) earthquake's location; (2) model station's location;
###  (3) model station's order in file 'stations.txt'. (4) P wave arrival time of model station; (5) Evevt's reference
###  time in model station.  

set mod = `awk '{print $1}' mod_station.txt `

set evlo = ` saclst evlo f $mod | awk '{print $2}' `              # (1) earthquake's longnitude
set evla = ` saclst evla f $mod | awk '{print $2}' `              # (1) earthquake's latitude

set mod_stlo  = ` awk '{print $2}' mod_station.txt `              # (2) model station's longnitude
set mod_stla  = ` awk '{print $3}' mod_station.txt `              # (2) model station's latitude

set imod      = ` grep -n ''$mod'' stations.txt | cut -d: -f 1 `
set order_mod = ` echo ''$imod'-1' | bc -l `                      # (3) model station's order

###  Using PPK to pick P phases using the graphics cursor (tap 'A' or 'a' at the P arrival time)

echo "r $mod"  > sac.com
echo "qdp off" >> sac.com
echo "bp co 0.5 2 p 2" >> sac.com
echo "rmean" >> sac.com
echo "rtrend" >> sac.com
echo "taper" >> sac.com
echo "ppk" >> sac.com       
echo "wh" >> sac.com
echo "quit" >> sac.com
sac < sac.com > /dev/null
set ptime = `saclst A f $mod | awk '{printf "%d", $2}'`           # (4) P wave arrival time 
saclst nzhour nzmin nzsec nzmsec f $mod | awk '{print $2,$3,$4,$5}'> mod_time.txt  # (5) Reference time

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

### Save above information in 'BP_model_para.txt'

echo $evlo $evla > BP_model_para.txt
echo $ptime $order_mod $mod_stlo $mod_stla >> BP_model_para.txt
cat mod_time.txt >> BP_model_para.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

rm mod_station.txt mod_time.txt sac.com 
### Don't deleate 'stations.txt' and 'BP_model_para.txt'.



