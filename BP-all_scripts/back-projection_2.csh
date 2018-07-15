#!/bin/csh -f

set cl            = 0.5      # minimum of high frequency band
set ch            = 1        # maximum of high frequency band
set nsam          = 40       # sampling; 1/delta
set P_length      = 600      # length of P wave

set window_length_L = 40       # length of long window for cross correlation
set lag_lenth_L     = 5        # lenth of time shift
set cc_min_L        = 0.02     # minimum of cross-correlation to eliminate bad data

set window_length = 10       # length of window for cross correlation
set lag_lenth     = 10       # lenth of time shift
set cc_min        = 0.4      # minimum of cross-correlation to eliminate bad data

set wlen          = 10       # length of window for stacking windows
set wdif          = 2.0      # interval of stacking windows
set nwin          = 100      # number of stacking windows


#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

cp BP_model_para.txt BP_para.txt

echo $cl $ch $nsam $P_length >> BP_para.txt
echo $window_length_L $lag_lenth_L $cc_min_L >> BP_para.txt
echo $window_length   $lag_lenth   $cc_min >> BP_para.txt
echo $wlen $wdif $nwin >> BP_para.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


###  Next we use Fortran script to invoke SAC(http://ds.iris.edu/files/sac-manual/)

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals

###  Using the SAC I/O library, ${SACHOME}/lib/libsacio.a, one can write stand-alone codes in C or FORTRAN to read 
###  and write SAC formatted data files. Here '/usr/local/sac/lib/sacio.a' is the path of libsacio.a in my computer

gfortran -o stack_Tshift_raw-EU.exe stack_Tshift_raw-EU.f /usr/local/sac/lib/sacio.a
./stack_Tshift_raw-EU.exe < input_station_names.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _




