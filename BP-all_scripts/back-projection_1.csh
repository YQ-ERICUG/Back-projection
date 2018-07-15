#!/bin/csh -f

set cl            = 0.05     # upper bound of low frequency band
set ch            = 1        # lower bound of low frequency band
set nsam          = 40       # sampling; 1/delta
set P_length      = 600      # length of waveform needed to be analysed

set window_length = 10       # length of window for cross correlation
set lag_lenth     = 5        # lenth of time shift
set cc_min        = 0.6      # minimum of cross-correlation to eliminate bad data

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

cp BP_model_para.txt BP_para.txt

echo $cl $ch $nsam $P_length >> BP_para.txt
echo $window_length $lag_lenth $cc_min >> BP_para.txt

###  Next we use Fortran script to invoke SAC(http://ds.iris.edu/files/sac-manual/)

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals

###  Using the SAC I/O library, ${SACHOME}/lib/libsacio.a, one can write stand-alone codes in C or FORTRAN to read 
###  and write SAC formatted data files. Here '/usr/local/sac/lib/sacio.a' is the path of libsacio.a in my computer

gfortran -o stack_raw-EU.exe stack_raw-EU.f /usr/local/sac/lib/sacio.a
./stack_raw-EU.exe < input_station_names.txt

#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _



