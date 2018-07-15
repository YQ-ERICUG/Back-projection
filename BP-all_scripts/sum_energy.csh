#!/bin/csh -f

set nwin          = 100      # number of stacking windows
set ntimch        = 100      # step number of the BP with rupture direction changes

echo $nwin $ntimch > mkcontour.txt

###  gfortran(https://gcc.gnu.org/wiki/GFortran) isn't the only editor, you can try other editors to achievie goals

gfortran -o mkcontour_max_smo-multi-V2.0.exe mkcontour_max_smo-multi-V2.0.f
./mkcontour_max_smo-multi-V2.0.exe

sort -k 4 -n  mkrup_sm_CBP.out_calibrated  > result
mv result mkrup_sm_CBP.out_calibrated



