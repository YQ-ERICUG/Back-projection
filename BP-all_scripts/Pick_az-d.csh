#!/bin/csh -f

             
set a1 = 0                           # azimutuh's minimum 
set a2 = 180                         # azimutuh's maximum
set d1 = 30                          # epicentral distance's minimum
set d2 = 85                          # epicentral distance's maximum


foreach sacfile (*SAC)

 set d  = `saclst gcarc f $file | awk '{printf $2}'`
 set az = `saclst az    f $file | awk '{printf $2}'`


 set tmp = `echo $d $az $a1 $a2 $d1 $d2 | awk '{if (($1 >= $3 && $1 <= $4) && ($2 >= $5 && $2 <= $6)) print 1;else print -1}'`

 if ($tmp < 0 ) then

    cp sacfile ./Data_trash          # save files even not required to avoid accidentally deleting 
    rm sacfile                        

 endif


end
