nt=62
nz=15

'open /ptmp/rvasic/ECMWF/may.ctl'
'set x 1 144'
'set y 1 73'
'set gxout fwrite'
'set fwrite tmpprs.gra'
it=1
while (it <= nt)
  'set t 'it
   iz=1
   while (iz <= nz)
     'set z 'iz
     'd tprs'
      iz=iz+1
   endwhile
it=it+1
endwhile
say " *** tmpprs.gra OK"
'disable fwrite'
'quit'
