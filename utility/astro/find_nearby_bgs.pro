;+
; NAME:  
;
;    FIND_NEARBY_BGS
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Parses Spitzer observing logs for potential suitable background
;    observations, close in time and ecliptic latitude (which is the
;    primary variable affecting the zodiacal emission which dominates
;    at short Spitzer wavelengths).
;    
; CALLING SEQUENCE:
;
;    find_nearby_bgs, targ_coords,targ_date,files,[RECORDS=,DELTA_DAYS=, $
;                     DELTA_ECLIPTIC_LATITUDE=]
;
; INPUT PARAMETERS:
;
;    targ_coords: The coordinates of the target for which you'd like
;       potential background observations, in a string form GET_COORDS
;       can understand.
;
;    targ_date: The date/time of the observation, as a string in the
;       form included in the Spitzer observing logs (e.g. '2005-08-10
;       18:05:53.6').
;
;    files: A vector list of filenames of observing logs to search.
;       See http://ssc.spitzer.caltech.edu/approvdprog/sched for the
;       logs.  Use the individual log files, not the large
;       concatenated version.
;			
; INPUT KEYWORD PARAMETERS:
;
;    RECORDS: Input/output, the record data read in from 'files'.  If
;       passed in, the files are not read.  Otherwise, on output, the
;       variable contains the set of records read from files.
;
;    DELTA_DAYS: The range of days to consider for potential
;       observations.  Default=2 days.
;                    
;    DELTA_ECLIPTIC_LATITUDE: The range of ecliptic latitude deltas
;       from the target to consider for potential observations.
;       Default=10 degrees.
;			
; OUTPUT KEYWORD PARAMETERS:
;
;    OUTKEYWORD: Describe the output keywords.  Don't forget to [M-q]
;       to clean up the description text. May be unnecessary.
;			
; SIDE EFFECTS:
;
;    Potential suitable background observations are printed.
;
; PROCEDURES:
;
;    The NasaLib library.
;
; NOTES:
;  
;    Observing log files available at:
;
;       http://ssc.spitzer.caltech.edu/approvdprog/sched.
;
;    Individual IRS modules, and exposure times are not listed for
;    observations in these files, so Spot/Leopard must be consulted to
;    find matching IRS module/observing time and to check if the data
;    are available (i.e. if the proprietary period has elapsed).  Also
;    be sure to check that observations consititute real background
;    data, and are not contaminated by source.  The "off" observations
;    in point-source staring mode AORs are often useful.  Beware of
;    spectral maps of extended sources (or at least visualize them
;    using Spot over 2MASS/DSS/IRAC images/etc.).
;                    
;    It can be helpful to blend together a bracketing pair of
;    observations above and below in ecliptic latitude.  In SL, the
;    16um peak-up field can be used to set the blend fractions, to
;    null it out in the target data.
;
;
; EXAMPLE:
;
;    find_nearby_bgs,'12:50:53.10   41: 7:13.00','2004-06-25 07:04:03.4', $
;                    ['week030.txt','week031.txt','week032.txt']
;
; MODIFICATION HISTORY:
;
;    2005-07-02 (J.D. Smith): Written
;-
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

pro find_nearby_bgs,targ_coords,targ_date,files,RECORDS=recs,DELTA_DAYS=dd, $
                    DELTA_ECLIPTIC_LATITUDE=del
  if n_elements(dd) eq 0 then dd=2.
  if n_elements(del) eq 0 then del=10.
  
  if n_elements(recs) eq 0 then begin 
     for i=0,n_elements(files)-1 do begin 
        readfmt,files[i], SKIPLINE=6, $
                'A12,1X,A11,2X,A12,2X,A8,1X,A8,1X,I5,' + $
                '1X,A9,1X,F7,2X,A21,1X,I12', $
                targ,ra,dec,pi,progname,pid,aot,min_dur,date,aor
        irs=where(strpos(aot,'irs') ge 0,n)
        for j=0,n-1 do begin 
           k=irs[j]
           if n_elements(strsplit(ra[k],':')) ne 3 then begin 
              print,'Skipping: ',targ[k],' ',ra[k],' ',dec[k]
              continue
           endif 
           get_coords,INSTRING=ra[k]+','+dec[k],c
           euler,c[0]*15.D,c[1],l,b,3
           s=strsplit(date[k],/EXTRACT,COUNT=cnt)
           if cnt ne 2 then begin 
              print,'Skipping: ',targ[k],' ',ra[k],' ',dec[k]
              continue
           endif 
           d=strsplit(s[0],'-',/EXTRACT)
           t=strsplit(s[1],':',/EXTRACT)
           jul=julday(d[1],d[2],d[0],t[0],t[1],t[2])
           
           rec={TARGET:targ[k],COORDS:c,ECLIPTIC:[l,b],PI:pi[k], $
                PROGRAM:progname[k],PID:pid[k],AOT:aot[k], $
                DURATION:min_dur[k],DATE:jul,AOR:aor[k]}
           if n_elements(recs) eq 0 then recs=[rec] else recs=[recs,rec]
        endfor 
     endfor 
  endif 
  if n_elements(recs) eq 0 then message,'No records found'
  get_coords,INSTRING=targ_coords,tc
  euler,tc[0]*15.D,tc[1],tl,tb,3
  print,FORMAT='("Target Eclipt: (",F0.3,", ",F0.3,")")',tl,tb
  s=strsplit(targ_date,/EXTRACT)
  d=strsplit(s[0],'-',/EXTRACT)
  t=strsplit(s[1],':',/EXTRACT)
  targ_jul=julday(d[1],d[2],d[0],t[0],t[1],t[2])
  
  keep=where(abs(recs.date-targ_jul) le dd,nkeep)
  if nkeep eq 0 then begin 
     message,'No records match the date requirement.',/INFORMATIONAL
     return
  endif 
  
  good=recs[keep]
  
  keep=where(abs(good.ecliptic[1,*]-tb) le del,nkeep)
  if nkeep eq 0 then begin 
     message,'No records match the date and ecliptic latitude requirement.', $
             /INFORMATIONAL
     return
  endif 
  good=good[keep]
  
  s=sort(abs(good.ecliptic[1,*]-tb))
  for i=0,nkeep-1 do begin 
     rec=good[s[i]]
     print,FORMAT='(%"TARG: %s PID: %5d, AOR: %8d PI: %s, AOT: %s")', $
           rec.target,rec.pid,rec.aor,rec.pi,rec.aot
           
     print,FORMAT='(%"  ECLIPT: %6.2f,%6.2f (delta: %6.2f,%6.2f) ' + $
           'Offset: %0.2f days")', rec.ecliptic,rec.ecliptic-[tl,tb], $
           rec.date-targ_jul
     print,''
  endfor 
end
