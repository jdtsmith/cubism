;+
; NAME:  
;
;    irs_recent_calib
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.sirtf.edu/cubism
;
; DESCRIPTION:
;    
;    Identifies the most recent calibration object.
;    
; CATEGORY:
;
;    IRS Calibration
;    	
; CALLING SEQUENCE:
;
;    recent_cal_file=irs_recent_calib([/PRE_LL])
;
; INPUT KEYWORD PARAMETERS:
;
;    PRE_FTBOTH: Look for the most recent with "preFTBoth" in the
;       name, indicating a relevant frametable before 2004-07-13, when
;       the LLBoth (and SLBoth) frame table was updated.  If not set,
;       calibration files containing preFTBoth will be excluded.
;
; COMMON BLOCKS:
;
;    IRS_DIR: For locating the IRS calibration file directories.
;
; MODIFICATION HISTORY:
;
;    2004-10-28 (J.D. Smith): Added support for "preFTboth" cals.
;    2002-11-15 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001-2004 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################
function irs_recent_calib,PRE_FTBOTH=pre_FTBOTH
  @cubism_dir
  if keyword_set(pre_FTBOTH) then filt='*preFTboth*.cal' else filt='*.cal'
  files=file_search(filepath(ROOT=irs_calib_dir,SUBDIR="sets",filt))
  
  if ~keyword_set(pre_FTBOTH) then begin 
     wh=where(stregex(files,'preFTboth.*\.cal',/BOOLEAN),cnt, $
              COMPLEMENT=good,NCOMPLEMENT=ngood)
     if ngood eq 0 then message,'No matching calibration sets found.'
     files=files[good]
  endif 
  dates=lon64arr(2,n_elements(files))
  for i=0,n_elements(files)-1 do begin 
     openr,un,/get_lun,files[i]
     dates[0,i]=(fstat(un)).mtime
     free_lun,un
     date=stregex(/EXTRACT,/SUBEXPR,filestrip(files[i]), $
                  '([0-9]{4})[-_]([0-9]{2})[-_]([0-9]{2})')
     if n_elements(date) ne 4 OR strlen(date[1]) eq 0 then continue
     dates[1,i]=julday(date[2],date[3],date[1])
  endfor 
  if array_equal(dates[1,*] ne 0,1b) then s=sort(dates[1,*]) else $
     s=sort(dates[0,*])
  return,files[s[n_elements(s)-1]]
end
