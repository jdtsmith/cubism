;+
; NAME:  
;
;    RADECSTRING
;
; DESCRIPTION:
;    
;    Create a string from RA or DEC coordinates.
;    
; CATEGORY:
;
;    Astro Utility
;
; CALLING SEQUENCE:
;
;    st=radecstring(ra_dec,[RA=,PRECISION=,HMS=])
;
; INPUT PARAMETERS:
;
;    ra_dec: An ra or dec coordinate to format.
;    
; INPUT KEYWORD PARAMETERS:
;
;    RA: If set, format the coordinate as an RA, otherwise as a DEC.
;
;    PRECISION: Number of digits to appear after the decimal point,
;       one less for DEC than RA.
;
;    HMS: If set, format with hms or dms.
;
; OUTPUT:
;
;    st: The formatted string.
;    
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Import from SCORE-era sources.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2005 J.D. Smith
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
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;; Convert from decimal degrees to a print format
function radecstring,pos,RA=ra,PRECISION=pres,HMS=hms
  if n_elements(pres) eq 0 then pres=2 else pres=pres>2
  
  final_ra='%0'+strtrim(pres+3,2)+'.'+strtrim(pres,2)+'f'
  final_dec='%0'+strtrim(pres+2,2)+'.'+strtrim(pres-1,2)+'f'
  
  if keyword_set(ra) then begin 
     if keyword_set(hms) then form='(%"%2.2dh%2.2dm'+final_ra+'s")' $
     else form='(%"%2.2d:%2.2d:'+final_ra+'")'
     str=string(FORMAT=form,sixty(pos/15.0D))
  endif else begin 
     if keyword_set(hms) then form='(%"%1s%2.2dd%2.2dm'+final_dec+'s")' $
     else form='(%"%1s%2.2d:%2.2d:'+final_dec+'")'
     str=string(FORMAT=form, $
                pos lt 0.0D?"-":"+", abs(sixty(pos)))
  endelse 
  
  return,str
end
