;+
; NAME:
;
;    JUL2DATE
;       
; DESCRIPTION:
;
;    Convert a julian day value to a formatted date string.
;
; CATEGORY:
;
;    Date Utility
;
; CALLING SEQUENCE:
; 
;    date=jul2date(jul,[D_T=]
;   
; INPUTS:
; 
;    jul: The julian day value, as a double (e.g. systime(/JULIAN).
;	
; INPUT KEYWORD PARAMETERS:
;
;    D_T: Format as "DATE*TIME" instead of the default.
;	  
; OUTPUTS:
; 
;    date: The formatted date
;       
; MODIFICATION HISTORY:
;
;    2006-01-02 (J.D. Smith): Use new date formatting codes.
;    2002-12-14 (J.D. Smith): Initial import from SCORE-era sources.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2002, 2003, 2006 J.D. Smith
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

function jul2date,jul,D_T=d_t
  case 1 of
     keyword_set(d_t) : begin 
        form='(C(CMOI2.2,"/",CDI2.2,"/",CYI2.2,"*",' + $
             'CHI2.2,":",CMI2.2,":",CSI2.2))'
        return,string(format=form,jul)
     end
     else: return,string(FORMAT='(C())',jul)
  endcase
end
