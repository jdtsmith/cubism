;+
; NAME:
;
;    DATE2JUL
;       
; DESCRIPTION:
;
;    Convert a formatted date string to Julian day
;
; CATEGORY:
;
;    Date Utility
;
; CALLING SEQUENCE:
; 
;    jul=date2jul(date)
;   
; INPUTS:
; 
;    date: The date as a standard formatted date/time string (e.g as
;      returned by jul2date).
;	
; OUTPUTS:
; 
;    jul: The julian day, as a double.
;       
; MODIFICATION HISTORY:
;
;    2007-01-16 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2007 J.D. Smith
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

function date2jul,date
  jul=dblarr(n_elements(date),/NOZERO)
  for i=0,n_elements(date)-1 do begin 
     b=bin_date(date[i])
     jul[i]=julday(b[1],b[2],b[0],b[3],b[4],b[5])
  endfor 
  return,jul
end
