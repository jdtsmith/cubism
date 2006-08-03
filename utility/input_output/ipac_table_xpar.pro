;+
; NAME:  
;
;    IPAC_TABLE_XPAR
;
; DESCRIPTION:
;    
;    Extract header parameters from an IPAC table header.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    val=ipac_table_xpar(hdr, name,[COUNT=])
;
; INPUT PARAMETERS:
;
;    hdr: The IPAC table header, as returned from READ_IPAC_TABLE.
;			
; INPUT KEYWORD PARAMETERS:
;
;    COUNT: The number of matching header cards.
;			
; OUTPUTS:
;
;    val: The header parameter value.
;
; NOTES:
;  
;    See http://irsa.ipac.caltech.edu/applications/DDGEN/Doc/ipac_tbl.html
;    for format description.
;
; EXAMPLE:
;
;    foo=read_ipac_table(file,hdr)
;    val=ipac_table_sxpar(hdr,'PARAM1')
;
; MODIFICATION HISTORY:
;
;    2006-06-13 (J.D. Smith): Written.
;-
;    $Id$
;
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2006 J.D. Smith
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

function ipac_table_xpar,hdr,name,COUNT=cnt
  if n_params() lt 2 then message,'Usage: IPAC_TABLE_SXPAR, header, name'
  cnt=0
  for i=0,n_elements(hdr)-1 do begin 
     val=stregex(hdr[i],'^\\'+strtrim(name,2)+' *= *("([^"]+)"|'+"'([^'])+'"+ $
                 '|([-0-9.]+))',/SUBEXPR,/EXTRACT)
     if val[0] then begin 
        val=val[2]?val[2]:(val[3]?val[3]:(val[4]?val[4]:-1))
        cnt++
        return,val
     endif 
  endfor 
  return,-1
end
