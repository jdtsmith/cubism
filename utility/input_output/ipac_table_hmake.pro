;+
; NAME:  
;
;    IPAC_TABLE_HMAKE
;
; DESCRIPTION:
;    
;    Create an IPAC table header.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    ipac_table_hmake,hdr,[/DATE],[/INITIALIZE]
;			
; INPUT KEYWORD PARAMETERS:
;
;    DATE: If set, encode the date in the header.
;
;    INITIALIZE: If set, clear the header before any additions.
;			
; OUTPUTS:
;
;    hdr: The new header, initialized if requested.
;
; NOTES:
;  
;    Creates an IPAC table header.  See also IPAC_TABLE_ADDPAR
;
; EXAMPLE:
;
;    ipac_table_hmake,hdr,/INIT,/DATE
;
; MODIFICATION HISTORY:
;    
;    2005-03-01 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2005 J.D. Smith
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

pro ipac_table_hmake,hdr,INITIALIZE=init,DATE=date
  if keyword_set(init) then hdr=''
  if keyword_set(date) then $
     ipac_table_addpar,hdr,'DATE',systime(0,/UTC)
end
