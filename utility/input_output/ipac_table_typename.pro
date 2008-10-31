;+
; NAME:  
;
;    IPAC_TABLE_TYPENAME
;
; DESCRIPTION:
;    
;    Return IPAC table type name for given IDL variable.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    tname=ipac_table_typename(var,[TYPE=,FORMAT=])
;
; INPUT PARAMETERS:
;
;    var: The idl variable to pass, unless TYPE is passed.
;
; INPUT KEYWORD PARAMETERS:
;
;    TYPE: IDL type code, in lieu of passing variable.
;    
; OUTPUT KEYWORD PARAMETERS:
; 
;    FORMAT: The format code to use for this type.
;
; OUTPUTS:
;
;    tname: The name for an IPAC table column header type.
;
; NOTES:
;  
;    See also IPAC_TABLE_HMAKE
;
; MODIFICATION HISTORY:
;    
;    2005-03-24 (J.D. Smith): Written
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

function ipac_table_typename,val,TYPE=type,FORMAT=format
  tname=["",replicate("int",3),"real","double","","char",replicate("",4), $
         replicate("int",4)]
  
  if n_elements(type) eq 0 then begin 
     if n_elements(val) eq 0 then message,'Must pass value or TYPE'
     type=size(val,/TYPE)
  endif 
  
  if arg_present(format) then $
     format=(["","I","I","I","G","G","","A",replicate("",4), $
              replicate("I",4)])[type]
  return,tname[type]
end
