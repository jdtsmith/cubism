;+
; NAME:  
;
;    IPAC_TABLE_ADDPAR
;
; DESCRIPTION:
;    
;    Add a keyword to an IPAC table header.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    ipac_table_addpar,hdr,name,value,[BEFORE=],[AFTER=],[FORMAT=]
;
; INPUT PARAMETERS:
;
;    hdr: The IPAC TABLE header to be modified.
;
;    name: The name of the keyword.
;
;    value: The value to assign the keyword
;
; INPUT KEYWORD PARAMETERS:
;
;    BEFORE: The name of a keyword to set this keyword before in the
;       header.
;
;    AFTER: The name of a keyword to set this keyword after in the
;       header.
;
;    FORMAT: The format code, like F7.3, to be used to print the
;       value.  Keywords of the same name will be replaced.  Defaults
;       to native F, I, etc.
;			
; OUTPUTS:
;
;    hdr: The new header, with keyword added.
;
; NOTES:
;  
;    Keywords are added before any COMMENT or HISTORY comment
;    sections.  See also IPAC_TABLE_HMAKE, and IPAC_TABLE_ADDHIST.
;
; EXAMPLE:
;
;    ipac_table_addpar,hdr,'FILENAME','abc.txt'
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

pro ipac_table_addpar,hdr,name,value,BEFORE=before,AFTER=after,FORMAT=format
  
  tname=ipac_table_typename(value,FORMAT=f)
  if n_elements(format) eq 0 then format=f
  
  if n_elements(hdr) gt 0 && hdr[0] then begin 
     wh=where(strpos(hdr,'\'+name+' *=') eq 0,cnt)
     if cnt gt 0 then repl=wh[0] else begin 
        if keyword_set(before) then begin 
           wh=where(stregex(hdr,'^\\'+before+' *=',/BOOLEAN), cnt)
           if cnt gt 0 then insert_at=wh[0]
        endif 
        
        if keyword_set(after) then begin 
           wh=where(stregex(hdr,'^\\'+after+' *=',/BOOLEAN),cnt)
           if cnt gt 0 then insert_at=wh[0]+1
        endif 
        
        if n_elements(insert_at) eq 0 then begin ;before comment/history
           wh=where(stregex(hdr,'^\\ (COMMENT|HISTORY)',/BOOLEAN),cnt)
           if cnt gt 0 then insert_at=wh[0]
        endif 
     endelse 
  endif  
  
  if size(value,/TYPE) eq 7 then value="'"+value+"'"
  record=string(FORMAT='("\",A,"= ",'+format+')',name,value)
  
  if n_elements(hdr) gt 0 && hdr[0] then begin 
     if n_elements(repl) gt 0 then hdr[repl]=record else begin 
        if n_elements(insert_at) ne 0 then begin 
           if insert_at le 0 then hdr=[record,hdr] else $
              if insert_at ge n_elements(hdr) then hdr=[hdr,record] else $
                 hdr=[hdr[0:insert_at-1],record,hdr[insert_at:*]]
        endif else hdr=[hdr,record]
     endelse
  endif else hdr=[record]
end
