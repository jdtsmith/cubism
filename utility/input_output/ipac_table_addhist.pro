;+
; NAME:  
;
;    IPAC_TABLE_ADDHIST
;
; DESCRIPTION:
;    
;    Add a history/comments to an IPAC table header.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    ipac_table_addhist,hist_or_comment,hdr,[COMMENT=,BLANK=]
;
; INPUT PARAMETERS:
;
;    hist_or_comment: The string or string array to add, prepended
;       with \ HISTORY or \ COMMENT.
;
;    hdr: The IPAC TABLE header to be modified.
;
; INPUT KEYWORD PARAMETERS:
;
;    COMMENT: If set, use COMMENT instead of HISTORY.
;    BLANK: If set, use neither COMMENT nor HISTORY, but leave blank.
;
; OUTPUTS:
;
;    hdr: The new header, with history/comments added.
;
; NOTES:
;  
;    See also IPAC_TABLE_HMAKE
;
; EXAMPLE:
;
;    ipac_table_addhist,hdr,'This File is processed with vXYZ',/COMMENT
;
; MODIFICATION HISTORY:
;    
;    2005-03-03 (J.D. Smith): Written
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

pro ipac_table_addhist,value,hdr,COMMENT=comm,BLANK=blank
  if n_elements(value) eq 0 then $
     message,'Invalid IPAC TABLE header: must be string or string array.'
  
  add=(keyword_set(comm)?'\ COMMENT ': $
       (keyword_set(blank)?'\ ':'\ HISTORY ')) + value
  
  ;; Add comment before history, and history at end of History
  wh=where(strpos(hdr,'\ HISTORY') eq 0,cnt)
  if cnt gt 0 then insert_at=keyword_set(comm)?wh[0]:wh[cnt-1]+1
  
  if n_elements(hdr) gt 0 && hdr[0] then begin
     if n_elements(insert_at) ne 0 && insert_at lt n_elements(hdr) then $
        hdr=[hdr[0:insert_at-1],add,hdr[insert_at:*]] else hdr=[hdr,add]
  endif else hdr=[add]
end 
