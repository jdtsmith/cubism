;+
; NAME:  
;
;    READ_IPAC_TABLE
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Reads ascii files in the form of IPAC tables, and returns
;    columnar data in a structure.
;    
; CATEGORY:
;
;    SMART IRS Spectral Reduction, Analysis and Processing.
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    st=read_ipac_table(file, HEADERS=)
;
; INPUT PARAMETERS:
;
;    file: The IPAC table file.  
;			
; INPUT KEYWORD PARAMETERS:
;
;    HEADERS: If used, return the header variables set at the
;       beginning of the table (\key=value) as a structure.
;			
; OUTPUTS:
;
;    st: A list of structures with fields set by the column headers in
;       the table file, of the appropriate type.
;
; RESTRICTIONS:
;
;    Only reads float, double, integer, and string column types.
;    Blanks in header names are changed to underscores in structure
;    field names.
;
; PROCEDURES:
;
;    Notable routines or classes (*not* parent classes -- see next entry)
;    it relies on.
;
; NOTES:
;  
;    Additional description and other information which doesn't fit elsewhere.
;
; EXAMPLE:
;
;    foo=read_ipac_table
;    SMART_ROUTINE,a,b,KEYWORD=foo
;    print,foo
;
; MODIFICATION HISTORY:
;    
;    2001-12-02 (J.D. Smith): Written, based very loosely on routine
;        "read_tbl" provided by Jim Ingalls of the SSC.
;        
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 Cornell University
;
;  This file is part of SMART.
;
;  SMART is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  SMART is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with SMART; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################

function read_ipac_table,file, HEADERS=hdr
  openr,un,file,/get_lun
  line=''
  at_data=(got_tags=(got_type=0))
  while NOT eof(un) do begin 
     readf,un,line
     if stregex(line,'^ *$',/BOOLEAN) then continue ; Skip blank lines
     if at_data eq 0 then begin 
        firstchar = strmid(line,0,1)
        case firstchar of
           '\': begin 
              parts=strtrim(stregex(line,'^\\([a-z ]+=?)(.*)$', $
                                    /SUBEXPR,/EXTRACT),2)
              if n_elements(hdr) eq 0 then begin 
                 hdr=create_struct(parts[1],parts[2]) 
              endif else hdr=create_struct(hdr,parts[1],parts[2])
           end
           '|': begin
              tok=strtrim(strsplit(line,'|',/EXTRACT),2) 
              if got_tags eq 0 then begin 
                 tags=tok
                 got_tags=1
                 break
              endif 
              if got_type then break ; skip anything beyond the type field
              for i=0,n_elements(tok)-1 do begin 
                 case 1 of 
                    strmatch(tok[i],'r*'): val=0.0
                    strmatch(tok[i],'i*'): val=0L
                    strmatch(tok[i],'d*'): val=0.0D
                    strmatch(tok[i],'c*'): val=''
                    1: begin 
                       print,'warning, unkown type '+tok[i]
                       break
                    end 
                 endcase 
                 if n_elements(data_elem) eq 0 then $
                    data_elem=create_struct(tags[i],val) $
                 else data_elem=create_struct(data_elem,tags[i],val) 
              endfor 
              got_type=1
           end
           else: at_data=1
        endcase
        if at_data eq 0 then continue
     endif 
     ;; reading data
     reads,line,data_elem
     if n_elements(ret) eq 0 then ret=data_elem else ret=[ret,data_elem]
  endwhile
  free_lun,un
  return,ret
end
