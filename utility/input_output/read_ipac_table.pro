;+
; NAME:  
;
;    READ_IPAC_TABLE
;
; DESCRIPTION:
;    
;    Reads ascii files in the form of IPAC tables, and returns
;    columnar data in a structure.
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
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
;       beginning of the table (\type key=value) as a structure.
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
;    See http://spider.ipac.caltech.edu/staff/tab/ipactables.help for
;    format description
;
; EXAMPLE:
;
;    foo=read_ipac_table
;    print,foo
;
; MODIFICATION HISTORY:
;
;    2003-11-24 (J.D. Smith): Better header keyword value treatment.
;    2002-08-27 (J.D. Smith): Initial migration from SMART codebase.
;    2001-12-02 (J.D. Smith): Written, based very loosely on routine
;        "read_tbl" provided by Jim Ingalls of the SSC.
;        
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2004 J.D. Smith
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

function read_ipac_table,file, HEADERS=hdr_in
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
              parts=strtrim( $
                    stregex(line, $
                            '^\\(REAL|CHAR|INT) +([a-zA-Z0-9_ ]+)=(.*)$', $
                            /SUBEXPR,/EXTRACT),2)
              if strlen(parts[0]) eq 0 then break ;probably a comment
              skip=0
              case parts[1] of
                 "REAL": val=double(parts[3])
                 "CHAR": val=parts[3]
                 "INT": val=long(parts[3])
                 else: skip=1
              endcase
              if skip then break
              if n_elements(hdr) eq 0 then begin 
                 hdr=create_struct(parts[2],val) 
              endif else hdr=create_struct(hdr,parts[2],val)
           end
           '|': begin
              line=strtrim(line) ;no trailing blanks, please
              sep_pos=[strsplit(line,'|'),strlen(line)-1]
              tok=strtrim(strsplit(line,'|',/EXTRACT),2) 
              if got_tags eq 0 then begin 
                 tags=tok
                 got_tags=1
                 break
              endif 
              ;; skip any other headers beyond the type field
              if got_type then break 
              for i=0,n_elements(tok)-1 do begin 
                 case 1 of 
                    strmatch(tok[i],'r*'): begin 
                       val=0.0
                       if n_elements(format) eq 0 then format=['F0'] else $
                          format=[format,'F0']
                    end 
                    strmatch(tok[i],'i*'): begin 
                       val=0L
                       if n_elements(format) eq 0 then format=['I0'] else $
                          format=[format,'I0']
                    end 
                    strmatch(tok[i],'d*'): begin 
                       val=0.0D
                       if n_elements(format) eq 0 then format=['D0'] else $
                          format=[format,'D0']
                     end
                     strmatch(tok[i],'c*'): begin 
                        val=''
                        aform=['T'+strtrim(sep_pos[i],2), $
                               'A'+strtrim(sep_pos[i+1]-sep_pos[i]-1,2)]
                        if n_elements(format) eq 0 then format=[aform] else $
                           format=[format,aform]
                     end
                    1: begin 
                       print,'warning, unkown type '+tok[i]
                       break
                    end 
                 endcase 
                 
                 ;; Remove disallowed characters from the tag
                 unclean_tag=tags[i]
                 tags[i]=''
                 while ((pos=stregex(unclean_tag,'[^0-9 _$A-Za-z]'))) $
                    ne -1 do begin 
                    tags[i]=tags[i]+strmid(unclean_tag,0,pos)+'_'
                    unclean_tag=strmid(unclean_tag,pos+1)
                 endwhile 
                 tags[i]=tags[i]+unclean_tag
                 
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
     reads,line,data_elem,FORMAT='('+strjoin(format,",")+')'
     if n_elements(ret) eq 0 then ret=data_elem else ret=[ret,data_elem]
  endwhile
  if arg_present(hdr_in) then hdr_in=hdr
  free_lun,un
  return,ret
end
