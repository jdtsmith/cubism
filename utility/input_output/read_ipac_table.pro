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
;    st=read_ipac_table(file, [hdr], [HEADERS=,UNITS=])
;
; INPUT PARAMETERS:
;
;    file: The IPAC table file.  
;			
; OPTIONAL OUTPUT PARAMETERS:
;
;    hdr: The headers as string array.
;    
; OPTIONAL OUTPUT KEYWORD PARAMETERS:
;
;    HEADERS: If used, return the header variables set at the
;       beginning of the table (\type key=value) as a structure.
;
;    UNITS: The units for each data column, if available
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
;    2005-02-25 (J.D. Smith): Pre-allocate return for speedup.
;    2003-11-24 (J.D. Smith): Better header keyword value treatment.
;    2002-08-27 (J.D. Smith): Initial migration from SMART codebase.
;    2001-12-02 (J.D. Smith): Written, based very loosely on routine
;        "read_tbl" provided by Jim Ingalls of the SSC.
;        
;-
;    $Id$
;
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2004,2005 J.D. Smith
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

function read_ipac_table,file, hdr,HEADERS=st_hdrs,UNITS=units
  openr,un,file,/get_lun
  line=''
  line_cnt=0
  nlines=file_lines(file)
  at_data=(chead=0)
  while NOT eof(un) do begin 
     readf,un,line
     if stregex(line,'^ *$',/BOOLEAN) then continue ; Skip blank lines
     if at_data eq 0 then begin 
        line_cnt++
        firstchar = strmid(line,0,1)
        case firstchar of
           '\': begin 
              if ~arg_present(hdr) then break
              if n_elements(hdr) eq 0 then hdr=[line] else hdr=[hdr,line]
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
              if n_elements(st_hdrs) eq 0 then begin 
                 st_hdrs=create_struct(parts[2],val) 
              endif else st_hdrs=create_struct(st_hdrs,parts[2],val)
           end
           '|': begin
              line=strtrim(line) ;no trailing blanks, please
              sep_pos=[strsplit(line,'|'),strlen(line)-1]
              tok=strtrim(strsplit(line,'|',/EXTRACT),2) 
              case chead++ of 
                 0: begin       ;column names
                    tags=strarr(n_elements(tok))
                    ;; Remove disallowed characters from the tag
                    for i=0,n_elements(tok)-1 do $
                       tags[i]=idl_validname(tok[i],/CONVERT_ALL)
                 end
                 1: begin       ;format flags
                    got=bytarr(n_elements(tags))
                    for i=0,n_elements(tok)-1 do begin 
                       case 1 of 
                          strmatch(tok[i],'r*'): begin 
                             val=0.0
                             if n_elements(format) eq 0 then $
                                format=['F0'] else format=[format,'F0']
                          end 
                          strmatch(tok[i],'i*'): begin 
                             val=0L
                             got[i]=2b ;indicate integer
                             if n_elements(format) eq 0 then $
                                format=['I0'] else format=[format,'I0']
                          end 
                          strmatch(tok[i],'d*'): begin 
                             val=0.0D
                             if n_elements(format) eq 0 then $
                                format=['D0'] else format=[format,'D0']
                          end
                          strmatch(tok[i],'c*'): begin 
                             got[i]=1b ;indicate string
                             val=''
                             aform=['T'+strtrim(sep_pos[i],2), $
                                    'A'+strtrim(sep_pos[i+1]-sep_pos[i]+1,2)]
                             if n_elements(format) eq 0 then $
                                format=[aform] else format=[format,aform]
                          end
                          1: begin 
                             print,'warning, unkown type '+tok[i]
                             break
                          end 
                       endcase 
                 
                       ;; Create/extend data structure
                       if n_elements(data_elem) eq 0 then $
                          data_elem=create_struct(tags[i],val) $
                       else data_elem=create_struct(data_elem,tags[i],val) 
                    endfor
                 end
                 2: begin       ;units
                    units=tok
                 end 
              endcase 
           end 
           else: begin 
              at_data=1
              ret=replicate(data_elem,nlines-line_cnt+1)
              line_cnt=0
           end 
        endcase
        if at_data eq 0 then continue
     endif 
     ;; reading data
     reads,line,data_elem,FORMAT='('+strjoin(format,",")+')'
     ret[line_cnt++]=data_elem
  endwhile
  
  if n_elements(got_string) ne 0 then begin 
     wh=where(got_string eq 1b,cnt)
     for i=0,cnt-1 do ret.(wh[i])=strtrim(ret.(wh[i]),2)
  endif 
  if arg_present(units) && chead lt 2 then units=replicate('',n_tags(ret))
  free_lun,un
  return,ret
end
