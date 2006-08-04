;+
; NAME:  
;
;    READ_IPAC_TABLE
;
; DESCRIPTION:
;    
;    Reads ASCII files in the form of IPAC tables, and returns
;    columnar data in a structure.
;    
; CATEGORY:
;
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    st=read_ipac_table(file, [hdr], [UNITS=])
;
; INPUT PARAMETERS:
;
;    file: The IPAC table file.  
;			
; OPTIONAL OUTPUT PARAMETERS:
;
;    hdr: The header cards as string array.
;    
; OPTIONAL OUTPUT KEYWORD PARAMETERS:
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
;    See http://irsa.ipac.caltech.edu/applications/DDGEN/Doc/ipac_tbl.html
;    for format description.
;
; EXAMPLE:
;
;    foo=read_ipac_table
;    print,foo
;
; MODIFICATION HISTORY:
;
;    2006-08-04 (J.D. Smith): Improved header reading speed for long
;                             headers.
;    2005-03-25 (J.D. Smith): Brought into compliance with "new" IRSA
;                             IPAC TABLE Format standard.
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
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

function read_ipac_table,file, hdr,UNITS=units
  openr,un,file,/get_lun
  line=''
  line_cnt=0
  nlines=file_lines(file)
  at_data=(chead=0)
  hcnt=0L
  while ~eof(un) do begin 
     readf,un,line
     if ~strtrim(line,2) then continue ; Skip blank lines
     if at_data eq 0 then begin 
        line_cnt++
        firstchar = strmid(line,0,1)
        case firstchar of
           '\': begin 
              if ~arg_present(hdr) then break
              if n_elements(hdr) le hcnt then begin 
                 if n_elements(hdr) eq 0 then hdr=[''] else begin 
                    hdr=[hdr,strarr(n_elements(hdr))]
                 endelse 
              endif 
              hdr[hcnt++]=line
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
                       tags[i]=idl_validname(strjoin(strsplit($
                               tok[i],'-',/EXTRACT),'_'), /CONVERT_ALL)
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
                 else: ;just skip other column header rows
              endcase 
           end 
           else: begin 
              at_data=1
              nlines=nlines-line_cnt+1
              ret=replicate(data_elem,nlines)
              line_cnt=0
           end 
        endcase
        if at_data eq 0 then continue
     endif 
     ;; reading data
     reads,line,data_elem,FORMAT='('+strjoin(format,",")+')'
     ret[line_cnt++]=data_elem
  endwhile
  
  if hcnt gt 0 then hdr=hdr[0:hcnt-1]
  
  if line_cnt lt nlines then begin 
     ret=ret[0:line_cnt-1]
  endif 
  
  if n_elements(got_string) ne 0 then begin 
     wh=where(got_string eq 1b,cnt)
     for i=0,cnt-1 do ret.(wh[i])=strtrim(ret.(wh[i]),2)
  endif 
  if arg_present(units) && chead lt 2 then units=replicate('',n_tags(ret))
  free_lun,un
  return,ret
end
