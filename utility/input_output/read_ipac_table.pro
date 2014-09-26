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
;    for format description.  In particular, the valid columns for
;    data are:
;
;      |  column1 |  column2 | column3 | column4  |    column5       |   
;      |  double  |  double  |   int   |   double |     char         | 
;      |  unit1   |  unit2   |  unit3  |   unit4  |     unit5        | 
;      111111111111222222222223333333333444444444445555555555555555555  
;
; EXAMPLE:
;
;    foo=read_ipac_table(file)
;    print,foo
;
; MODIFICATION HISTORY:
;
;    2014-09-02 (J.D. Smith): Correctly read tables with large number
;                             of columns.
;    2007-02-12 (J.D. Smith): Null value check, using explicit field
;                             widths, and handling tabs.
;    2006-08-04 (J.D. Smith): Improved header reading speed for long
;                             headers.
;    2005-03-25 (J.D. Smith): Brought into compliance with "new" IRSA
;                             IPAC TABLE Format standard.
;    2005-02-25 (J.D. Smith): Pre-allocate return for speedup.
;    2003-11-24 (J.D. Smith): Better header keyword value treatment.
;    2002-08-27 (J.D. Smith): Initial migration from SMART codebase.
;    2001-12-02 (J.D. Smith): Written, based very loosely on routine
;                             "read_tbl" provided by Jim Ingalls of
;                             the SSC.
;        
;-
;    $Id$
;
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002,2004,2005,2006,2007 J.D. Smith
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

function read_ipac_table_readline,line,data_fmt
  ;; data_fmt is an array of structures ala:
  ;;   {format: fmt, data_elem:de}
  ;; where fmt is a complete and valid format string, and de is a
  ;; pointer to a structure with matching types/names.  
  ;; reads can then account for long lines with too many format codes.
  ;; returns a single concatenated structure of all the resulting
  ;; data_elem structures.
  
  foreach fmt,data_fmt do begin 
     de=*fmt.data_elem
     reads,line,de,FORMAT=fmt.format
     if n_elements(data_elems) eq 0 then data_elems=de else $
        data_elems=create_struct(data_elems,de) ; concatenate
  endforeach 
  return,data_elems
end

function read_ipac_table,file, hdr,UNITS=units
  openr,un,file,/get_lun
  line=''
  line_cnt=0
  nlines=file_lines(file)
  at_data=(chead=0)
  hcnt=0L
  max_tok=50                    ;maximum number of tokens to read in one go
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
              sep_pos=strsplit(line,'|')
              tok=strtrim(strsplit(line,'|',/EXTRACT),2) 
              
              nt=n_elements(tok) 
              width=[sep_pos[1:*],strlen(line)]-sep_pos
              width[0]+=1       ;first gets an extra column
              toff=sep_pos+1L   ;T format code is 1-based
              toff[0]--         ;first column starts on the separator
              
              case chead++ of 
                 0: begin       ;column names
                    tags=strarr(nt)
                    ;; Remove disallowed characters from the tag
                    for i=0,nt-1 do begin 
                       tags[i]=idl_validname(strjoin(strsplit($
                               tok[i],'-',/EXTRACT),'_'), /CONVERT_ALL)
                    endfor
                 end
                 1: begin       ;format flags
                    got=bytarr(nt)
                    format=strarr(nt)
                    tok=strlowcase(tok)
                    
                    full_data_elem=!NULL
                    ngrps=1L+long((nt-1L)/max_tok) 
                    data_fmt=replicate({format:'',data_elem:ptr_new()},ngrps)
                    for grp=0L,ngrps-1L do begin 
                       ;; bundle in groups of length  max_tok
                       beg=grp*max_tok
                       en=((grp+1L)*max_tok-1L)<(nt-1L)
                       data_elem=!NULL
                       for i=beg,en do begin 
                          case 1 of 
                             strmatch(tok[i],'r*') || $
                                strmatch(tok[i],'f*'): begin 
                                val=0.0
                                got[i]=2b
                                format[i]='F'
                             end 
                             
                             strmatch(tok[i],'i*'): begin 
                                val=0L
                                got[i]=1b ;indicate integer
                                format[i]='I'
                             end 
                             
                             strmatch(tok[i],'d*'): begin 
                                val=0.0D
                                got[i]=3b
                                format[i]='D'
                             end
                             
                             strmatch(tok[i],'c*'): begin 
                                got[i]=0b ;indicate string
                                val=''
                                format[i]='A'
                             end
                             1: message,'Unknown format code '+tok[i]
                          endcase 
                       
                          ;; Create/extend the input data structure
                          if n_elements(data_elem) eq 0 then $
                             data_elem=create_struct(tags[i],val) $
                          else data_elem=create_struct(data_elem,tags[i],val) 
                       endfor
                       if n_elements(full_data_elem) eq 0 then $
                          full_data_elem=data_elem $
                       else full_data_elem=create_struct(full_data_elem, $
                                                           data_elem)
                       subfmt=reform(['T'+ $
                                        transpose(strtrim(toff[beg:en],2)), $
                                        transpose(format[beg:en]+$
                                                    strtrim(width[beg:en],2))],$
                                       2*(en-beg+1L))
                       data_fmt[grp].format='('+strjoin(subfmt,",")+')'
                       data_fmt[grp].data_elem=ptr_new(data_elem,/NO_COPY)
                    endfor
                 end
                 2: begin       ;units
                    units=tok
                 end 
                 3: begin       ;null value
                    nullvalue=strtrim(tok,2)
                    null_elem=full_data_elem
                    for i=0,n_elements(got)-1 do begin 
                       case got[i] of $
                          0b: val=nullvalue[i] ; string
                          1b: val=stregex(nullvalue[i],'[^0-9-]',/BOOLEAN)? $
                                  -9999:long(nullvalue[i]) ; int
                          2b: val=!VALUES.F_NAN            ; float
                          3b: val=!VALUES.D_NAN            ; double
                       endcase 
                       null_elem.(i)=val
                    endfor 
                 end 
                 else: ;just skip any other column header rows
              endcase 
           end 
           else: begin 
              at_data=1
              nlines=nlines-line_cnt+1
              
              ret=replicate(full_data_elem,nlines)
              line_cnt=0
           end 
        endcase
        if at_data eq 0 then continue
     endif 
     
     ;; Trim out "null" entries, and replace
     if n_elements(nullvalue) ne 0 then begin 
        parts=strmid(line,toff-1,width)
        null=where(strtrim(parts,2) eq nullvalue,nullcnt,COMPLEMENT=keep, $
                   NCOMPLEMENT=keepcnt)
        
        if nullcnt gt 0 then begin    ; some null values
           if keepcnt gt 0 then begin ; but not all null values
              p=parts[null]
              ;; Set to 00000 so it will always parse
              mx=max(width[null])
              strput,p,string(make_array(mx,VALUE=byte('0')))
              parts[null]=p
              line=strjoin(parts)
              ;; read data
              data_elems=read_ipac_table_readline(line,data_fmt)
              ;; set nulls in
              for i=0,nullcnt-1 do data_elems.(null[i])=null_elem.(null[i])
           endif else data_elems=null_elem ;no reading required
        endif else data_elems=read_ipac_table_readline(line,data_fmt)
     endif else data_elems=read_ipac_table_readline(line,data_fmt)
     
     ret[line_cnt++]=data_elems
  endwhile
  
  if hcnt gt 0 then hdr=hdr[0:hcnt-1]
  
  if line_cnt lt nlines then begin 
     ret=ret[0:line_cnt-1]
  endif 
  
  wh=where(got eq 0b,stringcnt)
  for i=0,stringcnt-1 do ret.(wh[i])=strtrim(ret.(wh[i]),2)
  if arg_present(units) && chead lt 2 then units=replicate('',n_tags(ret))
  free_lun,un
  ptr_free,data_fmt.data_elem
  return,ret
end
