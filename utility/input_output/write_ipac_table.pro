;+
; NAME:  
;
;    WRITE_IPAC_TABLE
;
; DESCRIPTION:
;    
;    Write an ascii file in the form of an IPAC table
;    
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    File I/O
;    	
; CALLING SEQUENCE:
;
;    write_ipac_table,file,(NAMES=, v0,v1,[v2,v3,...,v24] | DATA=),
;       [HEADER=],[UNITS=]
;       
;
; INPUT PARAMETERS:
;
;    file: The IPAC table file to write

;    v0..v24: Input vectors, integer, string, float or double, to be
;       written as columns.  Must be passed with a NAMES keyword
;       string array naming the headers, one for each input column
;       vector.  Alternatively, an array of structures can be passed
;       in the DATA keyword, which is necessary if more than 25
;       columns of output are required.
;			
; OPTIONAL KEYWORD INPUT PARAMETERS:
;
;    NAMES: A string array with as many elements as input vectors,
;       giving the column name to be used in the output file.
;
;    DATA: Instead of a name list and individual vectors, an array of
;       structures of type {NAME1:value1,NAME2:value2,...} (e.g., as
;       returned by READ_IPAC_TABLE) can be passed, and the names will
;       be taken from the field name.  The values can be integer,
;       string, float, or double.  If NAMES is also passed, it will
;       override the field-based names.
;
;    HEADER: String array header to prepend to the data.  Can be
;       created with IPAC_TABLE_HMAKE and modified with
;       IPAC_TABLE_ADDPAR.
;
;    UNITS: A string array giving the units for each input vector or
;       field member of the DATA structure array.  Use blanks to omit
;       units for a subset of the columns.
;
;
; RESTRICTIONS:
;
;    Only writes float, double, integer, and string column types.
;    Blanks in header names are changed to underscores in structure
;    field names.  Integer types are tagged with "int", and read in by
;    default as a signed long, so larger data types may need special
;    treatment.
;
; NOTES:
;  
;    See http://spider.ipac.caltech.edu/staff/tab/ipactables.help for
;    format description.  See also READ_IPAC_TABLE.
;
; EXAMPLE:
;
;    write_ipac_table,'file.tbl',findgen(100),NAME='TEST',UNIT='m'
;
; MODIFICATION HISTORY:
;
;    2005-02-25 (J.D. Smith): Written
;-
;    $Id$
;
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
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

function write_ipac_table_space_pad,str,len,space_string
  COMPILE_OPT hidden
  spaces=len-strlen(str)
  if n_elements(space_str) eq 0 then $
     space_str=string(make_array(VALUE=32b,max(len)))
  left_spaces=spaces/2 & right_spaces=spaces-left_spaces
  left_spaces=strmid(space_str,0,left_spaces)
  right_spaces=strmid(space_str,0,right_spaces)
  return,left_spaces+str+right_spaces
end


pro write_ipac_table,file,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14, $
                     v15,v16,v17,v18,v19,v20,v21,v22,v23,v24,NAMES=names, $
                     DATA=data,HEADER=hdr,UNITS=units
  
  nvec=n_params()
  if size(file,/TYPE) ne 7 then message,'First argument must be filename.'
  nvec--
  
  ;;          B I  L  F  D            UI UL LL ULL 
  fmin_len=[0,7,7,12,15,25,0,0,0,0,0,0,7,12,22,22]
  
  if n_elements(data) ne 0 then begin 
     tnames=tag_names(data)
     nvec=n_elements(tnames) 
  endif 
  min_len=lonarr(nvec)
  type=lonarr(nvec)

  for i=0,n_elements(names)-1 do $
     names[i]=idl_validname(names[i],/CONVERT_ALL)

  if n_elements(data) ne 0 then begin 
     if size(data,/TYPE) ne 8 then message,'DATA must be structure array.'
     if n_elements(names) eq 0 then names=tnames else begin 
        if n_elements(names) ne nvec then $
           message,'Names are required for each data column.'
     endelse 
     for i=0,nvec-1 do begin 
        type[i]=size(data[0].(i),/TYPE)
        min_len[i]=type[i] eq 7?max(strlen(data.(i))):fmin_len[type[i]<15]
     endfor
     nrec=n_elements(data)
  endif else begin 
     if nvec lt 1 then message,'At least one data vector required.'
     if n_elements(names) ne nvec then $
        message,'Names are required for each data column.'
     vec_len=lonarr(nvec)
     switch nvec-1 of
        24: begin
           vec_len[24]=n_elements(v24)
           type[24]=size(v24,/TYPE)
           min_len[24]=type[24] eq 7?max(strlen(v24)):fmin_len[type[24]<15]
        end
        23: begin
           vec_len[23]=n_elements(v23)
           type[23]=size(v23,/TYPE)
           min_len[23]=type[23] eq 7?max(strlen(v23)):fmin_len[type[23]<15]
        end
        22: begin
           vec_len[22]=n_elements(v22)
           type[22]=size(v22,/TYPE)
           min_len[22]=type[22] eq 7?max(strlen(v22)):fmin_len[type[22]<15]
        end
        21: begin
           vec_len[21]=n_elements(v21)
           type[21]=size(v21,/TYPE)
           min_len[21]=type[21] eq 7?max(strlen(v21)):fmin_len[type[21]<15]
        end
        20: begin
           vec_len[20]=n_elements(v20)
           type[20]=size(v20,/TYPE)
           min_len[20]=type[20] eq 7?max(strlen(v20)):fmin_len[type[20]<15]
        end
        19: begin
           vec_len[19]=n_elements(v19)
           type[19]=size(v19,/TYPE)
           min_len[19]=type[19] eq 7?max(strlen(v19)):fmin_len[type[19]<15]
        end
        18: begin
           vec_len[18]=n_elements(v18)
           type[18]=size(v18,/TYPE)
           min_len[18]=type[18] eq 7?max(strlen(v18)):fmin_len[type[18]<15]
        end
        17: begin
           vec_len[17]=n_elements(v17)
           type[17]=size(v17,/TYPE)
           min_len[17]=type[17] eq 7?max(strlen(v17)):fmin_len[type[17]<15]
        end
        16: begin
           vec_len[16]=n_elements(v16)
           type[16]=size(v16,/TYPE)
           min_len[16]=type[16] eq 7?max(strlen(v16)):fmin_len[type[16]<15]
        end
        15: begin
           vec_len[15]=n_elements(v15)
           type[15]=size(v15,/TYPE)
           min_len[15]=type[15] eq 7?max(strlen(v15)):fmin_len[type[15]<15]
        end
        14: begin
           vec_len[14]=n_elements(v14)
           type[14]=size(v14,/TYPE)
           min_len[14]=type[14] eq 7?max(strlen(v14)):fmin_len[type[14]<15]
        end
        13: begin
           vec_len[13]=n_elements(v13)
           type[13]=size(v13,/TYPE)
           min_len[13]=type[13] eq 7?max(strlen(v13)):fmin_len[type[13]<15]
        end
        12: begin
           vec_len[12]=n_elements(v12)
           type[12]=size(v12,/TYPE)
           min_len[12]=type[12] eq 7?max(strlen(v12)):fmin_len[type[12]<15]
        end
        11: begin
           vec_len[11]=n_elements(v11)
           type[11]=size(v11,/TYPE)
           min_len[11]=type[11] eq 7?max(strlen(v11)):fmin_len[type[11]<15]
        end
        10: begin
           vec_len[10]=n_elements(v10)
           type[10]=size(v10,/TYPE)
           min_len[10]=type[10] eq 7?max(strlen(v10)):fmin_len[type[10]<15]
        end
        9: begin
           vec_len[9]=n_elements(v9)
           type[9]=size(v9,/TYPE)
           min_len[9]=type[9] eq 7?max(strlen(v9)):fmin_len[type[9]<15]
        end
        8: begin
           vec_len[8]=n_elements(v8)
           type[8]=size(v8,/TYPE)
           min_len[8]=type[8] eq 7?max(strlen(v8)):fmin_len[type[8]<15]
        end
        7: begin
           vec_len[7]=n_elements(v7)
           type[7]=size(v7,/TYPE)
           min_len[7]=type[7] eq 7?max(strlen(v7)):fmin_len[type[7]<15]
        end
        6: begin
           vec_len[6]=n_elements(v6)
           type[6]=size(v6,/TYPE)
           min_len[6]=type[6] eq 7?max(strlen(v6)):fmin_len[type[6]<15]
        end
        5: begin
           vec_len[5]=n_elements(v5)
           type[5]=size(v5,/TYPE)
           min_len[5]=type[5] eq 7?max(strlen(v5)):fmin_len[type[5]<15]
        end
        4: begin
           vec_len[4]=n_elements(v4)
           type[4]=size(v4,/TYPE)
           min_len[4]=type[4] eq 7?max(strlen(v4)):fmin_len[type[4]<15]
        end
        3: begin
           vec_len[3]=n_elements(v3)
           type[3]=size(v3,/TYPE)
           min_len[3]=type[3] eq 7?max(strlen(v3)):fmin_len[type[3]<15]
        end
        2: begin
           vec_len[2]=n_elements(v2)
           type[2]=size(v2,/TYPE)
           min_len[2]=type[2] eq 7?max(strlen(v2)):fmin_len[type[2]<15]
        end
        1: begin
           vec_len[1]=n_elements(v1)
           type[1]=size(v1,/TYPE)
           min_len[1]=type[1] eq 7?max(strlen(v1)):fmin_len[type[1]<15]
        end
        0: begin
           vec_len[0]=n_elements(v0)
           type[0]=size(v0,/TYPE)
           min_len[0]=type[0] eq 7?max(strlen(v0)):fmin_len[type[0]<15]
           break
        end
        else: message,'At least 1 and no more than 25 vector arguments.'
     endswitch
     
     if ~array_equal(vec_len,vec_len[0]) then $
        message,'All arrays must be of equal length'
     nrec=vec_len[0]
  endelse 
  
  if n_elements(units) ne 0 && n_elements(units) ne nvec then $
     message,'Units must be present for each data column if used.'
  
  wh=where(type lt 1 OR type eq 6 OR (type ge 8 and type le 11),cnt)
  if cnt gt 0 then $
     message,'Only integer, float, double, and string data allowed.'
  
  ;; Construct | label header | section
  len=min_len > strlen(names) > 4
  if n_elements(units) ne 0 then len=len > strlen(units)
  form='("|",'+strtrim(nvec,2)+'(" ",A," |"))'
  
  ;; Names
  colhdr=string(FORMAT=form,write_ipac_table_space_pad(names,len,ss))
  
  ;; Types
  type_names=ipac_table_typename(TYPE=type,FORMAT=typeform)
  colhdr=[colhdr, $
          string(FORMAT=form,write_ipac_table_space_pad(type_names,len,ss))]
  
  ;; Units
  if n_elements(units) ne 0 then $
     colhdr=[colhdr, $
             string(FORMAT=form,write_ipac_table_space_pad(units,len,ss))]
  
  ;; Write file
  openw,un,file,/GET_LUN
  if n_elements(hdr) ne 0 then printf,un,transpose(hdr)
  printf,un,transpose(colhdr)
  
  form='("  ",'+strjoin(typeform+strtrim(len,2),',"   ",')+")"
  if n_elements(data) ne 0 then begin 
     for i=0,nrec-1 do $
        printf,un,FORMAT=form,data[i]
  endif else begin 
     case nvec-1 of
        0: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i]
        end
        1: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i]
        end
        2: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i]
        end
        3: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i]
        end
        4: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i]
        end
        5: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i]
        end
        6: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i]
        end
        7: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i]
        end
        8: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i]
        end
        9: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i]
        end
        10: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i]
        end
        11: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i]
        end
        12: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i]
        end
        13: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i]
        end
        14: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i]
        end
        15: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i]
        end
        16: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i]
        end
        17: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i]
        end
        18: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i]
        end
        19: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i]
        end
        20: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i],v20[i]
        end
        21: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i],v20[i],v21[i]
        end
        22: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i],v20[i],v21[i],v22[i]
        end
        23: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i],v20[i],v21[i],v22[i],v23[i]
        end
        24: begin
           for i=0,nrec-1 do $
              printf,un,FORMAT=form,v0[i],v1[i],v2[i],v3[i], $
                     v4[i],v5[i],v6[i],v7[i],v8[i],v9[i],v10[i],v11[i], $
                     v12[i],v13[i],v14[i],v15[i],v16[i],v17[i],v18[i], $
                     v19[i],v20[i],v21[i],v22[i],v23[i],v24[i]
        end
     endcase 
  endelse 
  free_lun,un
  
end


;perl -e 'for $i (reverse 0..24) {print "$i: begin\nvec_len[$i]=n_elements(v$i)\ntype[$i]=size(v$i,/TYPE)\nmin_len[$i]=type[$i] eq 7?max(strlen(v$i)):fmin_len[type[$i]<15]\nend\n"}'
  
;perl -e 'for $i (0..24) {print "$i: begin\nfor i=0,nrec-1 do \$\nprintf,un,FORMAT=form,".join(",", map {"v${_}[i]"} 0..$i)."\nend\n"}'
  
; perl -e 'use Text::Wrap qw(wrap $columns $break); $break="[,\\s]"; $columns=55; for $i (0..24) {$line="$i: begin\nfor i=0,nrec-1 do \$\nprintf,un,FORMAT=form,".join(",", map {"v${_}[i]"} 0..$i)."\nend\n"; $line=wrap("\t","",$line); $line=~s/]\nv/], \$\nv/g; print $line;}'
