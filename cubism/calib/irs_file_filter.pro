;+
; NAME:
;
;    IRS_FILE_FILTER
;
; DESCRIPTION:
;
;    Group passed files into matching IRS spectral records.
;
; CATEGORY:
;
;    IRS Spectral Reduction, Analysis and Processing.
;    File Matching.
;    
; CALLING SEQUENCE:
;
;    groups=irs_file_filter(files,[COUNT=,MODULE_ONLY=,EXTRA_KEYS=,
;                                  RETURN_RECORDS=])
;
; INPUT PARAMETERS:
;
;    files: A list of IRS fits files (BCD or otherwise) to group.
;
; INPUT KEYWORD PARAMETERS:
;
;    MODULE_ONLY: Group records by module only, instead of grouping by
;       AOR and FOVID (i.e., which slit/position).
;
;    EXTRA_KEYS: List of extra keys to extract from the FITS headers
;       and add to each record.
;
;    RETURN_RECORDS: If set, return individual records, one per file,
;       instead of grouping records by AOR/module/etc.
;
; OUTPUT KEYWORD PARAMETERS:
;
;    COUNT: Number of records returned.
;       
; OUTPUTS:
;
;    groups: A list of structures with grouped or by individual
;       filename, according to the passed parameters, and the
;       RETURN_RECORDS keyword.
;
; MODIFICATION HISTORY:
;
;    2004-09-21 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2004, 2005 J.D. Smith
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
;; Boston, MA 02110-1301, USA.
;
;##############################################################################
  
function irs_file_filter,files,COUNT=cnt,MODULE_ONLY=mo,EXTRA_KEYS=ek, $
                         RETURN_RECORDS=rr
  if keyword_set(mo) then begin 
     match_module=1
     match_aor=0
     match_fov=0
  endif else begin 
     match_module=0
     match_aor=1
     match_fov=1
  endelse 
  
  for i=0,n_elements(files)-1 do begin 
     file=irs_associated_file(files[i]) ;get the bcd header...
     if size(file,/TYPE) ne 7 then continue
     hdr=headfits(file)
     fov=long(sxpar(hdr,'FOVID'))
     void=irs_fov(fov,MODULE=md)
     if n_elements(md) eq 0 then continue
     rec={FILE:files[i], $
          AOR:long(sxpar(hdr,'AORKEY')), $
          OBJECT:strtrim(sxpar(hdr,'OBJECT'),2), $
          FOV:fov, $
          MODULE:md, $
          NCYCLES:long(sxpar(hdr,'NCYCLES')), $
          TIME:float(sxpar(hdr,'EXPTOT_T')), $
          STEPS:long([sxpar(hdr,'STEPSPAR'),sxpar(hdr,'STEPSPER')])}
     ;; Add extra keywords if requested
     for j=0,n_elements(ek)-1 do begin 
        key=sxpar(hdr,ek[j],COUNT=key_cnt)
        if key_cnt eq 0 then continue
        if size(key,/TYPE) eq 4 then key=double(key) ;standardize on doubles
        rec=create_struct(rec,ek[j],key)
     endfor
     if n_elements(all) eq 0 then all=[rec] else all=[all,rec]
  endfor 
  
  if n_elements(all) eq 0 then begin 
     cnt=0
     return,-1
  endif 
  
  ;; Group into sets with matching objects first
  uniq_objs=uniq(all.OBJECT,sort(all.OBJECT))
  for i=0,n_elements(uniq_objs)-1 do begin 
     object=all[uniq_objs[i]].OBJECT
     wh_obj=where(all.OBJECT eq object)
     if n_elements(sets) eq 0 then sets=ptr_new(wh_obj) else $
        sets=[sets,ptr_new(wh_obj)]
  endfor 
  
  ;; Match AOR, if required
  if keyword_set(match_aor) then begin 
     for i=0,n_elements(sets)-1 do begin 
        wh_obj=*sets[i]
        uniq_aors=uniq(all[wh_obj].AOR,sort(all[wh_obj].AOR))
        for j=0,n_elements(uniq_aors)-1 do begin 
           aor=all[wh_obj[uniq_aors[j]]].AOR
           wh_aor=wh_obj[where(all[wh_obj].AOR eq aor)]
           if n_elements(aor_sets) eq 0 then aor_sets=ptr_new(wh_aor) else $
              aor_sets=[aor_sets,ptr_new(wh_aor)]
        endfor 
     endfor 
     ptr_free,sets
     sets=aor_sets
  endif 
  
  ;; Match FOVID or MODULE, depending on MODULE_ONLY keyword setting
  if keyword_set(mo) then match=4 else match=3
  for i=0,n_elements(sets)-1 do begin 
     wh=*sets[i]
     uniq_fov_mod=uniq(all[wh].(match),sort(all[wh].(match)))
     for j=0,n_elements(uniq_fov_mod)-1 do begin 
        fov_mod=all[wh[uniq_fov_mod[j]]].(match)
        wh_fov_mod=wh[where(all[wh].(match) eq fov_mod)]
        if n_elements(fov_sets) eq 0 then fov_sets=ptr_new(wh_fov_mod) else $
           fov_sets=[fov_sets,ptr_new(wh_fov_mod)]
        endfor 
  endfor 
  ptr_free,sets
  sets=fov_sets
  
  if keyword_set(rr) then begin 
     ;; Just return the full records
     for i=0,n_elements(sets)-1 do $
        if n_elements(recs) eq 0 then recs=ptr_new(all[*sets[i]]) else $
           recs=[recs,ptr_new(all[*sets[i]])]
  endif else begin      
     ;; Condense the information
     for i=0,n_elements(sets)-1 do begin   
        these=all[*sets[i]]
        this=these[0]
        if keyword_set(mo) then begin 
           rec={MODULE:this.module, $
                CNT: n_elements(these), $
                OBJECT:this.object, $
                FILES:ptr_new(these.file)}
        endif else begin 
           slitname=irs_fov(this.fov,/SLIT_NAME,MODULE=md,ORDER=ord)
           ;; A fov we don't recognize
           if size(slitname,/TYPE) ne 7 then continue
           rec={AOR:this.AOR, $
                CNT:n_elements(these), $
                MODULE: md, $
                FOVNAME:slitname, $
                OBJECT:this.OBJECT,$
                FILES:ptr_new(these.file), $
                STEPS:this.STEPS, $
                TIME:this.TIME, $
                NCYCLES:this.NCYCLES}
        endelse 
        if n_elements(recs) eq 0 then recs=rec else recs=[recs,rec]
     endfor 
  endelse 
  ptr_free,sets
  cnt=n_elements(recs) 
  if cnt eq 0 then return,-1
  return,recs
end
