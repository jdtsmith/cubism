;; Read through headers of passed files and collect into matching
;; spectral records.  Pointers are created for the FILES structure
;; member, and must be freed by the caller.  MAP requires matching
;; maps (same AOR, FOV, and OBJECT), whereas normally just
;; OBJECT+MODULE matching is required.
function irs_file_filter,files,COUNT=cnt,MODULE_ONLY=mo
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
     rec={FILE:files[i], $
          AOR:long(sxpar(hdr,'AORKEY')), $
          OBJECT:strtrim(sxpar(hdr,'OBJECT'),2), $
          FOV:fov, $
          MODULE:md, $
          NCYCLES:long(sxpar(hdr,'NCYCLES')), $
          TIME:float(sxpar(hdr,'EXPTOT_T')), $
          STEPS:long([sxpar(hdr,'STEPSPAR'),sxpar(hdr,'STEPSPER')])}
     if n_elements(all) eq 0 then all=[rec] else all=[all,rec]
  endfor 
  
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
  
  ;; Match FOVID or MODULE
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
  
  ;; Collect and return
  for i=0,n_elements(sets)-1 do begin   
     these=all[*sets[i]]
     this=these[0]
     if keyword_set(mo) then begin 
        rec={MODULE:this.module, $
             CNT: n_elements(these), $
             OBJECT:this.object, $
             FILES:ptr_new(these.file)}
     endif else begin 
        slitname=irs_fov(this.fov,/SLIT_NAME_ONLY,MODULE=md,ORDER=ord)
        ;; A file we don't recognize
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
  ptr_free,sets
  cnt=n_elements(recs) 
  if cnt eq 0 then return,-1
  return,recs
end
