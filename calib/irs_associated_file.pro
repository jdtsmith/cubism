;; Convert an IRS data filename to an existing bmask/uncertainty/bcd file
function irs_associated_file,file,BMASK=bm,UNCERTAINTY=unc
  if keyword_set(bm) then reg='bmask\.fits$' else $
     if keyword_set(unc) then reg='func\.fits$' else $
        reg='bcd(_fp)?\.fits$'
  if stregex(file,reg,/BOOLEAN) then return,file
  parts=stregex(file,'^(.*[0-9][._])[^.]+\.fits$',/EXTRACT,/SUBEXPR)
  base=parts[1]
  if strlen(base) eq 0 then return,-1
  if keyword_set(bm) then filt='bmask' else $
     if keyword_set(unc) then filt='func' else filt='bcd*'
  ; if stregex(base,'(^|/)IRSX',/BOOLEAN) then begin 
;      ;; Remove all the versioning info from pipeline-named files.
;      for i=0,3 do begin 
;         pos=strpos(base,'.',/REVERSE_SEARCH)
;         base=strmid(base,0,pos)
;      endfor 
;      base+='*'
;   endif 
  f=file_search(base+filt+'.fits',COUNT=cnt)
  if cnt eq 0 then return,-1
  return,f[0]
end
