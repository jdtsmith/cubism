;; load with loadct, but set the !ctabl system variable too.
pro cu_loadct, table_number, NO_RESET=nr, _REF_EXTRA=e
  sm_ctdef
  if keyword_set(nr) then table_number=!ctabl.cmap
  if n_elements(table_number) ne 0 then begin
     loadct,table_number,_EXTRA=e
     !ctabl.cmap=table_number
     if NOT keyword_set(nr) then begin 
        !ctabl.high=1.
        !ctabl.low=0.
        !ctabl.gamma=1.
     endif 
  endif else loadct,_EXTRA=e
end


