;; Convert from decimal degrees to a print format
function radecstring,pos,RA=ra,PRECISION=pres,HMS=hms
  if n_elements(pres) eq 0 then pres=2 else pres=pres>2
  
  final_ra='%0'+strtrim(pres+3,2)+'.'+strtrim(pres,2)+'f'
  final_dec='%0'+strtrim(pres+2,2)+'.'+strtrim(pres-1,2)+'f'
  
  if keyword_set(ra) then begin 
     if keyword_set(hms) then form='(%"%2.2dh%2.2dm'+final_ra+'s")' $
     else form='(%"%2.2d:%2.2d:'+final_ra+'")'
     str=string(FORMAT=form,sixty(pos/15.0D))
  endif else begin 
     if keyword_set(hms) then form='(%"%1s%2.2dd%2.2dm'+final_dec+'s")' $
     else form='(%"%1s%2.2d:%2.2d:'+final_dec+'")'
     str=string(FORMAT=form, $
                pos lt 0.0D?"-":"+", abs(sixty(pos)))
  endelse 
  
  return,str
end
