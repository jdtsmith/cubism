;; Convert from decimal degrees to a print format
function radecstring,pos,RA=ra,PRECISION=pres
  if n_elements(pres) eq 0 then pres=2 else pres=pres>2
  
  final_ra='%'+strtrim(pres+3,2)+'.'+strtrim(pres,2)+'f'
  final_dec='%'+strtrim(pres+2,2)+'.'+strtrim(pres-1,2)+'f'
  
  if keyword_set(ra) then $
     str=string(FORMAT='(%"%2.2d:%2.2d:'+final_ra+'")',sixty(pos/15.0D)) $
  else str=string(FORMAT='(%"%1s%2.2d:%2.2d:'+final_dec+'")', $
                  pos lt 0.0D?"-":"+", abs(sixty(pos)))
  
  ;; XXX switch to IDL 6.1 leading-zero formatting
  p=0
  while p ne -1 do begin 
     p=strpos(str,': ',p) 
     if p ne -1 then str=strmid(str,0,p)+':0'+strmid(str,p+2)
  endwhile 
  return,str
end
