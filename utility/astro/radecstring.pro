;; Convert from decimal degrees to a print format
;; IDL doesn't have a %05.2f equivalent.
function radecstring,pos,RA=ra
  if keyword_set(ra) then $
     str=string(FORMAT='(%"%2.2d:%2.2d:%5.2f")',sixty(pos/15.0D)) $
  else str=string(FORMAT='(%"%1s%2.2d:%2.2d:%4.1f")',pos lt 0.0D?"-":"+", $
                  abs(sixty(pos)))
  p=0
  while p ne -1 do begin 
     p=strpos(str,': ',p) 
     if p ne -1 then str=strmid(str,0,p)+':0'+strmid(str,p+2)
  endwhile 
  return,str
end
