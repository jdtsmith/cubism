
function file_size_string,sz
  pref=['B','KB','MB','GB']
  type=0
  size=float(sz)
  while size gt 1000. do begin 
     type++
     size/=1024.
     if type eq 3 then break
  endwhile 
  return,string(FORMAT='('+(type lt 2?'I0':'F0.1')+',A)',size,pref[type])
end
