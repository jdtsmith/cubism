; get proportional fonts as default for displaying in widgets...
function sm_get_fonts, BOLD=b,SIZE=sz,ITALIC=i
  if keyword_set(b) then b='bold' else b='medium'
  if keyword_set(i) then i='i' else i='r'
  if NOT keyword_set(sz) then sz=12
  switch !VERSION.OS_FAMILY of
     'unix': $
        begin
        device,get_fontnames=df,font='*cour*'+b+'-'+i+'-normal*--'+ $
               strtrim(sz,2)+'*'
        dispfont=df[0] 
        break
     end

     'Macintosh':      
     'Windows': $
        begin
        windows: 
        dispfont='COURIER'
        if keyword_set(b) then dispfont=dispfont+'*BOLD'
        if keyword_set(i) then dispfont=dispfont+'*ITALIC'
        if keyword_set(sz) then dispfont=dispfont+'*'+strtrim(sz,2)
     end
     
     else: message, !VERSION.OS_FAMILY+' not supported.'
  endswitch
  return,dispfont
end
