pro multchoice_event,ev
  widget_control, ev.top,GET_UVALUE=ptr
  widget_control, ev.id,GET_UVALUE=uv
  if size(uv,/TYPE) eq 7 then begin 
     if uv eq 'cancel' then *ptr=-1
     widget_control, ev.top,/DESTROY 
     return
  endif 
  (*ptr)[uv]=1b
end


function multchoice, text,list,TITLE=ttl,PARENT_GROUP=parent, MODAL=md, $
                     SELECT=sel,NONEXCLUSIVE=nexc,_EXTRA=e
  if n_elements(ttl) eq 0 then ttl='Choose One'
  if n_elements(md) eq 0 then md=0
  if n_elements(sel) ne 0 then sel=0>sel<n_elements(list)
  if keyword_set(nexc) then exc=0 else begin 
     exc=1 & nexc=0
  endelse
  
  if keyword_set(parent) then begin 
     base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                      /BASE_ALIGN_CENTER,TITLE=ttl,SPACE=1)
  endif else begin 
     sc=get_screen_size()
     base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                      XOFFSET=sc[0]/2-20,YOFFSET=sc[1]/2-20,TITLE=ttl)
  endelse 
  
  lab=widget_label(base,value=text)
  subbase=widget_base(base,EXCLUSIVE=exc,NONEXCLUSIVE=nexc,/COLUMN,/FRAME) 
  
  p=ptr_new(bytarr(n_elements(list)))
  
  for i=0,n_elements(list)-1 do begin 
     b=widget_button(subbase,value=list[i],uvalue=i)
     if n_elements(sel) gt 0 &&  i eq sel[0] then begin 
        widget_control, b,/SET_BUTTON
        (*p)[i]=1b
     endif 
  endfor 
  row=widget_base(base,/ROW) 
  ok=widget_button(row,value='  OK  ',uvalue='ok')
  cncl=widget_button(row,value='Cancel',uvalue='cancel')
  widget_control, base,/REALIZE,SET_UVALUE=p
  XManager,'multchoice',base
  if (*p)[0] eq -1 then ret=-1 else ret=where(*p)
  ptr_free,p
  return,ret
end
