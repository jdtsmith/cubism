function cw_bitmen_event,ev
   widget_control,ev.id,get_value=value
   return, { ID:ev.handler, TOP:ev.top, HANDLER:0L, value:value }
end

function cw_bitmen_get_value,id
   stash = WIDGET_INFO(id, /CHILD) ; Retrieve the state
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   if state.num gt 0 then begin 
      widget_control, state.ids(0), get_value=ret ; Get the value 
      for i=1,state.num-1 do begin 
         widget_control, state.ids(i), get_value=tmp ; Get the value 
         ret=[ret,tmp]          ;put it in an array
      endfor 
   endif  
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
   RETURN, ret
end

pro cw_bitmen_set_value,id,value
   stash=widget_info(id,/CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   nv=n_elements(value)
   ;;--- Set the number actually displayed (no greater than state.lim)
   state.num=nv < state.lim
   ;;--- set buttons' values
   for i=0,state.num-1 do begin
      widget_control, state.ids(i), set_value=value(i)
   endfor 
   ;;--- empty the rest
   for i=state.num,state.lim-1 do begin
      widget_control, state.ids(i), set_value=''
   endfor 
   ;;--- restore state
   widget_control, stash, set_uvalue=state, /NO_COPY  
   return
end 

function cw_bitmen,parent,bitmap,list,lim,UVALUE=uval,XSIZE=xs,YSIZE=ys,IDS=ids
   IF (N_ELEMENTS(uval) EQ 0)  THEN uval = 0L
   nl=n_elements(list)
   ids=lonarr(lim)
   base=widget_base(parent,EVENT_FUNC='cw_bitmen_event', $
                    FUNC_GET_VALUE='cw_bitmen_get_value',$
                    PRO_SET_VALUE='cw_bitmen_set_value',UVALUE=uval)
   ;;--- create the bitmap base button
   if n_elements(xs) ne 0 and n_elements(ys) ne 0 then  $
    butbase=widget_button(base,VALUE=bitmap,MENU=1,$
                          xsize=xs,ysize=ys) $
   else $
        butbase=widget_button(base,VALUE=bitmap,MENU=1)
   
   for i=0,nl-1 do begin
      ids(i)=widget_button(butbase,value=list(i),/DYNAMIC_RESIZE)
   endfor 
   for i=nl,lim-1 do begin
      ids(i)=widget_button(butbase,value='',/DYNAMIC_RESIZE)
   endfor 
   state={ids:ids,lim:lim,num:nl}
   vid=widget_info(ids(0),/valid_id)
   if vid ne 1 then print,ids(0),'is not a valid id'
   ;;--- stash state in first child
   widget_control,butbase,SET_UVALUE=state,/NO_COPY
   return,base
end
