pro popup_event,ev
   widget_control, ev.top, get_uvalue=state
   case ev.id of
      state.popup: *(state.index)=ev.index
      state.ok: widget_control, ev.top, /DESTROY
   endcase 
end

function popup, text, list, TITLE=ttl, PARENT_GROUP=parent, MODAL=md, $
                SELECT=sel,_EXTRA=e
   if n_elements(ttl) eq 0 then ttl='Choose One'
   if n_elements(md) eq 0 then md=0
   if n_elements(sel) ne 0 then begin 
      sel=0>sel<n_elements(list)
   endif else sel=0
   
   if keyword_set(parent) then begin 
      base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                       /BASE_ALIGN_CENTER,TITLE=ttl,SPACE=1)
   endif else begin 
      sc=get_screen_size()
      base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                       XOFFSET=sc[0]/2-20, YOFFSET=sc[1]/2-20, TITLE=ttl)
   endelse 

   b1=widget_base(base,/ROW)
   nul=widget_label(b1,value=text)
   
   state={popup:0L, OK:0L, index:ptr_new(sel)}
   state.popup=widget_droplist(b1,value=list)
   widget_control,state.popup,SET_DROPLIST_SELECT=sel
   state.ok=widget_button(base,value='  OK  ')
   widget_control, base, /REALIZE, set_uvalue=state
   XManager, 'popup', base
   val=list[*state.index]
   ptr_free,state.index
   return,val
end
