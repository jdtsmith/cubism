pro popup_event,ev
   widget_control, ev.top, get_uvalue=state
   
   if ev.id eq state.popup then begin 
      *state.index=ev.index
   endif else begin 
      if widget_info(state.entry,/VALID_ID) then begin 
         widget_control, state.entry,GET_VALUE=val
         *state.text=val[0]
      endif 
      widget_control, ev.top, /DESTROY
   endelse 
end

function popup, label, list, TITLE=ttl, PARENT_GROUP=parent, MODAL=md, $
                SELECT=sel,COMBOBOX=cb,ENTRY=entry,_EXTRA=e
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
   nul=widget_label(b1,value=label)
   
   state={popup:0L, entry:0L, OK:0L, index:ptr_new(sel), $
          text:ptr_new(/ALLOCATE_HEAP)}
   
   if keyword_set(cb) then $
      state.popup=widget_combobox(b1,value=list) $
   else state.popup=widget_droplist(b1,value=list)
   
   if keyword_set(entry)  ne 0 then begin 
      lab=widget_label(b1,VALUE=entry)
      state.entry=widget_text(b1,/EDITABLE)
   endif 
   
   widget_control,state.popup,SET_DROPLIST_SELECT=sel
   state.ok=widget_button(base,value='  OK  ')
   widget_control, base, /REALIZE, set_uvalue=state
   XManager, 'popup', base
   if keyword_set(entry) then $
      val={text:*state.text, select:list[*state.index]} $
   else val=list[*state.index]
   ptr_free,state.index,state.text
   return,val
end
