pro xpopdiag_kill,id
  widget_control, id, get_uvalue=uv
  if uv.Free then ptr_free,uv.ptr
end

pro xpopdiag_event, ev
   evtype=tag_names(ev,/structure_name)
   if evtype eq 'WIDGET_BUTTON' then begin
      widget_control, ev.top,/DESTROY
      return
   endif 
   if evtype eq 'WIDGET_LIST' then return
   widget_control, ev.top, get_uvalue=uv
   top=widget_info(uv.wText,/LIST_TOP) 
   widget_control, uv.wText, set_value=*uv.Ptr[ev.index],SET_LIST_TOP=top
 end

pro xpopdiag, poplist, ptrlist, PARENT_GROUP=parent, _EXTRA=e, DEFAULT=def,  $
              LABEL=lbl,TOP_LABEL=tlbl,BUTTON_TEXT=bv, MODAL=md,TSIZE=ts, $
              FREE_POINTERS=fp
   if n_elements(ts) eq 0 then ts=[80,15]
   if n_elements(bv) eq 0 then bv='  OK  '
   if n_elements(def) eq 0 then def=0
   if keyword_set(parent) then begin 
      base=widget_base(/COLUMN,MODAL=md,GROUP_LEADER=parent,_EXTRA=e, $
                       /BASE_ALIGN_CENTER,SPACE=1)
   endif else begin 
      sc=get_screen_size()
      base=widget_base(/COLUMN,_EXTRA=e,/BASE_ALIGN_CENTER,  $
                       XOFFSET=sc[0]/2-20, YOFFSET=sc[1]/2-20)
   endelse 
   
   if n_elements(tlbl) ne 0 then l=widget_label(base,value=tlbl)
   if n_elements(lbl) ne 0 then begin 
      b2=widget_base(base,/ROW)
      l=widget_label(b2,value=lbl)
   endif else b2=base
   list=widget_droplist(b2,value=poplist)
   text=widget_list(base,value=*ptrlist[def],/FRAME,XSIZE=TS[0],YSIZE=TS[1])
   button=widget_button(base,value=bv)
   widget_control, list, set_droplist_select=def
   widget_control, base,set_uvalue={wText:text,Ptr:ptrlist, $
                                    Free:keyword_set(fp)},/REALIZE
   XManager, 'xpopdiag', base,CLEANUP='xpopdiag_kill',/NO_BLOCK
end

