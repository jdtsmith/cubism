pro getinp_event, ev
   widget_control,ev.id,get_uvalue=inpid
   widget_control,ev.top,get_uvalue=val_ptr
   if inpid ne 0L then begin 
      widget_control, inpid, get_value=val
      *val_ptr=val
   endif else ptr_free,val_ptr
   widget_control, ev.top,/DESTROY
   return
end


function getinp, question, DEFAULT, TITLE=t,MODAL=md,PARENT_GROUP=group, $
                 _EXTRA=e
   if n_elements(t) eq 0 then t='Enter Value'
   if n_elements(md) eq 0 then md=1
   if n_elements(default) eq 0 then default=' '
   groupQ=n_elements(group) ne 0?widget_info(group,/VALID_ID):0
   if groupQ then begin 
      g=widget_info(group,/GEOMETRY) 
      xoff=g.xoffset+g.scr_xsize/4
      yoff=g.yoffset+g.scr_ysize/4
   endif else begin 
      device,get_screen_size=sc
      xoff=sc[0]/3 & yoff=sc[1]/3
   endelse 
   val_ptr=ptr_new(DEFAULT)
   if NOT groupQ then $
      base=widget_base(TITLE=t,/COLUMN,xoffset=xoff,yoffset=yoff, $
                       uvalue=val_ptr) $
   else base=widget_base(TITLE=t,/COLUMN,xoffset=xoff,yoffset=yoff, $
                         uvalue=val_ptr,MODAL=md, GROUP=group)
   rowbase=widget_base(base,/row)
   input=cw_field(rowbase,value=DEFAULT,TITLE=question,/RETURN_EVENTS,_EXTRA=e)
   widget_control, input, set_uvalue=input ;put the id in
   rbase=widget_base(base,/row) 
   cancel=widget_button(rbase,value='Cancel', uvalue=0L)
   ok=widget_button(rbase,value=' OK ',uvalue=input)
   widget_control, base,/realize,DEFAULT_BUTTON=ok
   widget_control, input,/INPUT_FOCUS
   XManager,'getinp',base
   if NOT ptr_valid(val_ptr) then return,''
   ret=*val_ptr
   ptr_free,val_ptr
   return, ret[0]
end




