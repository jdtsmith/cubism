
pro xf_event,ev
   widget_control,ev.id,get_uvalue=eventval
   ; get the fileptr
   widget_control,ev.top,get_uvalue=fileptr
   case eventval of
      "file": $
       begin 
         widget_control, ev.id, get_value=filt ;get the current filter
         *fileptr=*ev.file
         ptr_free,ev.file
         widget_control, ev.top, /DESTROY 
      end
      "cancel": $
       begin 
         *fileptr=-1
         widget_control, ev.top, /DESTROY 
      end
   endcase
end
   
;; see cw_xf for keywords-- note that FVAL is explicitly passed
pro xf,file, _EXTRA=e, TITLE=t, PARENT_GROUP=xfp,GROUP=group, $
       xoffset=xoff, yoffset=yoff,CANCEL_TEXT=cancel,MODAL=md

   if n_elements(xoff) eq 0 then begin
      device,get_screen_size=sc
      xoff=sc(0)/3
   endif 

   if n_elements(yoff) eq 0 then begin 
      if n_elements(sc) eq 0 then device,get_screen_size=sc
      yoff=sc(1)/5
   endif 
   
   if n_elements(md) eq 0 then md=0
   if n_elements(t) eq 0 then t='Select File'
   if n_elements(cancel) eq 0 then cancel="CANCEL"
   fileptr=ptr_new(-1)
   if keyword_set(xfp) then begin 
      if widget_info(xfp,/VALID) then begin 
         g=widget_info(xfp,/GEOMETRY) 
         xoff=g.xoffset+g.scr_xsize/4
         yoff=g.yoffset+g.scr_ysize/4
         base=widget_base(TITLE=t,/COLUMN,uvalue=fileptr,xoffset=xoff, $
                          yoffset=yoff,GROUP_LEADER=xfp,/FLOATING, $
                          MODAL=md)
      endif else md=0           ;no topbase? can't be modal
   endif else md=0
   if n_elements(base) eq 0 then $
    base=widget_base(TITLE = t,/COLUMN,uvalue=fileptr,xoffset=xoff, $
                     yoffset=yoff,MODAL=md)

   fileloader=cw_xf(base,UVALUE='file',_EXTRA=e)
   
   button=widget_button(base,value=cancel,uvalue="cancel")
   
   if keyword_set(xfp) then $
      widget_control, base, /REALIZE, CANCEL_BUTTON=button $
   else widget_control, base, /REALIZE
   
   XManager,'xf', base,GROUP=group,NO_BLOCK=0
   ;; get the value of the filename, and put into file
   file=*fileptr
   ptr_free,fileptr
end
