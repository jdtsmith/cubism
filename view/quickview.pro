pro quickview_cleanup,id
  widget_control, id, get_uvalue=objs
  obj_destroy,objs
end

pro quickview_event,ev
  if tag_names(ev,/STRUCTURE_NAME) eq 'WIDGET_TRACKING' then begin 
     if ev.enter then begin 
        widget_control, ev.id, get_uvalue=oDraw
        oDraw->Focus
     endif 
  endif else widget_control, ev.top, /DESTROY ;quit
end

pro quickview,im,SIZE=sz,BLOCK=bl,TITLE=ttl,XNAME=xn,ODRAW=odraw,MODULE=md, $
              _EXTRA=e
  
  if n_elements(sz) eq 0 then sz=[384L,384L]
  if n_elements(sz) eq 1 then sz=[sz,sz]
  if n_elements(xn) eq 0 then xn='Quickview'
  if n_elements(ttl) eq 0 then ttl='Quickview'
  nd=size(im,/N_DIMENSIONS)
  if nd lt 2 or nd gt 3 then message,'Data must be 2-d or 3-d',_EXTRA=e
   
  base=widget_base(/COLUMN,SPACE=1,/BASE_ALIGN_CENTER,TITLE=ttl,MBAR=mbar, $
                   /TRACKING_EVENTS)
  
  ;; a base for all our buttons, at top
  sbase=widget_base(base,/COLUMN,/ALIGN_LEFT)
  
  ;; a color bar canvas, for later use by the color tool
  cbar=widget_draw(base,xsize=sz[0],ysize=sz[1]/10,/FRAME,_EXTRA=e)
  
  ;; the tvDraw object and draw canvas, and message-passer extraordinaire
  oDraw=obj_new('tvDraw',base,TVD_XSIZE=sz[0],TVD_YSIZE=sz[1],_EXTRA=e)

  ;; Menus
  file_menu=widget_button(mbar,value="File",/MENU) 
  ;;  "Print..." tool in file menu (uses David Fanning's excellent
  ;; `FSC_PSConfig' to configure the print job)
  tmp=obj_new('tvPrint',oDraw,file_menu,_EXTRA=e)
;  tmp=obj_new('tvExportImage',oDraw,file_menu,_EXTRA=e)
  
  option_menu=widget_button(mbar,value="Options",/MENU) 
  tool_menu=widget_button(mbar,value="Tools",/MENU)
  
   ;; a display line.
  if n_elements(md) eq 0 then line=obj_new('tvLine',base,oDraw,_EXTRA=e) $
  else line=obj_new('cvLine',base,oDraw,MODULE=md,_EXTRA=e)
  
  ;; A plane picker, if we need one
  if nd eq 3 then begin 
     pbase=widget_base(base,/COLUMN,/FRAME) 
     pick=obj_new('tvpSelect',pbase,oDraw,im,START=0)
  endif 
  
  ;;************ Setup the Exclusive Tools (with icons!) **************
  ;; a color stretcher, with colorbar drawn in cbar, color table
  ;; selector, reserve colors, protected internal colors, and
  ;; mouse-mode (which makes it an exclusive plug-in!)
  stretcher=obj_new('tvColor',base,oDraw,CBAR=cbar,/PROTECT, $
                    /RESERVE,COL_TABLE_MENU=option_menu,/MOUSE_MODE,$
                    USE_COLORMAPS=[0,1,3,4,6,8,13,32],_EXTRA=e)
  
  ;; a slicer object, cyan
  slicer=obj_new('tvSlice',oDraw,COLOR=stretcher->GetColor('Cyan'))
  
  ;; a histogram tool, red, with stretcher as its color object.
  hist=obj_new('tvHist',oDraw,COLOR=stretcher->GetColor('Red'), $
               SCALING_MENU=option_menu, _EXTRA=e)
  
  ;; a zoom-in tool, green
  zoomer=obj_new('tvZoom',oDraw,COLOR=stretcher->GetColor('Green'),_EXTRA=e)
  
  ;; a box statistics tool, yellow
  stats=obj_new('tvStats',base,oDraw,COLOR=stretcher->GetColor('Yellow'), $
                HANDLE=2,_EXTRA=e)
  
  ;; a photometry tool, blue
  phot=obj_new('tvPhot',base,oDraw,COLOR=stretcher->GetColor('Blue'), $
               /HIDE,_EXTRA=e)

  ;; a pixel table tool (non-exclusive)
  pxtbl=obj_new('tvPixTbl',base,oDraw,_EXTRA=e)
  
  ;;**********************************************************************
  exc_list=replicate({Obj:obj_new(), keys:'',Exclusive:1b},6)
  exc_list.Obj=[zoomer,hist,stretcher,slicer,stats,phot]
  exc_list.keys=['z',   'h','c',      'l',   's',  'p']

  tog_list=replicate({Obj:obj_new(), keys:'',Exclusive:0b},1)
  tog_list.Obj= [pxtbl]
  tog_list.keys=['t']

  ;; a switcher for switching among the tools using icons or keypresses
  switcher=obj_new('tvSwitcher',sbase,oDraw,MsgList=[exc_list, tog_list], $
                   TOOL_MENU=tool_menu,_EXTRA=e)
  
  bquit=widget_button(file_menu,value="Quit") 
  
  ;; put the tvD into the uvalue, to destroy on cleanup.
  widget_control,base,SET_UVALUE=oDraw,/REALIZE
  
  ;;now that we're up and running, start everything up
  oDraw->Start
  oDraw->SetProperty,IMORIG=im[*,*,0]
  widget_control,base,/UPDATE   ;make sure everything's in place

  XManager,xn,base,NO_BLOCK=(keyword_set(bl) eq 0), $
           EVENT_HANDLER='quickview_event', CLEANUP='quickview_cleanup'
end
