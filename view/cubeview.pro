pro cubeview_cleanup,id
  widget_control, id, get_uvalue=objs
  obj_destroy,objs
end

pro cubeview_event,ev
  ;; Only quit.
  widget_control, ev.top, /DESTROY
end

pro cubeview,SIZE=sz,BLOCK=bl,TITLE=ttl,RECORD=rec,XNAME=xn, $
             ODRAW=odraw,CUBE=cube,_EXTRA=e
  if n_elements(sz) eq 0 then sz=[384L,384L]
  if n_elements(sz) eq 1 then sz=[sz,sz]
  if n_elements(xn) eq 0 then xn='CubeView'
  if n_elements(ttl) eq 0 then ttl='CubeView'

  base=widget_base(/COLUMN,SPACE=1,/BASE_ALIGN_CENTER,TITLE=ttl,MBAR=mbar)
  
  ;; a base for all our buttons, at top
  sbase=widget_base(base,/COLUMN,/ALIGN_LEFT)
  
  ;; a color bar canvas, for later use by the color tool
  cbar=widget_draw(base,xsize=sz[0],ysize=sz[1]/12,/FRAME,_EXTRA=e)
  
  ;; the tvDraw object and draw canvas, and message-passer extraordinaire
  oDraw=obj_new('tvDraw',base,TVD_XSIZE=sz[0],TVD_YSIZE=sz[1], $
                /INVISIBLE_BASE,_EXTRA=e)

  ;; Menus
  file_menu=widget_button(mbar,value="File",/MENU) 
  ;;  "Print..." tool in file menu (uses David Fanning's excellent
  ;; `FSC_PSConfig' to configure the print job)
  tvP=obj_new('tvPrint',oDraw,file_menu,_EXTRA=e)
  bquit=widget_button(file_menu,value="Quit") 
  option_menu=widget_button(mbar,value="Options",/MENU) 
  tool_menu=widget_button(mbar,value="Tools",/MENU)
  
   ;; a display line.
  line=obj_new('cvLine',base,oDraw,_EXTRA=e) 
  
  ;;************ Setup the Exclusive Tools (with icons!) **************
  ;; a color stretcher, with colorbar drawn in cbar, color table
  ;; selector, reserve colors, protected internal colors, and
  ;; mouse-mode (which makes it an exclusive plug-in!)
  stretcher=obj_new('tvColor',base,oDraw,CBAR=cbar,/PROTECT, $
                    /RESERVE,COL_TABLE_MENU=option_menu,/MOUSE_MODE,$
                    USE_COLORMAPS=[0,1,3,4,6,8,13,32],_EXTRA=e, $
                    TOP=!D.TABLE_SIZE-6)
  
  ;; A slicer object, cyan
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

  ;; a Cube record tool for extracting and stacking cubes
  cuberec=obj_new('CubeRec',base,oDraw,CUBE=cube, $
                  COLOR=stretcher->GetColor('Magenta'))
  ;; make sure the display line gets updates
  cuberec->MsgSignup,line,/CUBEREC_UPDATE
  
  ;; a pixel table tool (non-exclusive)
  pxtbl=obj_new('tvPixTbl',base,odraw,_EXTRA=e)

  ;;**********************************************************************
  exc_list=replicate({Obj:obj_new(), keys:'',Exclusive:1b},7)
  exc_list.Obj= [zoomer,hist,stretcher,slicer,stats,phot,cuberec] 
  exc_list.keys=['z',   'h', 'c',      'l',   's',  'p', 'r'    ]

  tog_list=replicate({Obj:obj_new(), keys:'',Exclusive:0b},1)
  tog_list.Obj= [pxtbl]
  tog_list.keys=['t']

  ;; a switcher for switching among the tools using icons or keypresses
  switcher=obj_new('tvSwitcher',sbase,oDraw,MsgList=[exc_list, tog_list], $
                   TOOL_MENU=tool_menu,_EXTRA=e)
  
  ;; put the tvD into the uvalue, to destroy on cleanup.
  widget_control,base,SET_UVALUE=oDraw,/REALIZE
  
  ;;now that we're up and running, start everything up
  oDraw->Start
  widget_control,base,/UPDATE   ;make sure everything's in place

  XManager,xn,base,NO_BLOCK=(keyword_set(bl) eq 0), $
           EVENT_HANDLER='cubeview_event', CLEANUP='cubeview_cleanup'
end
