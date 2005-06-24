pro cubeview_cleanup,id
  widget_control, id, get_uvalue=objs
  obj_destroy,objs
end

pro cubeview_event,ev
  widget_control, ev.id,GET_UVALUE=uv
  if size(uv,/TYPE) eq 7 && uv eq "quit" then begin 
     widget_control, ev.top, /DESTROY ;quit
     return
  endif
  
  ;; Just hand off the rest of our events to tvDraw
  tvDraw_Event,ev
end

pro cubeview,SIZE=sz,BLOCK=bl,TITLE=ttl,RECORD=cuberec,XNAME=xn, $
             ODRAW=odraw,CUBE=cube,_EXTRA=e
  if n_elements(sz) eq 0 then sz=[384L,384L]
  if n_elements(sz) eq 1 then sz=[sz,sz]
  if n_elements(xn) eq 0 then xn='CubeView'
  if n_elements(ttl) eq 0 then ttl='CubeView'

  base=widget_base(/COLUMN,SPACE=1,/BASE_ALIGN_CENTER,TITLE=ttl,MBAR=mbar, $
                   /TRACKING_EVENTS)
  
  ;; a base for all our buttons, at top
  sbase=widget_base(base,/COLUMN,/ALIGN_LEFT)
  
  ;; a color bar canvas, for later use by the color tool
  cbar=widget_draw(base,xsize=sz[0],ysize=sz[1]/10,/FRAME,_EXTRA=e)
  
  ;; the tvDraw object and draw canvas, and message-passer extraordinaire
  oDraw=obj_new('tvDraw',base,TVD_XSIZE=sz[0],TVD_YSIZE=sz[1], $
                /INVISIBLE_BASE,_EXTRA=e)

  ;; Menus
  file_menu=widget_button(mbar,value="File",/MENU) 
  ;;  "Print..." tool in file menu (uses David Fanning's excellent
  ;; `FSC_PSConfig' to configure the print job)
;  tmp=obj_new('tvPrint',oDraw,file_menu,_EXTRA=e)
  tmp=obj_new('tvExportImage',oDraw,file_menu,_EXTRA=e)
;  tmp=obj_new('tvExportImage',oDraw,file_menu,_EXTRA=e)
  
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
                    USE_COLORMAPS=[0,1,3,4,6,8,13,32],TOP=!D.TABLE_SIZE-6, $
                    _EXTRA=e)
  
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
  
  ;; a photometry tool, blue: adds to base
  phot=obj_new('tvPhot',base,oDraw,COLOR=stretcher->GetColor('Blue'), $
               /HIDE,_EXTRA=e)

  ;; a Cube record tool for extracting and stacking cubes: adds to base
  ;; and creates aperture and visualize tools as well
  cuberec=obj_new('CubeRec',base,oDraw,CUBE=cube,APER_OBJECT=aper, $
                  COLOR=stretcher->GetColor('Magenta'), $
                  VISUALIZE_OBJECT=irsmapviz, $
                  VISUALIZE_COLOR=stretcher->GetColor(['goldenrod', $
                                                       'charcoal','white']), $
                  MENU=file_menu,_EXTRA=e)
  
    
  ;; make sure the display line gets CubeRec updates
  cuberec->MsgSignup,line,/CUBEREC_UPDATE
  
  ;; a cube backtrack tool, green, with updates from cuberec
  cubeback=obj_new('CubeBackTrack',base,oDraw, $
                   COLOR=stretcher->GetColor('Green'),_EXTRA=e)
  cuberec->MsgSignup,cubeback,/CUBEREC_UPDATE,/CUBEREC_FULL
  
  ;; a bad pixel selector tool, with updates from cuberec
  cubebadpix=obj_new('CubeBadPix',base,oDraw,_EXTRA=e)
  cuberec->MsgSignup,cubebadpix,/CUBEREC_UPDATE
    
  ;; a pixel table tool (non-exclusive)
  pixtbl=obj_new('tvPixTbl',base,oDraw,_EXTRA=e)
  
  ;; a WavSamp masker (non-exclusive)
  wsmask=obj_new('WSMask',oDraw,_EXTRA=e)
  cuberec->MsgSignup,wsmask,/CUBEREC_UPDATE
  
  ;; a Compass rose (non-exclusive)
  rose=obj_new('CubeRose',oDraw,COLOR=stretcher->GetColor('Magenta'),_EXTRA=e)
  cuberec->MsgSignup,rose,/CUBEREC_UPDATE ;give them our message
  
  ;;**********************************************************************
  
  ;; Mutually exclusive list, to the left in toolbar
  exc_list=[{Obj:zoomer,     keys:'z',Exclusive:1b}, $
            {Obj:hist,       keys:'h',Exclusive:1b}, $
            {Obj:stretcher,  keys:'c',Exclusive:1b}, $
            {Obj:slicer,     keys:'l',Exclusive:1b}, $
            {Obj:stats,      keys:'s',Exclusive:1b}, $
            {Obj:phot,       keys:'p',Exclusive:1b}, $
            {Obj:cuberec,    keys:'x',Exclusive:1b}, $
            {Obj:cubeback,   keys:'t',Exclusive:1b}, $
            {Obj:cubebadpix, keys:'b',Exclusive:1b}, $
            {Obj:aper,       keys:'', Exclusive:1b}, $
            {Obj:irsmapviz,  keys:'v',Exclusive:1b}]
  
  ;; Toggle-able, non-exclusive list, to the right in toolbar
  tog_list=[{Obj:pixtbl,     keys:'', Exclusive:0b}, $
            {Obj:wsmask,     keys:'w',Exclusive:0b}, $
            {Obj:rose,       keys:'', Exclusive:0b}]

  ;; a switcher for switching among the tools using icons or keypresses
  switcher=obj_new('tvSwitcher',sbase,oDraw,MsgList=[exc_list, tog_list], $
                   TOOL_MENU=tool_menu,_EXTRA=e)
  
  ;; Various other buttons
  resize=obj_new('tvResize',oDraw,SIZE_MENU=option_menu)
  
  bquit=widget_button(file_menu,value="Quit",UVALUE='quit',/SEPARATOR) 
  
  ;; put the tvD into the uvalue, to destroy on cleanup.
  widget_control,base,SET_UVALUE=oDraw,/REALIZE
  
  ;;now that we're up and running, start everything up
  oDraw->Start
  widget_control,base,/UPDATE   ;make sure everything's in place

  XManager,xn,base,NO_BLOCK=(keyword_set(bl) eq 0), $
           EVENT_HANDLER='cubeview_event', CLEANUP='cubeview_cleanup'
end
