;+
; NAME:  
;
;    CUBEVIEW
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Generic image/cube visualization tool for IRS spectra and
;    spectral cubes, based on tvTools, plus custom components.
;    
; CATEGORY:
;
;    CUBISM, Image visualization.
;    	
; CALLING SEQUENCE:
;
;    cubeview,[SIZE=,/BLOCK,TITLE=,RECORD=,XNAME=,ODRAW=,CUBE=]
;			
; INPUT KEYWORD PARAMETERS:
;
;    SIZE: The one or two element initial pixel size of the draw
;       window.  Defaults to 384 pixels.
;
;    BLOCK: If set, run as a blocking widget.
;
;    TITLE: The title to give the cubeview top level widget.
;
;    XNAME: The name to register under.
;
;    CUBE: The cube to sign up initially for communication with
;       CubeRec.
;
; OUTPUT KEYWORD PARAMETERS:
;
;    RECORD: The output CubeRec object which will handle communication
;       with CubeProj, etc.
;
;    ODRAW: The tvDraw object which handles all image display and
;       event handling.
;
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2005 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################
pro cubeview_cleanup,id
  widget_control, id, get_uvalue=kill
  obj_destroy,kill
end

pro cubeview_event,ev
  if widget_info(ev.id,/UNAME) eq "quit" then begin 
     cubeview_cleanup,ev.id
     return
  endif
  
  ;; Just hand off the rest of our events to tvDraw
  tvDraw_Event,ev
end

pro cubeview,SIZE=sz,BLOCK=bl,TITLE=ttl,RECORD=cuberec,XNAME=xn, $
             ODRAW=odraw,CUBE=cube,KILL_CUBE=kc,_EXTRA=e
  if n_elements(sz) eq 0 then sz=[384L,384L]
  if n_elements(sz) eq 1 then sz=[sz,sz]
  if n_elements(xn) eq 0 then xn='CubeView'
  if n_elements(ttl) eq 0 then ttl='CubeView'

  base=widget_base(/COLUMN,SPACE=1,/BASE_ALIGN_CENTER,TITLE=ttl,MBAR=mbar, $
                   /TRACKING_EVENTS)
  
  ;; A base for all our buttons, at top
  sbase=widget_base(base,/COLUMN,/ALIGN_LEFT)
  
  ;; A color bar canvas, for later use by the color tool
  cbar=widget_draw(base,xsize=sz[0],ysize=(sz[1]/10)>30,/FRAME,_EXTRA=e)
  
  ;; The tvDraw object and draw canvas, and message-passer extraordinaire
  oDraw=obj_new('tvDraw',base,TVD_XSIZE=sz[0],TVD_YSIZE=sz[1], $
                /INVISIBLE_BASE,_EXTRA=e)

  ;; Menus (many tools add their own menu items below)
  file_menu=widget_button(mbar,value="File",/MENU) 
  tmp=obj_new('tvExportImage',oDraw,file_menu,_EXTRA=e)
  
  option_menu=widget_button(mbar,value="Options",/MENU) 
  tool_menu=widget_button(mbar,value="Tools",/MENU)
  
  ;; Display line.
  line=obj_new('cvLine',oDraw,base,_EXTRA=e) 
  
  ;;************ Setup the Exclusive Tools (with icons!) **************
  ;; a color stretcher, with colorbar drawn in cbar, color table
  ;; selector, reserve colors, protected internal colors, and
  ;; mouse-mode (which makes it an exclusive plug-in!)
  stretcher=obj_new('tvColor',oDraw,base,CBAR=cbar,/PROTECT, $
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
  stats=obj_new('tvStats',oDraw,base,COLOR=stretcher->GetColor('Yellow'), $
                HANDLE=2,_EXTRA=e)
  
  ;; a photometry tool, blue: adds to base
  phot=obj_new('tvPhot',oDraw,base,COLOR=stretcher->GetColor('Blue'), $
               /HIDE,_EXTRA=e)

  ;; a Cube record tool for extracting and stacking cubes: adds to base
  ;; and creates aperture and visualize tools as well
  cuberec=obj_new('CubeRec',oDraw,base,CUBE=cube,APER_OBJECT=aper, $
                  COLOR=stretcher->GetColor('Magenta'), $
                  VISUALIZE_OBJECT=irsmapviz, $
                  VISUALIZE_COLOR=stretcher->GetColor(['goldenrod', $
                                                       'charcoal','white']), $
                  MENU=file_menu,_EXTRA=e)
  
    
  ;; make sure the display line gets CubeRec updates
  cuberec->MsgSignup,line,/CUBEREC_UPDATE
  
  ;; a cube backtrack tool, green, with updates from cuberec
  cubeback=obj_new('CubeBackTrack',oDraw,base, $
                   COLOR=stretcher->GetColor('Green'),_EXTRA=e)
  cuberec->MsgSignup,cubeback,/CUBEREC_UPDATE,/CUBEREC_FULL
  
  ;; a bad pixel selector tool, with updates from cuberec
  cubebadpix=obj_new('CubeBadPix',oDraw,base,cuberec,_EXTRA=e)
  cuberec->MsgSignup,cubebadpix,/CUBEREC_UPDATE
    
  ;; a pixel table tool (non-exclusive)
  pixtbl=obj_new('tvPixTbl',oDraw,base,_EXTRA=e)
  
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
  switcher=obj_new('tvSwitcher',oDraw,sbase,MsgList=[exc_list, tog_list], $
                   TOOL_MENU=tool_menu,_EXTRA=e)
  
  ;; Various other buttons
  resize=obj_new('tvResize',oDraw,SIZE_MENU=option_menu)
  
  kill=[oDraw]
  if keyword_set(kc) then kill=[kill,cube]

  bquit=widget_button(file_menu,value="Close",UNAME='quit',/SEPARATOR, $
                      KILL_NOTIFY='cubeview_cleanup',UVALUE=kill)
  
  ;; put the tvD into the uvalue, to destroy on cleanup.
  widget_control,base,SET_UVALUE=oDraw,/REALIZE
  
  ;;now that we're up and running, start everything up
  oDraw->Start
  widget_control,base,/UPDATE   ;make sure everything's in place

  XManager,xn,base,NO_BLOCK=~keyword_set(bl),EVENT_HANDLER='cubeview_event'
end
