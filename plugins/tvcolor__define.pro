;+
; NAME:  
;
;    tvColor
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    An all-purpose color manipulation plug-in, which supports
;    colormap selection from either a menu or droplist, optional
;    widget sliders for low, high and gamma modifications of the
;    colormap, and an optional mouse mode, emulating SAOImage with
;    mouse-based color stretching.
;    
; CATEGORY:
;
;    Color Manipulation, Colormaps
;    	
; SIDE EFFECTS:
;
;    Sets a system variable !ctabl:
;    {!ctabl,cmap:0,low:0.,high:1.,gamma:1.0} which keeps track of the
;    given colormap and stretch parameters.
;
; RESTRICTIONS:
;
;    On TrueColor display devices, color table changes are not
;    reflected in the graphics subsystem, but in an internal IDL
;    translation table.
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('tvColor',parent,oDraw,[/PROTECT,/MOUSE_MODE=,
;          	       /SLIDERS,START=,LOW=,HIGH=,WIDTH=,BOTTOM=,TOP=, $
;          	       RESERVE=,COL_TABLE_PARENT=,COL_TABLE_MENU=,CBARID=, $
;                      USE_COLORMAPS=])
;                
;       INPUT PARAMETERS:
;
;          parent:  The widget id of the object widget's parent.
;          
;	   oDraw: The tvDraw object.
;          
;       INPUT KEYWORD PARAMETERS:
;
;          PROTECT: If set, reinstate the colormap upon re-entry to
;             the widget.
;             
;          MOUSE_MODE: If set, enable the tvColor as an exclusive
;             object, with SAOImage-style mouse-driven color-map
;             adjustment.  The icon looks like 3 crayons.
;             
;          SLIDERS: If set, draw the three widget sliders.
;
;          START: The initial starting values of the three widget
;             sliders (bottom and top chop, and gamma).  Defaults to
;             values saved into structure !ctabl.
;
;	   MIN:	The lower limit (3 element vec) for bottom,
;	      top, and gamma, as shown on the slider.  Defaults to
;	      [0,0,1]. (Gamma=gamslider/100.)
;       
;          MAX: The upper limit (3 element vec) for bottom, top, and
;             gamma, as shown on the slider.  Defaults to
;             [100,100,500].
;       
;	   WIDTH: The width in pixels of the widget.  
;	   
;	   COL_TABLE_PARENT: The widget id of the parent into which to place
;             a colormap selection droplist, or any non_zero value to
;             use the same parent as the slider base. If zero or
;             ommitted, no colortable selector is created or used.
;	           
;          COL_TABLE_MENU: If set to a menu button widget or MBAR
;             base, the colormap selector is created as a menu, rather
;             than a droplist.
;             
;	   CBARID: The widget id of a colorbar draw widget to draw a color
;             bar into (with appropriate top and bottom colors).  The
;             colorbar is drawn with method DrawCbar, which must be
;             called *after* the colorbar widget is realized.
;	      
;          PROTECT: If this keyword is set, colors will be ;
;            reinstated on entering the draw widget.
;	     
;	   RESERVE: Load reserve colors in the colormap.  One of:
;
;             *  1: Load standard set of colors
;                (charcoal,red,green,blue, yellow) into the reserve
;                space at the top of the colormap.
;
;             *  A vector of structures of the form:
;                {color:'colname',r:0b,g:0b,b:0b} where colname is the
;                name of the color, and the r,g,b byte values are
;                [0-255].  These colors are loaded as reserve, and are
;                available by name via the method GetColor.
;	      
;	   BOTTOM: The bottom of the colormap to fill from, defaulting to 0.
;	      
;          TOP: The top of the colormap to fill to, including space
;             for reserve colors.  Defaults to
;             !D.TABLE_SIZE-1-nreserve
;                  
;          USE_COLORMAPS: A vector of indices into the RSI default
;             colormaps (try "loadct") to include in the selection.
;             
;    GetColor:
;  
;	DESCRIPTION:
;
;	   Get reserve color indices by name.
;	
;       CALLING SEQUENCE:
;
;          color=obj->GetColor(color)
;          
;       INPUT PARAMETERS:
;
;          color:  The name of the color to be retrieved.  Case is ignored.
;
;       OUTPUTS:
;
;          colind: An index into the loaded color table where the
;             requested color resides, or -1 if no color is found.
;
;    GetProperty:
;  
;	DESCRIPTION:
;
;	   Retrieve several useful properties.
;	
;       CALLING SEQUENCE:
;
;          obj->GetProperty,BAR=,NRESERVE=,MIN=,MAX=,BSIZE=,TOP=,BOTTOM=
;          
;       OUTPUT KEYWORD PARAMETERS:
;
;          BAR:  The widget ID of the drawn colorbar.
;          
;          BSIZE: The [x,y] size of the colorbar widget.
;
;          All others as documented above for Init.
;
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug-->tvColor
;
; EXAMPLE:
;
;    cobj=obj_new('tvColor',parent,oDraw,/MOUSE_MODE,/RESERVE,$
;                 USE_COLORMAP=[1,2,3])
;    
; MODIFICATION HISTORY:
; 
;    2001-09-26 (J.D. Smith): Fixed color table issue for better
;       TrueColor operation.
;
;    2001-08-07 (J.D. Smith): Initial import from SCORE-era viewer
;       component collection.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001 Cornell University
;
;  This file is part of SMART.
;
;  SMART is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  SMART is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with SMART; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################

;;**************************OverRiding methods********************************
;=============================================================================
;       Message - Override and chain to tvPlug::Message to display
;=============================================================================
pro tvColor::Message,msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_MOTION': begin 
        bright=float(msg.X)/self.winsize[0]
        contrast=float(msg.Y)/self.winsize[1]
        range=(self.max[1]-self.min[0])*contrast > 1
        center=fix(bright*(self.max-self.min)+self.min)
        low=center[0]-fix(range/2) > self.min[0]
        high=center[1]+fix(range/2) < self.max[1]
        newhigh=(float(high)-self.min[1])/(self.max[1]-self.min[1])
        newlow=(float(low)-self.min[0])/(self.max[0]-self.min[0])
        if self.high eq newhigh AND self.low eq newlow then return
        self.high=newhigh
        self.low=newlow
        if widget_info(self.wLow,/VALID_ID) then begin
           widget_control, self.wLow,SET_VALUE=low
           widget_control, self.wHigh,SET_VALUE=high
        endif
        self->Stretch
     end 
     
     'DRAW_BUTTON': begin 
        case msg.type of 
           0: begin            ;button press
              if (msg.press AND 4b) ne 0 then begin ;reset them
                 self.high=1.
                 self.low=0.
                 self.gamma=1.
                 if widget_info(self.wLow,/VALID_ID) then begin
                    widget_control, self.wLow,SET_VALUE=self.min[0]
                    widget_control, self.wHigh,SET_VALUE=self.max[1]
                    widget_control, self.wGamma,SET_VALUE=100
                 endif
                 self->Stretch
              endif else begin 
                 self.oDraw->GetProperty,WINSIZE=ws
                 self.winsize=ws
                 self.oDraw->MsgSignup,self,/DRAW_MOTION
              endelse
           end
           1: self.oDraw->MsgSignup,self,DRAW_MOTION=0
        endcase 
     end 
     
     'WIDGET_TRACKING':  if msg.enter eq 1 then begin 
        self.oDraw->SetWin 
        self->SetColors
     endif
     else:
  endcase
end

;=============================================================================
;	On:  Get all our messages.
;=============================================================================
pro tvColor::On
  self->tvPlug::On
  self.oDraw->MsgSignup,self,WIDGET_TRACKING=self.protect, $
     DRAW_BUTTON=self.mouse_mode
end

;=============================================================================
;       Off: Possibly keep getting tracking messages.
;=============================================================================
pro tvColor::Off
  self->tvPlug::Off
  self.oDraw->MsgSignup,self,/NONE,TRACKING=self.protect
end

function tvColor::Icon
  return,  [[140B, 049B],[140B, 049B],[222B, 123B],[222B, 123B], $
            [210B, 074B],[094B, 106B],[082B, 123B],[214B, 091B], $
            [222B, 106B],[090B, 091B],[086B, 075B],[082B, 123B], $
            [222B, 075B],[082B, 106B],[214B, 107B],[222B, 123B]]
end

;;*************************End OverRiding methods******************************

;=============================================================================
;	GetColor - Get colors by name.
;=============================================================================
function tvColor::GetColor, colname
  if NOT ptr_valid(self.colnames) then return,-1
  wh=where(*self.colnames eq strtrim(strlowcase(colname),2),cnt)
  if cnt ne 0 then return,self.topval+1-self.nreserve+wh[0]>0  $
  else return,(self.bottom+self.topval-self.nreserve)
end

;=============================================================================
;       GetProperty
;=============================================================================
pro tvColor::GetProperty, BAR=bar, NRESERVE=nres, MIN=min, MAX=max, BSIZE=bs, $
                          TOP=top, BOTTOM=bot
  if arg_present(nres) then nres=self.nreserve
  if arg_present(bar) then bar=self.wBar
  if arg_present(max) then max=self.max
  if arg_present(min) then min=self.min
  if arg_present(bs) then bs=self.bSize
  if arg_present(top) then top=self.topval-self.nreserve
  if arg_present(bot) then bot=self.bottom
end

;=============================================================================
;       DrawCbar - For drawing the colorbar, call *after* the draw
;                  widget for the colorbar is realized.  Draws
;                  colorbar only for given range (bottom:top) in
;                  colormap.
;=============================================================================
pro tvColor::DrawCbar, WIN=win
  if self.wBar eq 0L then return
  widget_control, self.wBar, get_value=win
  wset,win
  tv,self.bottom+bytscl(lindgen(self.bSize[0],self.bSize[1])  $
                        mod self.bSize[0],  $
                        TOP=self.topval-self.bottom-self.nreserve)
  self.oDraw->SetWin
end

;=============================================================================
;       SetColors - Reset the colors to their appropriate values.
;                   Useful upon re-entry of the widget, in case others
;                   have damaged the map.
;=============================================================================
pro tvColor::SetColors
  tvlct,*self.r,*self.g,*self.b,self.bottom
end

;=============================================================================
;       Stretch - Stretch the colors according to the internal values.
;=============================================================================
pro tvColor::Stretch
  ;;total colors in our used range
  ncolors=(self.topval-self.bottom-self.nreserve+1)
  
  ;; at low want 0, at high want 1.
  ;; i.e. indxnew[low]=m*indxold[low]+b=0.;
  ;; indxnew[high]=m*indxold[high]+b=1.
  ;; --> m=1./(indxold[high]-indxold[low])
  ;; --> b=-m*indxold[low]
  div=self.high-self.low
  if div eq 0 then m=0. else m=1./(self.high-self.low)/ncolors
  b=-m*self.low*ncolors
  index=findgen(ncolors)        ;total range or *our* color indices
  index=0.>(m*index+b)<1.       ;chop it down to specified range
  if self.gamma eq 1.0 then index=long((ncolors-1)*index) else  $
     index=long((ncolors-1)*index^self.gamma)
  ;; Index into the original colors
  (*self.r)[self.bottom:self.bottom+ncolors-1]=(*self.rorig)[index]
  (*self.g)[self.bottom:self.bottom+ncolors-1]=(*self.gorig)[index]
  (*self.b)[self.bottom:self.bottom+ncolors-1]=(*self.borig)[index]
  self->SetColors               ;set them in.
end

pro tvColor_ctabl_Event, ev
  widget_control, ev.handler,get_uvalue=self
  self->CtablEvent,ev
end 

;=============================================================================
;       CtablEvent - Handle the color selector table events, droplist
;                    or menu items.
;=============================================================================
pro tvColor::CtablEvent, ev
  ;; load the color table into range
  if tag_names(ev,/STRUCTURE_NAME) eq 'WIDGET_DROPLIST' then ind=ev.index $
  else widget_control, ev.id,get_uvalue=ind
  sm_loadct,(*self.col_list)[ind],/silent,BOTTOM=self.bottom, $
            NCOLORS=self.topval-self.bottom-self.nreserve+1
  ;; chop the colors appropriately
  tvlct,r,g,b,/GET
  (*self.rorig)=r[self.bottom:self.topval]
  (*self.gorig)=g[self.bottom:self.topval]
  (*self.borig)=b[self.bottom:self.topval]
  self->Stretch                 ;restretch
end

;=============================================================================
;	tvColor_Event and Event.  Handle the slider stretch events...
;=============================================================================
pro tvColor_Event, ev
  ;; only stretch events arrive.
  widget_control, ev.handler, get_uvalue=self
  self->Event,ev
end

pro tvColor::Event,ev
  widget_control,self.wHigh, get_value=high
  widget_control,self.wLow,  get_value=low
  widget_control,self.wGamma,get_value=gamval
  self.high=(float(high)-self.min[1])/(self.max[1]-self.min[1])
  self.low=(float(low)-self.min[0])/(self.max[0]-self.min[0])
  self.gamma=float(gamval)/100.
  self->Stretch
end

;=============================================================================
;	Cleanup.  
;=============================================================================
pro tvColor::Cleanup
  ;; set system variable for later use.
  !ctabl.low=self.low & !ctabl.high=self.high & !ctabl.gamma=self.gamma
  ptr_free,self.r,self.g,self.b,self.rorig,self.gorig,self.borig, $
           self.colnames,self.col_list
  self->tvPlug::Cleanup
end

;=============================================================================
;	Start.  Post-Initialization
;=============================================================================
pro tvColor::Start
  self->tvPlug::Start
  self->DrawCbar
end

;=============================================================================
;	Init.  Initialize the tvColor object
;=============================================================================
function tvColor::Init,parent,oDraw,COL_TABLE_PARENT=ctp,COL_TABLE_MENU=menu, $
                       USE_COLORMAPS=uc,START=strt,MIN=min,MAX=max, $
                       WIDTH=sz,TOP=top,BOTTOM=bot,RESERVE=rsrv,CBARID=cbarid,$
                       MOUSE_MODE=mm,SLIDERS=sld,PROTECT=prt,_EXTRA=e
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  
  if n_elements(top) eq 0 then top=(!D.TABLE_SIZE-1)
  if n_elements(bot) eq 0 then bot=0
  
  ;; load up the reserve colors, if asked.
  type=size(rsrv,/TYPE)
  if type ne 0 then begin       ;*something* there
     if type eq 8 then begin    ;structs
        self.colnames=ptr_new(rsrv.color)
        tvlct, rsrv.red, rsrv.green, rsrv.blue, (top-n_elements(rsrv)+1) > 0
     endif else begin           ;anything else ...  make our own color table,
        ;; construct standards
        self.colnames= $
           ptr_new(['charcoal','red','green','blue','yellow','white'])
        red  =   [ 70b,       255b, 0b,     0b,    255b,    255b  ]
        green=   [ 70b,       0b,   255b,   0b,    255b,    255b  ]
        blue =   [ 70b,       0b,   0b,     255b,  0b,      255b  ]
        tvlct,red,green,blue,(top-6+1) > 0
     endelse 
     self.nreserve=n_elements(*self.colnames) 
  endif
  
  self.topval=top
  self.bottom=bot
  
  ;; set up the color range to use in our tvDraw object
  self.oDraw->SetProperty,TOP=self.topval-self.nreserve,BOTTOM=self.bottom
  
  ;; check if we have a colorbar to draw.
  if n_elements(cbarid) ne 0 then begin 
     if widget_info(cbarid,/VALID_ID) then begin 
        self.wBar=cbarid
        geom=widget_info(cbarid,/GEOMETRY) 
        self.bSize=[geom.xsize,geom.ysize]
     endif 
  endif 
  
  ;; check to see if color map selection is wanted (menu or droplist)
  if keyword_set(ctp) or keyword_set(menu) then begin
     colors=bytarr(1,40)
     sm_loadct,get_names=colors,/SILENT
     
     ;; which of the full list to keep
     if n_elements(uc) eq 0 then uc=indgen(n_elements(colors))  else begin 
        uc=0>uc<(n_elements(colors)-1)
        uc=uc[uniq(uc,sort(uc))]
     endelse 
     
     if keyword_set(menu) then begin ; button -- in a menu
        self.wColtabl=widget_button(menu,value='Colormaps', $
                                    /MENU,uvalue=self,  $
                                    event_pro='tvColor_ctabl_event')
        for i=0,n_elements(uc)-1 do $
           but=widget_button(self.wColtabl,value=colors[uc[i]],uvalue=i) 
     endif else begin           ; droplist it is
        p=parent
        if size(ctp,/TYPE) eq 3 then if widget_info(ctp,/VALID_ID) then p=ctp 
        self.wColtabl=widget_droplist(p,value=colors[uc],uvalue=self,  $
                                      event_pro='tvColor_ctabl_event')
        widget_control, self.wColtabl,set_droplist_select=!ctabl.cmap
     endelse 
     self.col_list=ptr_new(uc,/NO_COPY)
  endif
  
  if self.topval-self.bottom lt (self.nreserve+5) then begin 
     message,'tvColor: Not enough colors, aborting.'
     return,0
  endif 
  
  sm_loadct,/SILENT,/NO_RESET,BOTTOM=self.bottom, $
            NCOLORS=self.topval-self.bottom-self.nreserve+1
  if n_elements(min) eq 0 then min=[0,0,4]
  if n_elements(max) eq 0 then max=[100,100,500]
  if n_elements(strt) ne 3 then begin ;get starting params from system var
     sm_ctdef                   ; Ensure our color table sysvar is defined
     strt=fltarr(3)
     strt[0]=min[0]> min[0]+(!ctabl.low)*(max[0]-min[0]) < max[0]
     strt[1]=min[1]>min[0]+(!ctabl.high)*(max[1]-min[1]) < max[1]
     strt[2]=min[2]>fix(!ctabl.gamma*100)<max[2]
  endif 
  
  self.min=min & self.max=max
  self.high=(float(strt[1])-self.min[1])/(self.max[1]-self.min[1])
  self.low=(float(strt[0])-self.min[0])/(self.max[0]-self.min[0])
  self.gamma=float(strt[2])/100.
  
  ;; load up the current color palette
  tvlct,r,g,b,/GET
  self.r=ptr_new(r[self.bottom:self.topval]) 
  self.g=ptr_new(g[self.bottom:self.topval]) 
  self.b=ptr_new(b[self.bottom:self.topval])
  self.rorig=ptr_new(r[self.bottom:self.topval])
  self.gorig=ptr_new(g[self.bottom:self.topval]) 
  self.borig=ptr_new(b[self.bottom:self.topval])
  
  ;; Use mouse mode (tvColor as an exclusive tool with button events)
  self.mouse_mode=keyword_set(mm) 
  
  ;; Use sliders stuck into the base.
  if keyword_set(sld) then begin 
     ;; Use color sliders, or as an exclusive tool with mouse mode
     if n_elements(sz) eq 0 then begin 
        geom=widget_info(parent,/GEOMETRY)
        sz=geom.xsize > 150
     endif 
     base=widget_base(parent,/ROW, EVENT_PRO='tvColor_Event',uvalue=self)
     self.wlow=WIDGET_SLIDER(base, TITLE = "Stretch Bottom", $
                             MINIMUM = min[0], MAXIMUM = max[0],  $
                             VALUE = strt[0], /DRAG,XSIZE=sz/3-5)
     self.whigh=WIDGET_SLIDER(base, TITLE = "Stretch Top", $
                              MINIMUM = min[1], MAXIMUM = max[1],  $
                              VALUE = strt[1], /DRAG,XSIZE=sz/3-5)
     self.wgamma=WIDGET_SLIDER(base, TITLE = "Gamma (x 100)", $
                               MINIMUM = min[2], MAXIMUM = max[2],  $
                               VALUE = strt[2], /DRAG,XSIZE=sz/3-5)
  endif
  
  self->Stretch
  self.protect=keyword_set(prt)
  self->Off
  return,1
end 

;=============================================================================
;	tvColor__define.  Prototype the tvColor class.
;=============================================================================
pro tvColor__define
  struct={tvColor, $ 
          INHERITS tvPlug, $    ;make it a tvDraw plug-in
          mouse_mode: 0b, $     ;whether we're using a mouse based stretch
          protect: 0b, $        ;whether to protect the colormap on re-entry
          col_list: ptr_new(),$ ;which of IDL's built-in colormaps to use
          winsize:[0,0], $      ;size of the draw window
          wBar:0L, $            ;widget id of color bar
          bSize:[0,0], $        ;the size of the color bar        
          wLow:0L, $            ;low stretch widget slider id
          wHigh:0L, $           ;high widget slider id
          wGamma:0L, $          ;gamma widget slider id
          low:0., $             ;the current low stretch value
          high:0., $            ;the high value
          gamma:0., $           ;the gamma value
          topval:0L, $          ;value of top used index..
          bottom:0L, $          ;the value of the bottom color index to use.
          min:[0,0,0], $        ;Slider min and max values
          max:[0,0,0], $        ;
          r:ptr_new(), $        ;the r,g,b vectors to save.
          g:ptr_new(), $
          b:ptr_new(), $
          rorig:ptr_new(), $    ;original colors
          gorig:ptr_new(), $    ;original colors
          borig:ptr_new(), $    ;original colors
          nreserve: 0, $        ;number of reserve colors below topval  
          colnames:ptr_new(),$  ;the names of the reserved colors
          wColtabl:0L}          ;the color table widget id

end
