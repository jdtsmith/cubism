;+
; NAME: tvColor
;
; PURPOSE: An object for manipulating color.
;
; CATEGORY: Object-Based Widget
;
; CALLING SEQUENCE:
; 	td=obj_new('tvColor',parent,START=strt,LOW=low,HIGH=high, $
;                Width=wd, TOP=top, BOTTOM=bot, RESERVE=resrv)
;	  Create a new tvColor object with parent a widget id to be
;	  the parent of the draw widget created.
;	  KEYWORDS:
;	  	Start:	The starting values of bottom and top chop, and gamma
;		Min:	The lower limit (3 element vec) for bottom,
;			top, and gamma, as shown on the slider.  Defaults to
;			[0,0,1]. (Gamma=gamslider/100.)
;		Max:	The upper limit (3 element vec) for bottom,
;			top, and gamma, as shown on the slider.
;			Defaults to [100,100,500].
;		Width:	The width in pixels of the widget representing
;		COL_TABLE_PARENT:
;			The widget id of the parent to place a colortable
;			selector into, or any non_zero value to use the same
;			parent as the slider base. If COL_TABLE_PARENT is
;			zero or ommitted, no colortable selector is created
;			or used.
;	  	CBARID: The widget id of a colorbar draw widget to draw a color
;	  		bar into (with appropriate top and bottom colors).
;	  		The colorbar is drawn with method DrawCbar, which
;	  		must be called *after* the colorbar widget is realized.
;	  	PROTECT:If this keyword is set, colors will be
;	  		reinstated on entering the draw widget.
;		Reserve:EITHER:
;			1: A scalar such that if equal to 1 a standard set
;			of colors (charcoal,red, green,blue, yellow) will
;			be set into the reserve space (top-nres+1:top).
;			2. An nres length vector of structures of the form:
;			{color:'colname',r:0b,g:0b,b:0b} where colname is the
;			name of the color, and the r,g,b values are as in
;			tvlct.  These colors are loaded as reserve, and are
;			available by name via the method GetColor.
;			If absent, no colors are reserved.  Reserve colors
;			can be retrieved with the method tvColor::GetReserve.
;		Top:	The top of the colormap to fill to, including space
;			for reserve colors.  Defaults to !D.N_COLORS-1-nreserve
;		Bottom:	The bottom of the colormap to fill from, defaulting to
;			0.
; INPUTS:	parent:	The widget id of the object widget's parent
;

; METHODS:
; 		GetColor(colname): get the color index of the 'colorname'
; 			reserve color (only works if defined, -1 returned
; 			otherwise). e.g.
; 				plots,x,y,color=oCol->GetColor('yellow')
; 		DrawCbar: Draw the colorbar (only *after* the colorbar widget
; 			is realized).
; 		SetColors: Load the internally maintained colors into the
; 			colormap (useful, e.g. on reentry of widget).  Called
; 			automatically on reentry if PROTECT is set.
; INTERNAL METHODS:
; 		Stretch:  Use the bottom,top,gamma slider values to stretch
; 			the colormap (internall
;		Cleanup: Auto cleanup the object.
;		Init: See for arguments/keywords.
;		
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
; INHERITANCE TREE: tvPlug -> tvColor
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-


;;**************************OverRiding methods********************************
;=============================================================================
;	Message.  Override the tvPlug Message to display vals.  We have
;	signed up for tracking messages from the tvDraw object.
;=============================================================================
pro tvColor::Message,msg
   ;; only tracking events -- nothing else
   if msg.enter eq 1 then begin 
      ;;reinstate colors
      self.oDraw->SetWin
   endif
end 

;=============================================================================
;	tvColor_DrawCbar.  For drawing the colorbar, call *after* the 
;	draw widget for the colorbar is realized.  Draws colorbar only
;	for given range (bottom:top) in colormap.
;=============================================================================
function tvColor::GetColor, colname
   if NOT ptr_valid(self.colnames) then return,-1
   wh=where(*self.colnames eq strtrim(strlowcase(colname),2),cnt)
   if cnt ne 0 then return,self.topval+1-self.nreserve+wh[0]>0  $
   else return,(self.bottom+self.topval-self.nreserve)
end

pro tvColor::GetProperty, BAR=bar, NRESERVE=nres, MIN=min, MAX=max, BSIZE=bs, $
           TOP=top, BOTTOM=bot
   if arg_present(nres) then nres=self.nreserve
   if arg_present(bar) then bar=self.wBar
   if arg_present(max) then max=self.max
   if arg_present(min) then min=self.min
   if arg_present(bs) then bs=self.bSize
   if arg_present(top) then top=self.topval
   if arg_present(bot) then bot=self.bottom
end

;=============================================================================
;	tvColor_DrawCbar.  For drawing the colorbar, call *after* the 
;	draw widget for the colorbar is realized.  Draws colorbar only
;	for given range (bottom:top) in colormap.
;=============================================================================
pro tvColor::DrawCbar, WIN=win
   if self.wBar eq 0L then return
   oldwin=!D.Window
   widget_control, self.wBar, get_value=win
   wset,win
   tv,self.bottom+bytscl(lindgen(self.bSize[0],self.bSize[1])  $
                         mod self.bSize[0],  $
                         TOP=self.topval-self.bottom-self.nreserve)
   wset,oldwin
   return
end

;=============================================================================
;	tvColor_SetColors.  Reset the colors to their appropriate values.
;	useful upon reentry of the widget, in case others have damaged the map.
;=============================================================================
pro tvColor::SetColors
   tvlct,*self.r,*self.g,*self.b,self.bottom
end

;=============================================================================
;	Stretch.  Stretch the colors
;=============================================================================
pro tvColor::Stretch
   ;;total colors in our used range
   ncolors=(self.topval-self.bottom-self.nreserve+1)
   
   ;; at low want 0, at high want 1.
   ;; i.e indxnew[low]=m*indxold[low]+b=0.;
   ;; indxnew[high]=m*indxold[high]+b=1.
   ;; --> m=1./(indxold[high]-indxold[low])
   ;; --> b=-m*indxold[low]
   div=self.high-self.low
   if div eq 0 then m=0. else m=1./(self.high-self.low)/ncolors
   b=-m*self.low*ncolors
   index=findgen(ncolors)       ;total range or *our* color indices
   index=0.>(m*index+b)<1.      ;chop it down to specified range
   if self.gamma eq 1.0 then index=long((ncolors-1)*index) else  $
    index=long((ncolors-1)*index^self.gamma)
   ;; Index into the original colors
   (*self.r)[self.bottom:self.bottom+ncolors-1]=(*self.rorig)[index]
   (*self.g)[self.bottom:self.bottom+ncolors-1]=(*self.gorig)[index]
   (*self.b)[self.bottom:self.bottom+ncolors-1]=(*self.borig)[index]
   self->SetColors              ;set them in.
   return
end

;=============================================================================
;	TlbEvent.  Handle the tlb events...
;=============================================================================
;pro tvColor::TlbEvent,ev
;   widget_control, ev.handler, get_uvalue=self
;   type=tag_names(ev,/STRUCTURE_NAME)
;   if type eq 'WIDGET_TRACKING' then begin 
;      if ev.enter eq 1 then begin 
;         ;;reinstate colors
;         self->SetColors
;      endif 
;   endif 
;   return
;end



;=============================================================================
;	tvColor_tlb_Event.  Hand off tlb events...
;=============================================================================
;pro tvColor_tlb_Event, ev
;   widget_control, ev.handler, get_uvalue=self
;   self->TlbEvent, ev
;   return   
;end


;=============================================================================
;	CtablEvent.  Handle the color table events
;=============================================================================
pro tvColor::CtablEvent, ev
   ;; load the color table into range
   cload,ev.index,/silent, NCOLORS=self.topval-self.bottom-self.nreserve+1, $
    BOTTOM=self.bottom, /NOSET
   self.index=ev.index
   ;; adjust the colors
   tvlct,r,g,b,/GET
   (*self.rorig)=r[self.bottom:self.topval]
   (*self.gorig)=g[self.bottom:self.topval]
   (*self.borig)=b[self.bottom:self.topval]
   self->Stretch                ;restretch
   return
end

;=============================================================================
;	tvColor_ctabl_Event.  Hand off ctabl events...
;=============================================================================
pro tvColor_ctabl_Event, ev
   widget_control, ev.handler,get_uvalue=self
   self->CtablEvent,ev
end 
   

;=============================================================================
;	tvColor_Event and Event.  Handle the internal stretch events...
;=============================================================================
pro tvColor_Event, ev
   ;; only stretch events arrive.
   widget_control, ev.handler, get_uvalue=self
   self->Event,ev
end

pro tvColor::Event,ev
   widget_control,self.wHigh,get_value=high
   widget_control, self.wLow,get_value=low
   widget_control, self.wGamma,get_value=gamval
   self.high=(float(high)-self.min[1])/(self.max[1]-self.min[1])
   self.low=(float(low)-self.min[0])/(self.max[0]-self.min[0])
   self.gamma=float(gamval)/100.
   self->Stretch
   return
end

;=============================================================================
;	Cleanup.  
;=============================================================================
pro tvColor::Cleanup
   ;; set system variable for later use.
   !ctabl.low=self.low & !ctabl.high=self.high & !ctabl.gamma=self.gamma
   !ctabl.cmap=self.index
   ptr_free,self.r
   ptr_free,self.g
   ptr_free,self.b
   ptr_free,self.colnames
   return
end

;=============================================================================
;	Start.  Post-Initialization
;=============================================================================
pro tvColor::Start
   self->DrawCbar
end

;=============================================================================
;	Init.  Initialize the tvColor object
;=============================================================================
function tvColor::Init,parent,oDraw,COL_TABLE_PARENT=ctp,START=strt,MIN=min, $
                PROTECT=prt,MAX=max, Width=sz, TOP=top, BOTTOM=bot, $
                RESERVE=rsrv, CBARID=cbarid
   if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
   
   if n_elements(top) eq 0 then top=!D.N_COLORS-1 
   if n_elements(bot) eq 0 then bot=0
   
   ;; load up the reserve colors, if asked.
   s=size(rsrv)
   type=s[s[0]+1]
   if type ne 0 then begin ;*something* there
      if type eq 8 then begin   ;structs
         self.colnames=ptr_new(rsrv.color)
         tvlct, rsrv.red, rsrv.green, rsrv.blue, (top-s[s[0]]+1) > 0
      endif else begin     ;anything else ...  make our own color table,
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

   ;; set up the colors for the tvDraw object
   self.oDraw->SetProperty,TOP=self.topval-self.nreserve,BOTTOM=self.bottom
   
   ;; check if we have a bar to draw.
   if n_elements(cbarid) ne 0 then begin 
      if widget_info(cbarid,/VALID_ID) then begin 
         self.wBar=cbarid
         geom=widget_info(cbarid,/GEOMETRY) 
         self.bSize=[geom.xsize,geom.ysize]
      endif 
   endif 
   
   ;; check to see if a color table is wanted
   if keyword_set(ctp) then begin
      colors=bytarr(1,40)
      cdef                      ;ensure it's defined
      cload,get_names=colors,/SILENT ;this also define !ctabl if necessary
      s=size(ctp)
      if s[s[0]+1] eq 3 then p=ctp else p=parent ;if a long
      self.wColtabl=widget_droplist(p,value=colors,uvalue=self,  $
                                    event_pro='tvColor_ctabl_event')
      widget_control, self.wColtabl,set_droplist_select=!ctabl.cmap
      self.index=!ctabl.cmap
      cload,self.index,/SILENT, BOTTOM=self.bottom, $
       NCOLORS=self.topval-self.bottom-self.nreserve+1
   endif 
      
   if n_elements(min) eq 0 then min=[0,0,4]
   if n_elements(max) eq 0 then max=[100,100,500]
   if n_elements(strt) ne 3 then begin ;get starting params from system var
      strt=fltarr(3)
      cdef                      ;ensure it's defined...
      strt[0]=min[0]> min[0]+(!ctabl.low)*(max[0]-min[0]) < max[0]
      strt[1]=min[1]>min[0]+(!ctabl.high)*(max[1]-min[1]) < max[1]
      strt[2]=min[2]>fix(!ctabl.gamma*100)<max[2]
   endif 
   
   self.min=min & self.max=max
   self.high=(float(strt[1])-self.min[1])/(self.max[1]-self.min[1])
   self.low=(float(strt[0])-self.min[0])/(self.max[0]-self.min[0])
   self.gamma=float(strt[2])/100.
   
   if n_elements(sz) eq 0 then begin 
      geom=widget_info(parent,/GEOMETRY)
      sz=geom.xsize > 150
   endif 
   
   ;; load up the current color palette
   tvlct,r,g,b,/GET
   self.r=ptr_new(r[self.bottom:self.topval]) 
   self.g=ptr_new(g[self.bottom:self.topval]) 
   self.b=ptr_new(b[self.bottom:self.topval])
   self.rorig=ptr_new(r[self.bottom:self.topval])
   self.gorig=ptr_new(g[self.bottom:self.topval]) 
   self.borig=ptr_new(b[self.bottom:self.topval])
   
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
   self->Stretch
   self.recip.ACTIVE=1b
   if keyword_set(prt) then self.recip.TRACKING=1b
   self->Update                 ;sign up with draw widget
   return,1
end 

;=============================================================================
;	tvColor__define.  Prototype the tvColor class.
;=============================================================================
pro tvColor__define
   struct={tvColor, $ 
           INHERITS tvPlug, $   ;make it a tvDraw plug-in
           wBar:0L, $           ;widget id of color bar
           bSize:[0,0], $       ;the size of the color bar        
           wLow:0L, $           ;low stretch widget slider id
           wHigh:0L, $          ;high widget slider id
           wGamma:0L, $         ;gamma widget slider id
           low:0., $            ;the current low stretch value
           high:0., $           ;the high value
           gamma:0., $          ;the gamma value
           topval:0L, $          ;value of top used index.. leaves room
           bottom:0L, $          ;the value of the bottom color index to use.
           min:[0,0,0], $
           max:[0,0,0], $
           r:ptr_new(), $       ;the r,g,b vectors to save.
           g:ptr_new(), $
           b:ptr_new(), $
           rorig:ptr_new(), $   ;original colors
           gorig:ptr_new(), $   ;original colors
           borig:ptr_new(), $   ;original colors
           nreserve: 0, $       ;number of reserve colors  
           colnames:ptr_new(),$ ;the names of the reserved colors
           wColtabl:0L, $       ;the color table widget id
           index:0}             ;the color index
                        ;the tlb of the widget
end
