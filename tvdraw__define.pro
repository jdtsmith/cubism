;+
; NAME:  
;
;    tvDraw
;
; DESCRIPTION:
;    
;    Class interfacing to a draw widget, providing plug-in mechanism
;    (tvPlug) with ObjMsg communications.
;    
; CATEGORY:
;
;    Image viewing and display.
;    	
; METHODS:
;
;    Init:
;
;       CALLING SEQUENCE:
;
;          a=obj_new('tvDraw',parent,[IMORIG=,TVD_XSIZE=,TVD_YSIZE=,
;             TOP=,BOTTOM=,/INVISIBLE_BASE])
;
;       INPUT PARAMETERS:
;
;          parent: The widget id of the parent widget into which to
;             insert the widget draw canvas.
;
;       INPUT KEYWORD PARAMETERS:
;	   
;	   BOTTOM: The bottom color index to use.  Defaults to 0.
;       
;          IMORIG: An array or  pointer to an array for display.
;
;          TOP: The top color index to use.
;
;          TVD_(X,Y)SIZE: The (X,Y) size of the display canvas, in
;             pixels.  If XSIZE is set and YSIZE is not, both default
;             to that size, otherwise it defaults to 256x256
;
;          INVISIBLE_BASE: If set, an invisible base will be created
;             inside of parent, and the draw canvas will be placed
;             there.  Useful for hidden hotkey widget (see tvKey).
;             
;          _EXTRA: Any additional keywords for ObjMsg::Init or
;             widget_draw may also be passed.
;
;    GetProperty:
;  
;	DESCRIPTION:
;
;	   Get various properties of the object.
;	
;       CALLING SEQUENCE:
;
;          obj->GetProperty, [IMORIG=,IMMOD=, IMSCL=, PAN=, SIZE=, $
;                            ZOOM=, OFFSET=,DISPSIZE=,DRAWWIN=, $
;                            WINSIZE=, PIXWIN=,TOP=, BOTTOM=, $
;                            DRAWWIDGET=, _REF_EXTRA=]
;
;       OUTPUT KEYWORD PARAMETERS:
;
;          IMORIG: The original, unmodified image.
;          IMMOD: The modified image (e.g. filtered, rescaled, etc.)
;          IMSCL: The byte-scaled image as displayed.
;          PAN: The X,Y gutter from lower left edge of the draw canvas
;             to the corner of the image
;          SIZE: The size (dimensions) of the original image.
;          ZOOM: The current zoom factor.
;          OFFSET: The pixel coordinates of the lower left corner of
;             the portion of the full image actually being displayed.
;          DISPSIZE: The dimensions in pixels of the portion of the
;             image actually being displayed.
;          DRAWWIN: The window id of the draw canvas.
;          WINSIZE: The size (in device units).
;          PIXWIN: The widow id of the pixmap window, used for screen
;             erases.
;          TOP: The topmost color index to use.
;          BOTTOM: The bottomost color index from which to scale.
;          DRAWWIDGET: The widget ID of the draw canvas.
;          _REF_EXTRA: Any other GetProperty keyword relevant to the
;             parent class (ObjMsg).
;
;    SendRedraw:
;  
;	DESCRIPTION:
;
;	   Send out a redraw message when the screen is overwritten.
;	
;       CALLING SEQUENCE:
;
;          oDraw->SendRedraw
;
;
;    Redraw:
;  
;	DESCRIPTION:
;
;          Quickly redraw the scaled image and optionally take a
;          Snaphot (sending out SnapShot messages).
;          
;       CALLING SEQUENCE:
;
;          oDraw->Redraw,[/SNAPSHOT]
;          
;       INPUT KEYWORD PARAMETERS:
;	   
;          SNAPSHOT: If set, take a snapshot of the window right after
;             drawing it, which sends out the SnapShot messages (see
;             below).
;
; NOTES:
;  
;    tvDraw, and the related tvPlug parent class, facilitate the
;    creation of display "plug-ins", which can make use of common
;    display facilities (like zooming, pan/offset, coordinate
;    conversion, etc.) tvPlug provides.  See the "tvPlug" class file
;    for more information on creating plug-ins.
;
;    IMAGES:
;    
;    The tvDraw object holds heap data for three images for external
;    use, both of the same size (it also possibly keeps one more for
;    internal use, on visuals without writeable colormaps).  They are:
;
;    Original: The original data displayed, in the original type.  Set
;       this to display a new image altogether.
;    
;    Modify: Used only for in-place modification by plug-ins, this
;       image is initially seeded with the original image.  Plug-ins
;       read from and write to this image to effect modifications
;       (scalings, etc.).

;    Scaled: The byte-scaled image as actually displayed.  Should
;       *not* be written to.
;
;    Typically, a plug-in doesn't need to worry about which image to
;    work with, but instead should just use the one passed to it in
;    the "Im" field of all TVDRAW draw messages.  This is correct
;    image to use for almost all cases.
;
;    MESSAGES:
;
;    tvDraw send a variety of useful messages, including most relevant
;    WIDGET_DRAW events, along with several others.
;
;    Draw Messages:
;
;    All draw messages contain just one field: IM, which contains a
;    pointer to the image data associated with that message.
;
;    TVDRAW_PREDRAW: Plug-ins wishing to manipulate the image before
;       drawing should subscribe to TVDRAW_PREDRAW messages, which are
;       sent only when the original data is changed.  The "Modify"
;       image pointer is distributed with this message, and can be
;       read and/or written to.
;    
;    TVDRAW_POSTDRAW: Plug-ins wishing to respond to changes in the
;       overall image about to be drawn (e.g., computing statistics)
;       should subscribe to TVDRAW_POSTDRAW messages, which are sent
;       after every image is drawn, be it a new image, a change in
;       zoom, etc.  Plugins should only *read* the image passed.
;
;    TVDRAW_REDRAW: Plug-ins which draw atop the image (e.g., a box or
;       line) or otherwise need to be alerted when the image is
;       redrawn should subcribe to TVDRAW_REDRAW.  The scaled image is
;       passed along, but often won't be needed.  Another options is
;       to subscript to SNAPSHOT images.
;
;    TVDRAW_SNAPSHOT: An alternative to REDRAW.  For plug-ins which
;      draw detailed, but infrequently changing features on top of the
;      image, the option exists to have the feature included in the
;      erase buffer itself, by subscribing to SNAPSHOT messages, and
;      redrawing the features when this message is received.  This
;      would be used *instead of* the normal method of replotting on
;      receipt of REDRAW messages, and can significantly reduce
;      flicker, especially for complex overplots, since REDRAW
;      messages occur far more frequently than SNAPSHOT messages
;      SNAPSHOT's should occur only when a static overpattern is added
;      or removed.  The penalty is that the image itself has to be
;      fully redrawn (either by side effect, or directly using
;      oDraw->Redraw,/SNAPSHOT to initiate) when the overplot is added
;      or removed, and thus should occur relatively infrequently.
;
;    N.B. Since the ordering of image modifications performed by
;    plug-ins which subscribe to TVDRAW_PREDRAW messages is goverened
;    by the order in which they first signed up for any messages, if
;    the operations order is important, care should be taken to add
;    them in the appropriate sequence.
;
;    The relationship of the various coordinates and measures in a
;    tvDraw window,displaying a portion of an image of size `size':
;      
;   +----------- winsize[0] (device) -------------------------------------+
;   |                                                                     |
;   |                                                                     |
;   |               <------------  dispsize[0]     -------------->        |
;   |                            pixel coordinates                        |
;   |               +----+----+----+----+----+----+----+----+----+  ^     |
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   w               |    |    |    |    |    |    |    |    |    |  |     |
;   i               +----+----+----+----+----+----+----+----+----+  |     |
;   n               |    |    |    |    |    |    |    |    |    |  |     |
;   s               |    |    |    |    |    |    |    |    |    |  |     |
;   i               +----+----+----+----+----+----+----+----+----+  |     |
;   z               |    |    |    |    |    |    |    |    |    |  |     |
;   e               |    |    |    |    |    |    |    |    |    |  |     |
;  [1]              +----+----+----+----+----+----+----+----+----+  |     |
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   |               |    |    |    |    |    |    |    |    |    |  | disp|
;   |               +----+----+----+----+----+----+----+----+----+  | size|
;   |               |    |    |    |    |    |    |    |    |    |  |  [1]|
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   |               +----+----+----+----+----+----+----+----+----+  |     |
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   |               +----+----+----+----+----+----+----+----+----+  |     |
;   |               |  pixel (offset[0],offset[1])|    |    |    |  |     |
;   |               |    /       center at .5     |    |    |    |  |     |
;   | pan[0]        +---/+----+----+----+----+----+----+----+----+  |     |
;   +---------------+  / |    |    |    |    |    |    |    |    |  |     |
;   |               |    |    |    |    |    |    |    |    |    |  |     |
;   | device        +--+-+----+----+----+----+----+----+----+----+  ~     |
;   | coordinates      |                                                  |
;   |                  | pan[1]                                           |
;   +------------------+--------------------------------------------------+
;
;
; INHERITANCE TREE:
;
;     ObjMsg-->OMArray-->tvDraw
;
; MODIFICATION HISTORY:
; 
;    2001-08-17 (J.D. Smith): Accomodate rewritten and simplified
;       messaging subsytem.
;
;    2001-08-01 (J.D. Smith): Initial import from SCORE-era viewer
;       component collection.
;    
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002 J.D. Smith
;
;  This file is part of tvTools.
;
;  tvTools is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  tvTools is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with tvTools; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

;;**************************OverRiding methods********************************

;=============================================================================
;  MsgSignup - Reset the widget events when Signup changes.
;=============================================================================
pro tvDraw::MsgSignup, objs,_EXTRA=e
  self->OMArray::MsgSignup, objs,_EXTRA=e 
  self->ResetWidgetEvents       ; fix up widget events to match signup request
end

;=============================================================================
;  MsgListRemove - Remove an ObjMsg inherited object from the tvDraw
;                  message recipient list and shutdown any relevant
;                  widget events.
;=============================================================================
pro tvDraw::MsgListRemove, theList
  self->ObjMsg::MsgListRemove, theList ;chain to ObjMsg superclass
  self->ResetWidgetEvents
end   

;=============================================================================
;  ResetWidgetEvents - Make widget events settings match those
;                      messages requested on the recipient list.
;=============================================================================
pro tvDraw::ResetWidgetEvents
  ;; adjust widget events to suit recipient list properties.
  widget_control, self.wDraw, $
                  DRAW_BUTTON_EVENTS=self->IsSet('DRAW_BUTTON'), $
                  DRAW_MOTION_EVENTS=self->IsSet('DRAW_MOTION'), $
                  DRAW_VIEWPORT_EVENTS=self->IsSet('DRAW_VIEWPORT'),$
                  DRAW_EXPOSE_EVENTS=self->IsSet('DRAW_EXPOSE'), $
                  TRACKING_EVENTS=self->IsSet('WIDGET_TRACKING'), $
                  DRAW_KEYBOARD_EVENTS=self->IsSet('DRAW_KEY')
end

function tvDraw::GetMsgListObj,list
  return,list.Obj
end 

;=============================================================================
;  MsgSendWhich - Translate WIDGET_DRAW types appropriately
;=============================================================================
function tvDraw::MsgSendWhich, msg
  type=tag_names(msg,/STRUCTURE_NAME)
  if type eq 'WIDGET_DRAW' then begin 
     type=(['DRAW_BUTTON','DRAW_MOTION','DRAW_VIEWPORT','DRAW_EXPOSE', $
            'DRAW_KEY'])[0>(msg.type-1)<4]
     msg=create_struct(NAME=type,msg)
  endif 
  return,self->OMArray::MsgSendWhich(msg)
end

;=============================================================================
;  SetProperty - Set properties for tvDraw objects.  Use NO_RESIZE to
;                display a new image using the same offset and zoom,
;                etc. (unless we can't).  Otherwise, these are reset.
;=============================================================================
pro tvDraw::SetProperty, IMORIG=io,DISPSIZE=ds,OFFSET=off, $
                         TOP=top,BOTTOM=bottom,NO_RESIZE=nrs,NO_DRAW=nd, $
                         _EXTRA=e
  if n_elements(top) ne 0 then self.top=top
  if n_elements(bottom) ne 0 then self.bottom=bottom
  
  draw=0                        ; no drawing, by default
  
  ;;  By default, don't send out PREDRAWs: original image may not change.
  pre=0                        
  if n_elements(off) ne 0 then $
     if NOT array_equal(self.offset,off) then begin 
     draw=1
     self.offset=off
  endif 
  
  if n_elements(ds) ne 0 then $
     if NOT array_equal(self.dispsize,ds) then begin 
     draw=1
     self.dispsize=ds 
  endif 
  
  if n_elements(io) ne 0 then begin ; a new image
     pre=(draw=1)               ; Our plugins will want to have at it.
     s=size(io,/DIMENSIONS)
     ptr_free,self.imorig
     if size(io,/TYPE) eq 10 then begin ;a pointer, don't duplicate the space
        self.imorig=io
     endif else begin 
        self.imorig=ptr_new(io) ;actual data
     endelse 
     *self.immod=*self.imorig
     ;; Reset zoom, etc. if they didn't prohibit it, or if we're forced to
     if NOT keyword_set(nrs) OR self.dispsize[0] eq 0 OR $
        NOT array_equal(self.size,s) then begin 
        self.offset=0
        self.dispsize=(self.size=s)
     endif
  endif
  if draw and keyword_set(nd) eq 0 then self->Draw,PREDRAW=pre,_EXTRA=e
end

;=============================================================================
;  GetProperty - Retrieve the various properties of the object The
;                returned value is a structure with fields equivalent
;                to the keyword names.
;=============================================================================
pro tvDraw::GetProperty, IMORIG=io,IMMOD=im, IMSCL=is, PAN=pn, SIZE=sz, $
                         ZOOM=zm, OFFSET=off,DISPSIZE=disp,DRAWWIN=draw, $
                         WINSIZE=ws, PIXWIN=pw,TOP=top, BOTTOM=bot, $
                         DRAWWIDGET=draww,_REF_EXTRA=re
  
  ;; Chain up using by-reference keyword inheritance
  if n_elements(re) ne 0 then self->ObjMsg::GetProperty,_EXTRA=re
  if arg_present(draw) then draw=self.drawwin
  if arg_present(draww) then draww=self.wDraw
  if arg_present(top) then top=self.top
  if arg_present(bot) then bot=self.bottom
  if arg_present(pw) then pw=self.pixwin
  if arg_present(ws) then ws=self.winsize
  if arg_present(pn) then pn=self.pan
  if arg_present(sz) then sz=self.size
  if arg_present(zm) then zm=self.zoom
  if arg_present(io) then io=self.imorig
  if arg_present(is) then is=self.imscl
  if arg_present(im) then im=self.immod
  if arg_present(off) then off=self.offset
  if arg_present(disp) then disp=self.dispsize
end

;;*************************End OverRiding methods******************************

;=============================================================================
;  Convert - Convert device coordinates to pixel coordinates, unless
;            DEVICE is set, then return device coordinates of the
;            center of the pixel.  FRACTIONAL: when converting to
;            device, it means coordinates already in fractional units
;            (no center of pixel calculation required).... e.g. pixel
;            (x,y)=(1.544,1.23).  When converting to pixel, means
;            return fractional pixels, from which floor(x) and
;            floor(y) are the logical pixels.  Pixels are 0 based,
;            with pixel centers at 0.5.
;=============================================================================
function tvDraw::Convert, coord, DEVICE=dev, FRACTIONAL=frd,SHOWING=sh, $
                          SINGLE_INDEX=si,TRUNCATE=tr, DISTANCE=di
  if n_elements(coord) eq 1 then $
     co=[coord mod self.size[0],coord/self.size[0]]  $
  else co=coord
  if keyword_set(dev) then begin 
     ;; Return device coordinates of a pixel
     if keyword_set(di) then return,self.zoom*coord ;really a distance
     if keyword_set(sh) then begin 
        ;; Report only on pixels which are now diplayed.
        if array_equal(co ge self.offset AND $
                       co lt (self.offset+self.dispsize),1b) eq 0 then $
           return,-1
     endif 
     inc=keyword_set(frd)?0.:.5 ;fractional pixels, not to center
     pix=floor(self.pan+self.zoom*(co+inc-self.offset))
     if keyword_set(tr) then pix=0>pix<(self.winsize-1)
     if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
  endif else begin 
     ;; Return pixel coordinates of a given device coordinate
     if keyword_set(di) then return,floor(coord/self.zoom) ;really a distance
     pix=self.offset+float(co-self.pan)/self.zoom
     if NOT keyword_set(frd) then pix=floor(pix)
     if keyword_set(sh) then begin ;only those showing
        if array_equal(pix ge self.offset AND $
                       pix lt (self.offset+self.dispsize),1b) eq 0. $
           then return,-1
     endif
     if keyword_set(tr) then pix=0>pix<(self.size-1)
  endelse 
  if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
end

pro tvDraw::SetWin, OLD=old,DOUBLE=dbl
  if keyword_set(old) then dw=self.oldwin else $
     if keyword_set(dbl) then dw=self.dbwin else dw=self.drawwin
  wset,dw
end

pro tvDraw::Focus
  widget_control, self.wDraw,/INPUT_FOCUS
end

;; A bit rude, but set the TLB title in which our tvDraw is embedded.
pro tvDraw::SetTitle,title
  wi=self.wDraw
  while wi gt 0 do begin 
     tlb=wi
     wi=widget_info(wi,/PARENT) 
  endwhile 
  self.TLBTitle=title
  widget_control, tlb, TLB_SET_TITLE=title
end

function tvDraw::Title
  return,self.TLBTitle
end


pro tvdraw_event,ev
  widget_control, ev.handler,get_uvalue=self
  self->MsgSend,ev              ;Send out events to our message recipients.
end

;=============================================================================
;  BackGround - Set the smooth background color
;=============================================================================
;pro tvDraw::Background
;  erase,COLOR=self.bottom+(self.top-self.bottom)/2
;end
  
;=============================================================================
;  Erase - Erase a section of the screen by copying over the pixmap.
;          If FULL is set, erase the entire screen.  If DOUBLE is set,
;          erase to the Double-Buffer window (for later updating using
;          DBRefresh).
;=============================================================================
pro tvDraw::Erase,ll,dist,FULL=full,DOUBLE=dbl
  ;oldwin=!D.WINDOW
  self->SetWin,DOUBLE=dbl
  if keyword_set(full) then begin 
     ll=[0,0] & dist=self.winsize
  endif
  device,copy=[ll,dist,ll,self.pixwin]
  ;wset,oldwin
end

;=============================================================================
;  DBRefresh - Put the contents of the Double Buffer into the Win
;=============================================================================
pro tvDraw::DBRefresh,ll,dist,FULL=full
  self->SetWin
  if keyword_set(full) then begin 
     ll=[0,0] & dist=self.winsize
  endif
  device,copy=[ll,dist,ll,self.dbwin]
end

;=============================================================================
;  Snapshot - Take a snapshot to the pixmap
;=============================================================================
pro tvDraw::Snapshot
  self->MsgSend,/SNAPSHOT       ;see who wants to get in on the background
  wset,self.pixwin
  device,copy=[0,0,self.winsize,0,0,self.drawwin]
  wset,self.drawwin
end

;=============================================================================
;  ReDraw - Quickly redraw the already computed draw image.
;=============================================================================
pro tvDraw::ReDraw,SNAPSHOT=snap
;   if keyword_set(snap) then begin 
;      print,'redrawing with snap'
;      help,/traceback
;   endif else print,'redrawing without snap'
  snap=keyword_set(snap) 
  if snap then begin 
     self->SetWin
;     erase
  endif 
  tv, *self.imdisp, self.pan[0], self.pan[1] 
  if snap then self->Snapshot
  self->SendRedraw
end

;=============================================================================
;  Draw - Draw the relevant portion of the draw image, dispatching
;         messages before and afterwards.  If DOUBLE_BUFFER is set,
;         draw it all to an offscreen pixmap, and then copy it over
;         quickly in one pass.
;=============================================================================
pro tvDraw::Draw,PREDRAW=pre,DOUBLE_BUFFER=db
  if NOT ptr_valid(self.imorig) then return ;gotta have something there.
  if n_elements(pre) eq 0 then pre=1
  
  if keyword_set(db) then begin 
     ;;Direct all drawing to an offscreen pixmap
     dw=self.drawwin
     self.drawwin=self.dbwin
  endif 
  
  ;; Set the window
  self->SetWin
  if obj_valid(self.oColor) then self.oColor->SetColors,/NO_REDRAW
  
  ;; Set the original image
  if keyword_set(pre) then begin 
     *self.immod=*self.imorig   ;reset the mod image
     self->MsgSend,/PREDRAW     ;let the plug-ins go to it
  endif 
  
  ;; Do the byte scaling from the modified, sized image
  *self.imscl=self.bottom+ $
     bytscl((*self.immod)[self.offset[0]:self.offset[0]+self.dispsize[0]-1,$
                          self.offset[1]:self.offset[1]+self.dispsize[1]-1], $
            TOP=self.top-self.bottom,/NAN)
  
  ;; Resize it
  self.zoom=min(float(self.winsize)/self.dispsize)
  if self.zoom lt 1.0 then begin ;zooming out.
     s=self.dispsize * self.zoom
     ;; zoom out by whatever factor required... use rebin if it's an integer
     ;; factor, congrid otherwise
     if 1./self.zoom eq fix(1./self.zoom) and array_equal(s,fix(s)) then  $
        *self.imdisp=rebin(*self.imscl,s[0],s[1],/SAMPLE) else $
        *self.imdisp=congrid(*self.imscl,s[0],s[1])
  endif else begin              ;zooming in
     ;; only zoom in by an integer factor.
     self.zoom=floor(self.zoom)
     s=self.dispsize*self.zoom
     *self.imdisp=rebin(*self.imscl,s,/SAMPLE) 
  endelse
  self.pan=(self.winsize-self.zoom*self.dispsize)/2>0

  ;; Actually draw it
  erase & tv, *self.imdisp, self.pan[0], self.pan[1]
  
  ;; Grab a copy of it with a device copy
  self->Snapshot
  
  ;; Send out POSTDRAW and REDRAW messages, and let the plugins play
  self->MsgSend,/POSTDRAW,/REDRAW
  
  if keyword_set(db) then begin 
     self.drawwin=dw            ;put it all back, and copy the image over
     wset,dw
     device,COPY=[0,0,self.winsize,0,0,self.dbwin]
  endif 
end

;; Send a redraw message.
pro tvDraw::SendRedraw
  self->MsgSend,/REDRAW
end

;=============================================================================
;  Plug - Register a plug-in module... called by tvPlug_lite.  Note
;         that a tvPlug 'isa' tvPlug_lite so it works for either.
;=============================================================================
pro tvDraw::Plug, plugin
  if array_equal(obj_isa(plugin,'tvPlug_lite'),1b) eq 0b then return
  if ptr_valid(self.plug_list) then *self.plug_list=[*self.plug_list,plugin] $
  else self.plug_list=ptr_new(plugin)
end

;=============================================================================
;  SendMessage - Send a specific message of the specified type (either
;                pre or post-draw) to the interested recipients.
;=============================================================================
pro tvDraw::MsgSend,msg,PREDRAW=pre,POSTDRAW=post,REDRAW=redr,SNAPSHOT=snap
  if size(msg,/TYPE) eq 8 then begin ; A specific message
     self->ObjMsg::MsgSend,msg
     return
  endif 
  if keyword_set(pre) then self->ObjMsg::MsgSend,{TVDRAW_PREDRAW,self.immod}  
  if keyword_set(post) then self->ObjMsg::MsgSend,{TVDRAW_POSTDRAW,self.imscl}
  if keyword_set(redr) then self->ObjMsg::MsgSend,{TVDRAW_REDRAW,self.imscl}
  if keyword_set(snap) then self->ObjMsg::MsgSend,{TVDRAW_SNAPSHOT,self.imscl}
end

;=============================================================================
;  Start - Start up all of our plug-ins
;=============================================================================
pro tvDraw::Start
  widget_control, self.wDraw, get_value=dw
  self.drawwin=dw
  
  if NOT ptr_valid(self.plug_list) then return
  ;; trim the list of dead weight
  good=where(obj_valid(*self.plug_list),cnt,COMPLEMENT=bad,NCOMPLEMENT=nbad)
  if cnt eq 0 then begin
     ptr_free,self.plug_list
     return
  endif 
  ;;draw what we have, to set up zoom and other variables.
  self->Draw,PREDRAW=0
  if nbad gt 0 then *self.plug_list=(*self.plug_list)[good]
  ;; Initialize all the plug-ins
  for i=0,cnt-1 do (*self.plug_list)[i]->Start
  
  ;; Get the Color object, if any
  oCol=self->GetMsgObjs(CLASS='tvColor')
  if obj_valid(oCol[0]) then self.oColor=oCol[0]
end

;=============================================================================
;  Cleanup - Destroy all our plug-ins
;=============================================================================
pro tvDraw::Cleanup
  ptr_free,self.immod,self.imscl,self.imdisp,self.imorig
  wdelete,self.pixwin,self.dbwin
  if ptr_valid(self.plug_list) then obj_destroy,*self.plug_list
  ptr_free,self.plug_list
  self->OMArray::Cleanup
  self->ObjMsg::Cleanup         ;chain up: cleans up the recip list
end

;=============================================================================
;  Init - Initialize a tvDraw object, creating a draw widget and
;         initializing with provided data.
;=============================================================================
function tvDraw::Init,parent,IMORIG=imdata,TVD_XSIZE=xs,TVD_YSIZE=ys, $
                      TOP=top,BOTTOM=bottom,INVISIBLE_BASE=invb,_EXTRA=e
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain
  if n_elements(top) eq 0 then self.top=!D.N_COLORS-1 else self.top=top
  if n_elements(bottom) eq 0 then self.bottom=0 else self.bottom=bottom
  if n_elements(ys) eq 0 then begin
     if n_elements(xs) ne 0 then ys=xs else begin  
        ys=(xs=256) 
     endelse 
  endif 
  self.oldwin=!D.WINDOW
  if n_elements(xs) eq 0 then xs=256
  self.winsize=[xs,ys]
  self.zoom=1.
  
  if keyword_set(invb) then begin 
     invb=widget_base(parent)
     base=invb
  endif else base=parent
  
  ;; set up the widget draw canvas .. unrealized as of yet
  self.wDraw=widget_draw(base,/FRAME,EVENT_PRO='tvdraw_event',  $
                         UVALUE=self,XSIZE=self.winsize[0], $
                         YSIZE=self.winsize[1],_EXTRA=e)
  window,/Free,XSIZE=self.winsize[0],YSIZE=self.winsize[1],/PIXMAP 
  self.pixwin=!D.Window
  window,/Free,XSIZE=self.winsize[0],YSIZE=self.winsize[1],/PIXMAP 
  self.dbwin=!D.Window          ;for double buffering
  self.immod=ptr_new(/ALLOCATE_HEAP)
  self.imscl=ptr_new(/ALLOCATE_HEAP)
  self.imdisp=ptr_new(/ALLOCATE_HEAP)
  s=size(imdata,/TYPE)
  if s ne 0 then $              ;only set up if data is defined...
     self->SetProperty,IMORIG=imdata,/NO_DRAW
  
  ;; Set-up all the messages we can send
  self->MsgSetup,['DRAW_BUTTON','DRAW_MOTION','DRAW_VIEWPORT','DRAW_EXPOSE', $
                  'DRAW_KEY','WIDGET_TRACKING','TVDRAW_PREDRAW', $
                  'TVDRAW_POSTDRAW','TVDRAW_REDRAW','TVDRAW_SNAPSHOT']
  ;; we don't draw it here since our widget is as of yet unrealized.
  return,1
end

;=============================================================================
;  tvDraw__define - Prototype the tvDraw class, including any state
;                   elements that are necessary to describe the
;                   current data displayed in the draw widget
;=============================================================================
pro tvDraw__define
  st={tvDraw, $
      INHERITS OMArray, $       ;a Helper for ObjMsg
      INHERITS ObjMsg, $        ;Make it a messaging object
      wDraw:0L, $               ;the draw widget id
      drawwin: 0L, $            ;the window id of the draw canvas
      TLBTitle:'', $            ;the title we've given the TLB, if any
      oldwin:0, $               ;the old window which was set.
      psconf:obj_new(), $       ;the FSC_PSConfig object
      imorig:ptr_new(), $       ;saved original image, unmodified
      immod:ptr_new(), $        ;the modifiable (by, e.g., filters) image
      imscl:ptr_new(), $        ;the byte-scaled image
      imdisp:ptr_new(), $       ;the byte-scaled, resized image actually
                                ;  displayed (for quick redisplay)
      plug_list:ptr_new(),$     ;the list of plug-ins
      oColor:obj_new(), $       ;the color object (if any)
      pixwin:0, $               ;pixmap window in which we background
                                ; erase new image
      dbwin:0,$                 ;pixmap window for double buffering
      winsize:[0,0], $          ;size of window (device pixels)
      size:[0,0], $             ;size of image in pixels
      zoom:0.0, $               ;(int or 1/int) amount draw image zoomed
      bottom:0, $               ;bottom color index to scale to
      top:0, $                  ;top color index to scale to
      pan:[0,0], $              ;device pixel offsets for lower left corner
      offset:[0,0],$            ;the image pixeloffset of the lower left
                                ;    pixel drawn  [xoff,yoff]
      dispsize:[0,0]}           ;current [xsize,ysize] of displayed portion
                                ;    of array
  ;; Messages (other than widget events) we send.
  msg={TVDRAW_PREDRAW,im:ptr_new()} ; sent before draw, for modification
  msg={TVDRAW_POSTDRAW,im:ptr_new()} ; sent after draw, for interpretation
  msg={TVDRAW_REDRAW,im:ptr_new()} ; sent when screen gets clobbered
  msg={TVDRAW_SNAPSHOT,im:ptr_new()} ; when we fill the erase pixwin
end
