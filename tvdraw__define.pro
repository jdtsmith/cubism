;+
; NAME:  
;
;    tvDraw
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Class interfacing to a draw widget, providing plug-in mechanism
;    with ObjMsg communications.
;    
; CATEGORY:
;
;    SMART IRS Spectral Reduction, Analysis and Processing.
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
;          obj->GetProperty, [IMORIG=,IMMOD=, IMDISP=, PAN=, SIZE=, $
;                            ZOOM=, OFFSET=,DISPSIZE=,DRAWWIN=, $
;                            WINSIZE=, PIXWIN=,TOP=, BOTTOM=, $
;                            DRAWWIDGET=, _REF_EXTRA=]
;
;       OUTPUT KEYWORD PARAMETERS:
;
;          IMORIG: The original, unmodified image.
;          IMMOD: The modified image (e.g. filtered, rescaled, etc.)
;          IMDISP: The final byte-scaled image which is displayed.
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
; NOTES:
;  
;    The tvDraw object holds heap data for three images: original,
;    modified, and displayed.  The display image is as actually
;    displayed, i.e. bytescaled, etc, and probably should be
;    considered read-only.  The original image may be reset for new
;    display.  The modify image is used for in-place modification by
;    plug-ins.  Typically, a plug-in doesn't need to worry about which
;    image to work with, but instead should just use the one passed in
;    the "Im" field of all TVDRAW draw messages.  This is correct for
;    almost all cases. 
;
;    Plug-ins wishing to respond to changes in the overall image about
;    to be drawn should subscribe to TVDRAW_POSTDRAW messages, which
;    are sent after every image is drawn, be it a new image, a change
;    in zoom, etc.  Plug-ins wishing to manipulate the image before
;    drawing should subscribe to TVDRAW_PREDRAW messages, which are
;    sent only when the original data is changed.  Plug-ins which draw
;    atop the image (e.g., a box) and need to be alerted of the
;    possibility that their additions were erased or overdrawn should
;    subscribe to TVDRAW_REDRAW.
;
;    The modify image, immod, is distributed with TVDRAW_PREDRAW
;    messages, and is initially seeded with the original image on each
;    draw for which PREDRAW messages are sent out (typically those for
;    which the original data changes).
;
;    N.B. Since the order of modifications performed by plug-ins which
;    subscribe to TVDRAW_PREDRAW messages is the same as the ordering
;    of plug-ins on the message recipient list, if the order of
;    operations is important, care should be taken to add them in an
;    appropriate sequence.  The most useful plug-ins, however, try to
;    remain ordering-independent.
;
; INHERITANCE TREE:
;
;     ObjMsg-->OMArray-->tvDraw
;
; EXAMPLE:
;
;    a=some_init()
;    SMART_ROUTINE,a,b,KEYWORD=foo
;    print,foo
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
;      MsgSignup - Reset the widget events when Signup changes.
;=============================================================================
pro tvDraw::MsgSignup, objs,_EXTRA=e
  self->OMArray::MsgSignup, objs,_EXTRA=e 
  self->ResetWidgetEvents       ; fix up widget events to match signup request
end

;=============================================================================
;      MsgRemove - Remove an ObjMsg inherited object from the tvDraw
;                  message recipient list and shutdown any relevant
;                  widget events.
;============================================================================= 
pro tvDraw::MsgRemove, theList
  self->ObjMsg::MsgRemove, theList ;chain to ObjMsg superclass
  self->ResetWidgetEvents
end   

;=============================================================================
;      ResetWidgetEvents - Make widget events settings match those
;                          messages requested on the recipient list.
;============================================================================= 
pro tvDraw::ResetWidgetEvents
  ;; adjust widget events to suit recipient list properties.
  widget_control, self.wDraw, $
                  Draw_Button_Events=self->IsSet('DRAW_BUTTON'), $
                  Draw_Motion_Events=self->IsSet('DRAW_MOTION'), $
                  Draw_Viewport_Events=self->IsSet('DRAW_VIEWPORT'),$
                  Draw_Expose_Events=self->IsSet('DRAW_EXPOSE'), $
                  Tracking_Events=self->IsSet('WIDGET_TRACKING')
end

function tvDraw::GetObj,list
  return,list.Obj
end 

;=============================================================================
;       MsgSendWhich - Translate WIDGET_DRAW types appropriately
;=============================================================================
function tvDraw::MsgSendWhich, msg
  type=tag_names(msg,/STRUCTURE_NAME)
  if type eq 'WIDGET_DRAW' then begin 
     type=(['DRAW_BUTTON','DRAW_MOTION','DRAW_VIEWPORT'])[(msg.type-1)>0]
     msg=create_struct(NAME=type,msg)
  endif 
  return,self->OMArray::MsgSendWhich(msg)
end

;=============================================================================
;      SetProperty - Set properties for tvDraw objects.  Use NO_RESIZE
;                    to display a new image using the same offset and
;                    zoom, etc.  Otherwise, these are reset.
;============================================================================= 
pro tvDraw::SetProperty, IMORIG=io,DISPSIZE=ds,OFFSET=off, $
                         TOP=top,BOTTOM=bottom,NO_RESIZE=nrs,NO_DRAW=nd
  if n_elements(top) ne 0 then self.top=top
  if n_elements(bottom) ne 0 then self.bottom=bottom
  
  draw=0                        ; no drawing, by default
  
  ;;  By default, don't send out PREDRAWs: original image may not change.
  pre=0                        
  if n_elements(off) ne 0 then $
     if array_equal(self.offset,off) eq 0 then begin 
     draw=1
     self.offset=off
  endif 
  
  if n_elements(ds) ne 0 then $
     if array_equal(self.dispsize,ds) eq 0 then begin 
     draw=1
     self.dispsize=ds 
  endif 
  
  if n_elements(io) ne 0 then begin ; a new image
     pre=(draw=1)               ; Message recipients will want to modify it.
     s=size(io)
     ptr_free,self.imorig
     if s[s[0]+1] eq 10 then begin ;a pointer, don't duplicate the space
        self.imorig=io
     endif else begin 
        self.imorig=ptr_new(io) ;actual data
     endelse 
     *self.immod=*self.imorig
     ;; Set size if they didn't prohibit it, or if we *have* to
     if NOT keyword_set(nrs) or self.dispsize[0] eq 0 then begin 
        self.offset=0
        self.dispsize=(self.size=s[1:2])
     endif
  endif
  
  if draw and keyword_set(nd) eq 0 then self->Draw,PREDRAW=pre
end

;=============================================================================
;      GetProperty - Retrieve the various properties of the object The
;                    returned value is a structure with fields
;                    equivalent to the keyword names.
;=============================================================================
pro tvDraw::GetProperty, IMORIG=io,IMMOD=im, IMDISP=id, PAN=pn, SIZE=sz, $
                         ZOOM=zm, OFFSET=off,DISPSIZE=disp,DRAWWIN=draw, $
                         WINSIZE=ws, PIXWIN=pw,TOP=top, BOTTOM=bot, $
                         DRAWWIDGET=draww, _REF_EXTRA=re
  
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
  if arg_present(id) then id=self.imdisp
  if arg_present(im) then id=self.immod
  if arg_present(off) then off=self.offset
  if arg_present(disp) then disp=self.dispsize
end

;;*************************End OverRiding methods******************************

;=============================================================================
;       Convert - flexibly convert device coordinates to pixel
;                 coordinates, unless DEVICE is set, then return
;                 device coordinates of the center of the pixel.
;                 FRACTIONAL is for coorinates already in Fractional
;                 units (no center of pixel calculation
;                 required).... e.g. pixel (x,y)=(1.544,1.23)
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
     pix=self.offset+floor(float(co-self.pan)/self.zoom)
     if keyword_set(sh) then begin ;only those showing
        if array_equal(pix ge self.offset AND $
                       pix lt self.offset+self.dispsize,1b) eq 0. $
           then return,-1
     endif
     if keyword_set(tr) then pix=0>pix<(self.size-1)
  endelse 
  if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
end

pro tvDraw::SetWin, OLD=old
  if keyword_set(old) then dw=self.oldwin else dw=self.drawwin
  wset,dw
end

;; A bit rude, but set the TLB title in which our tvDraw is embedded.
pro tvDraw::SetTitle,title
  wi=self.wDraw
  while wi gt 0 do begin 
     tlb=wi
     wi=widget_info(wi,/PARENT) 
  endwhile 
  widget_control, tlb, TLB_SET_TITLE=title
end

pro tvdraw_event,ev
  widget_control, ev.handler,get_uvalue=self
  self->MsgSend,ev              ;Send out events to our message recipients.
end

;=============================================================================
;       Erase - Erase a section of the screen by copying over the pixmap
;=============================================================================
pro tvDraw::Erase,ll,dist
  oldwin=!D.WINDOW
  self->SetWin
  device,copy=[ll,dist,ll,self.pixwin]
  wset,oldwin
end

;=============================================================================
;       Draw - Draw the relevant portion of the draw image,
;              dispatching messages before and afterwards.
;=============================================================================
pro tvDraw::Draw,PREDRAW=pre
  if NOT ptr_valid(self.imorig) then return ;gotta have something there.
  if n_elements(pre) eq 0 then pre=1
  
  ;; Set the window
  self->SetWin
  
  ;; Set the original image
  if keyword_set(pre) then begin 
     *self.immod=*self.imorig   ;reset the mod image
     self->MsgSend,/PREDRAW     ;let the plug-ins go to it
  endif 
  
  ;; Do the byte scaling from the modified, sized image
  *self.imdisp=self.bottom+ $
     bytscl((*self.immod)[self.offset[0]:self.offset[0]+self.dispsize[0]-1,$
                          self.offset[1]:self.offset[1]+self.dispsize[1]-1], $
            TOP=self.top-self.bottom)
  
  ;; Resize it
  self.zoom=min(float(self.winsize)/self.dispsize)
  if self.zoom lt 1.0 then begin ;zooming out.
     s=self.dispsize * self.zoom
     ;; zoom out by whatever factor required... use rebin if it's an integer
     ;; factor, congrid otherwise
     if 1./self.zoom eq fix(1./self.zoom) and array_equal(s,fix(s)) then  $
        im=rebin(*self.imdisp,s[0],s[1]) else $
        im=congrid(*self.imdisp,s[0],s[1])
  endif else begin              ;zooming in
     ;; only zoom in by an integer factor.
     self.zoom=floor(self.zoom)
     s=self.dispsize*self.zoom
     im=rebin(*self.imdisp,s[0],s[1],SAMPLE=1) 
  endelse
  self.pan=(self.winsize-self.zoom*self.dispsize)/2>0

  ;; Actually draw it
  erase & tv, im, self.pan[0], self.pan[1]
  
  ;; Grab a copy of it with a device copy
  wset,self.pixwin
  device,copy=[0,0,self.winsize, 0,0,self.drawwin]
  self->SetWin
  
  ;; Send out POSTDRAW and REDRAW messages.
  self->MsgSend,/POSTDRAW,/REDRAW
end

;; Send a redraw message.
pro tvDraw::SendRedraw
  self->MsgSend,/REDRAW
end

;=============================================================================
;	Plug - Register a plug-in module... called by tvPlug.
;=============================================================================
pro tvDraw::Plug, pl
  if array_equal(obj_isa(pl,'tvPlug'),1b) eq 0b then return
  if ptr_valid(self.plug_list) then *self.plug_list=[*self.plug_list,pl] $
  else self.plug_list=ptr_new(pl)
end

;=============================================================================
;       SendMessage - Send a specific message of the specified type
;                     (either pre or post-draw) to the interested
;                     recipients.
;=============================================================================
pro tvDraw::MsgSend,msg,PREDRAW=pre,POSTDRAW=post,REDRAW=redr,EXCLUSIVE=obj
  if obj_valid(obj) then self->ObjMsg::MsgSend,{TVDRAW_EXCLUSIVE,obj}
  if keyword_set(pre) then self->ObjMsg::MsgSend,{TVDRAW_PREDRAW,self.immod}  
  if keyword_set(post) then self->ObjMsg::MsgSend,{TVDRAW_POSTDRAW,self.imdisp}
  if keyword_set(redr) then self->ObjMsg::MsgSend,{TVDRAW_REDRAW,self.imdisp}
  if size(msg,/TYPE) eq 8 then self->ObjMsg::MsgSend,msg
end

;=============================================================================
;       Start - Start up all of our plug-ins
;=============================================================================
pro tvDraw::Start
  widget_control, self.wDraw, get_value=dw
  self.drawwin=dw
  if NOT ptr_valid(self.plug_list) then return
  good=where(obj_valid(*self.plug_list),cnt) ;trim the list of dead weight
  if cnt eq 0 then begin
     ptr_free,self.plug_list
     return
  endif 
  ;;draw what we have, to set up zoom and other variables.
  self->Draw,PREDRAW=0
  *self.plug_list=(*self.plug_list)[good]
  ;; Initialize all the plug-ins
  for i=0,cnt-1 do (*self.plug_list)[i]->Start
end


;=============================================================================
;       Cleanup - Destroy all our plug-ins
;=============================================================================
pro tvDraw::Cleanup
  ptr_free,self.immod,self.imdisp,self.imorig
  wdelete,self.pixwin
  if ptr_valid(self.plug_list) then obj_destroy,*self.plug_list
  ptr_free,self.plug_list
  self->OMArray::Cleanup
  self->ObjMsg::Cleanup         ;chain up: cleans up the recip list
end

;=============================================================================
;       Init - Initialize a tvDraw object, creating a draw widget and
;              initializing with provided data.
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
  self.immod=ptr_new(/ALLOCATE_HEAP)
  self.imdisp=ptr_new(/ALLOCATE_HEAP)
  s=size(imdata,/TYPE)
  if s ne 0 then $              ;only set up if data is defined...
     self->SetProperty,IMORIG=imdata,/NO_DRAW $
  else self.imorig=ptr_new(/ALLOCATE_HEAP)
  
  ;; Set-up all the messages we can send
  self->MsgSetup,['DRAW_BUTTON','DRAW_MOTION','DRAW_VIEWPORT','DRAW_EXPOSE', $
                  'WIDGET_TRACKING','TVDRAW_PREDRAW','TVDRAW_POSTDRAW', $
                  'TVDRAW_REDRAW','TVDRAW_EXCLUSIVE']
  ;; we don't draw it here since our widget is as of yet unrealized.
  return,1
end

;=============================================================================
;       tvDraw__define - Prototype the tvDraw class, including any
;                        state elements that are necessary to describe
;                        the current data displayed in the draw widget
;=============================================================================
pro tvDraw__define
  struct={tvDraw, $
          INHERITS OMArray, $   ;the Helper for ObjMsg
          INHERITS ObjMsg, $    ;Make it a messaging object
          wDraw:0L, $           ;the draw widget id
          drawwin: 0L, $        ;the window id of the draw canvas
          oldwin:0, $           ;the old window which was set.
          psconf:obj_new(), $   ;the FSC_PSConfig object
          imorig:ptr_new(), $   ;saved original image, unmodified
          immod:ptr_new(), $    ;the modifiable (by, e.g., filters) image
          imdisp:ptr_new(), $   ;the scaled byte image actually displayed
          plug_list:ptr_new(),$ ;the list of plug-ins
          pixwin:0, $           ;pixmap window in which we save new image
          winsize:[0,0], $      ;size of window        
          size:[0,0], $         ;size of image in pixels
          zoom:0.0, $           ;(int or 1/int) amount draw image zoomed
          bottom:0, $           ;bottom color index to scale to
          top:0, $              ;top color index to scale to
          pan:[0,0], $          ;device pixel offsets for lower left corner
          offset:[0,0],$        ;the image pixeloffset of the lower left
          $                     ;    pixel drawn  [xoff,yoff]
          dispsize:[0,0]}       ;current [xsize,ysize] of displayed portion
                                ;    of array
  ;; Messages (other than widget events) we send.
  msg={TVDRAW_EXCLUSIVE,obj:obj_new()} ; for exclusive plug-ins
  msg={TVDRAW_PREDRAW,im:ptr_new()} ; sent before draw, for modification
  msg={TVDRAW_POSTDRAW,im:ptr_new()} ; sent after draw, for interpretation
  msg={TVDRAW_REDRAW,im:ptr_new()} ; sent when screen gets clobbered
end
