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
;    INIT:
;
;       CALLING SEQUENCE:
;
;          a=obj_new('tvDraw',parent,[DATA=,TVD_XSIZE=,TVD_YSIZE=,
;             TOP=,BOTTOM=])
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
;          DATA: A pointer to an array or array data itself for
;             display.
;
;          TOP: The top color index to use.
;
;          TVD_(X|Y)SIZE: The (X|Y) size of the display canvas, in
;             pixels.  If XSIZE is set and YSIZE is not, both default
;             to that size, otherwise it defaults to 256x256
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
;          ZOOM: The zoom factor.
;          OFFSET: The coordinates of the lower left corner of the
;             portion of the full image actually being displayed.
;          DISPSIZE: The dimensions of the portion of the image
;             actually being displayed.
;          DRAWWIN: The window id of the draw canvas.
;          WINSIZE: The size (in device pixels).
;          PIXWIN: The widow id of the pixmap window, used for screen
;             erases.
;          TOP: The topmost color index to which to scale.
;          BOTTOM: The bottomost color index from which to scale.
;          DRAWWIDGET: The widget ID of the draw canvas.
;          _REF_EXTRA: Any other GetProperty keyword relevant to the
;             parent class (ObjMsg).
;
; PROCEDURE:
;
;    Notable routines or classes (*not* parent classes -- see next entry)
;    it relies on.
;
; NOTES:
;  
;    Additional description and other information which doesn't fit elsewhere.
;
; INHERITANCE TREE:
;
;     ObjMsg--tvDraw
;
; EXAMPLE:
;
;    a=some_init()
;    SMART_ROUTINE,a,b,KEYWORD=foo
;    print,foo
;
; MODIFICATION HISTORY:
;    $Log$
;    Revision 1.2  2001/08/17 18:11:49  jdsmith
;    	  A substantial rewrite of SCORE-era messaging, with new
;    	  messages, simpler access to the recipient list (see
;    	  MsgList), etc.  The message flow also changed somewhat, with
;    	  the introduction of another image buffer (immod), which
;    	  allows plug-ins cleaner separation of passive and active
;    	  response to changes in the source data (like new images,
;    	  zooming, etc.).
;
;    Revision 1.1  2001/08/01 19:29:24  jdsmith
;             Imported source from SCORE files.
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



;+
; NAME: tvDraw
;
; PURPOSE: An object interface to a draw widget.  This object
;       subclasses from the ObjMsg class.
;
; CATEGORY: Object-Based Drawing Widget
;
; CALLING SEQUENCE:
; 	;
; 	td=obj_new('tvDraw',parent,DATA=data,MsgList=ml,_EXTRA=e)
;	  Create a new tvDraw object with a 'parent' widget id to be
;	  the parent of the draw widget created.
;	  KEYWORDS:
;	  	data:	the data to use for display  
;	  	EXTRA:	Any extra keywords accepted by the INIT method of
;	  	        any superclass in the inheritance tree are allowed.
;		including....
;		msglist:A recipient list of ObjMsg derived objects,
;	  		in the appropriate format (see below).  
;
;	  		
;	td->SetProperty,IMDRAW=imdraw,IMORIG=imorig,_EXTRA=e
;	  Set properties of the tvDraw object.
;	  KEYWORDS:
;		imdraw: image to draw into IDLgrImage object.. should derive
;			from the original image as stored in the data field
;			td.imorig.
;		imorig: original image to use, both for saving, and for
;			drawing as the draw image in the IDLgrImage object.
;		EXTRA:	Any keywords used by a SetProperty method of the
;			superclasses is also allowed.
; 
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;	
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
; NOTES: The tvDraw object holds heap data for two images... original,
; and draw image (OI and DI).  DI derives from OI through a series of
; modifications performed by qualified plug-ins.  These plug-ins can
; either:
;   a) affect no tvDraw parameters (such as, e.g., a colormap tool).
;   b) affect the display parameters such as zoom or offset.
;   c) affect the tvDraw data itself, such as a filter plug-in.
; Those plug-ins of type c) are always active plug-ins, whereas those
; of type a) or b) usually passive.  The operations defined by passive
; plug-ins can be performed in any order, so long as they are not
; adjusting the same resource (e.g. two plug-ins simultaneously
; changing the zoom factor), in which case these types might be classified
; active.  The ordering of operations performed by active widgets, however,
; affects the *values* of the final image drawn.  Examples might be an
; active 2-d smoothing filter, and an active resampling plug-in.  Whether
; you resample the data or smooth it first will depend on exactly how you
; want the application to behave.  A subcategory of active plug-ins is
; defined by whether the output data is suitable for computation, or for
; display only.  In our previous example, the smooth filter might produce an
; image suitable for further computation, whereas the resampling plug-in might
; produce a byte image suitable only for display.  Therefore, a third image
; variable is maintained: computational image, or CI.  All plug-ins which
; produce computational data would be implemented before all which affect
; display data.  The first active computational plug-in would modify OI,
; putting into CI, the second should modify CI, put back into CI, etc.
; Then the first non-computational active would modify CI, putting into DI,
; the second would modify DI, putting back into DI, etc.
; 
;
;
; RESTRICTIONS: This type of object is not for use in the standard,
; "up-the-tree" style event processing paradigm.  No events are returned by
; the event handler, but only passed as messages.
; The ObjMsg type of object *can*, however, be chained
; into a tree hierarchy by adding a liason ObjMsg to the recipient list,
; which simply passes the events on to the next event handler up the chain.
; The idea is that recipient ObjMsg objects would only request events
; if they can handle them, so the "up-the-tree" passing mechanism is obviated,
; and the user is free to design a complicated messaging relationship among
; the various objects which constitute their application.
; 
;
; INHERITANCE TREE: ObjMsg --> tvDraw
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

;      MsgList - Add an ObjMsg inherited object to the tvDraw message
;                recipient list.  Elements of the recipient list are
;                added here in an overloaded MsgList, by specifying
;                those message types an object or objects are
;                interested in as keywords.  The full keyword names
;                must be used.  The MsgList is searched first for an
;                existing record for each object, and is used as the
;                basis for the new record.  The list of possible
;                keywords (and messages) includes:
;	  WIDGET_DRAW EVENTS
;      		BUTTON
; 		MOTION
; 	   	VIEWPORT
; 	   	EXPOSE
; 	  	TRACKING
; 	  OTHER MESSAGES:
;               PREDRAW: Sent just before drawing.  For modification
;                        of the image passed as a pointer in the event.
;               POSTDRAW: Sent just after drawing.  For read-only
;                         access to the image passed as a pointer in
;                         the message.
;               EXCLUSIVE: For use by objects which require exclusive
;                          control ; of an application.  These
;                          messages will be sent when ; the exclusive
;                          state changes.
;
;       Special keyword ALL_OFF can be used to set or clear all messages.
;=============================================================================
pro tvDraw::MsgList, objs,ALL_OFF=ao,NO_RESET=nr,_EXTRA=set_source
  ;; Should we just remove them?
  if keyword_set(ao) and n_elements(set_source) eq 0 then begin 
     self->MsgRemove,objs
     return
  endif 
  ;; Construct the true message list elements from the keywords passed.
  for i=0,n_elements(objs)-1  do begin
     rec=self->GetRecord(objs[i])
     if size(rec,/TYPE) ne 8 then $ ; No record
        rec={TVDRAW_RECIP}
     struct_assign,set_source,rec,NOZERO=keyword_set(ao) ne 1
     rec.obj=objs[i]            ; make sure this is set
     if n_elements(recs) eq 0 then recs=[rec] else recs=[recs,rec]
  endfor 
  
  if n_elements(recs) eq 0 then return
  self->ObjMsg::MsgList, recs   ;chain to superclass to actually add them
  if keyword_set(nr) eq 0 then self->ResetWidgetEvents ;fix up widget events
end

;=============================================================================
;      MsgRemove - Remove an ObjMsg inherited object from the tvDraw
;                  message recipient list and shutdown any relevant
;                  widget events.
;============================================================================= 
pro tvDraw::MsgRemove, theList
  self->ObjMsg::MsgRemove, theList ;chain to superclass
  self->ResetWidgetEvents
end   

;=============================================================================
;      ResetWidgetEvents - Make widget events settings match those
;                          messages requested on the recipient list.
;============================================================================= 
pro tvDraw::ResetWidgetEvents
  ;; adjust widget events to suit recipient list properties.
  if Ptr_Valid(self.MsgList) then begin 
     val=intarr(5)
     ;; the widget event types start at field 2.
     for i=2,6 do begin 
        val[i-2]=total((*self.MsgList).(i)) ne 0
     endfor 
     widget_control, self.wDraw, Draw_Button_Events=val[0], $
                     Draw_Motion_Events=val[1], Draw_Viewport_Events=val[2], $
                     Draw_Expose_Events=val[3], Tracking_Events=val[4]
  endif
end

function tvDraw::GetObj,list
  return,list.Obj
end 

;=============================================================================
;       MsgSendWhich - Determine which object(s) on the list would
;                      like the message.
;=============================================================================
function tvDraw::MsgSendWhich, msg
  msgtype=tag_names(msg,/STRUCTURE_NAME)
  
  ;; calculate the location in the TVDRAW_RECIP struct
  case msgtype of
     'TVDRAW_EXCLUSIVE': pos=1
     'WIDGET_DRAW':      pos=(msg.type-1)>0+2
     'WIDGET_TRACKING':  pos=6
     'TVDRAW_PREDRAW':   pos=7
     'TVDRAW_POSTDRAW':  pos=8
     'TVDRAW_REDRAW':    pos=9
     else: message,'Message type has no match: '+msgtype
  endcase 
  
  ;; return those objects requesting this message type...
  good=where((*self.MsgList).(pos) ne 0b,cnt)
  if cnt eq 0 then return, -1 else return, self->GetObj((*self.MsgList)[good])
  
;; get the inactive ones which we care about
;   if pos eq 7 or pos eq 8 then begin 
;      flags=good AND ((*self.MsgList).ACTIVE eq 0b)
;      whi=where(flags,icnt)
;  endif else icnt=0
  
  ;; get the active ones
;   flags=good and ((*self.MsgList).ACTIVE ne 0b)
;   wha=where(flags,acnt)
  
;   if acnt eq 0 then begin       ;no active ones
;      if icnt ne 0 then wha=whi  ;only inactives
;   endif else if icnt ne 0 then $ ;some of both
;      if pos eq 7 then wha=[whi,wha] else wha=[wha,whi]
  
;   if icnt+acnt ne 0 then return,self->GetObj((*self.MsgList)[wha])  $
;  else return,-1
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
  
  pre=0 & post=0
  if n_elements(off) ne 0 then begin
     if total(self.offset eq off) le 1 then begin 
        post=1
        self.offset=off
     endif 
  endif 
  if n_elements(ds) ne 0 then begin 
     if total(self.dispsize eq ds) le 1 then begin
        post=1
        self.dispsize=ds 
     endif 
  endif 
  
  ;; a new image
  if n_elements(io) ne 0 then begin 
     pre=1                      ; We can modify it.
     s=size(io)
     ptr_free,self.imorig
     if s[s[0]+1] eq 10 then begin ;a pointer
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
  
  if pre+post gt 0 then begin 
     if NOT keyword_set(nd) then self->Draw,PRE=pre,POST=post
  endif 
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
;                 required).... e.g. pixel 1.544.  
;=============================================================================
function tvDraw::Convert, coord, DEVICE=dev, FRACTIONAL=frd, $
                          ZOOM=zm, PAN=pn, OFFSET=off, SHOWING=sh, $
                          SINGLE_INDEX=si,TRUNCATE=tr
  if arg_present(zm) then zm=self.zoom
  if arg_present(pn) then pn=self.pan
  if arg_present(off) then off=self.offset
  if n_elements(coord) eq 1 then $
     co=[coord mod self.size[0],coord/self.size[0]]  $
  else co=coord
  if keyword_set(dev) then begin 
     ;; Return device coordinates of a pixel
     if keyword_set(sh) then begin 
        ;; Report only on pixels which are now diplayed.
        if total(co lt self.offset)+ $
           total(co ge self.offset+self.dispsize) gt 0. then return,-1
     endif 
     inc=keyword_set(frd)?0.:.5 ;fractional pixels, not to center
     pix=floor(self.pan+self.zoom*(co+inc-self.offset))
     if keyword_set(tr) then pix=0>pix<(self.dispsize-1)
     if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
  endif else begin 
     ;; Return pixel coordinates of a given device coordinate
     pix=self.offset+floor(float(co-self.pan)/self.zoom)
     if keyword_set(sh) then begin 
        if total(pix lt self.offset)+ $
           total(pix ge self.offset+self.dispsize) gt 0. then return,-1
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
  return
end

pro tvdraw_event,ev
  widget_control, ev.handler,get_uvalue=self
  self->MsgSend,ev              ;Send out events to our message recipients.
end

pro tvDraw::Postscript
  if NOT obj_valid(self.psconf) then begin 
     self.psconf=Obj_New('FSC_PSConfig')
  endif 
  self.psconf->GUI,GROUP_LEADER=widget_info(self.wDraw,/PARENT),CANCEL=cncld
  if(cncld) then return
  thisDevice = !D.Name
  Set_Plot, "PS"
  keys=self.psconf->GetKeywords()
  Device, _Extra=keys
  tvfits, TOP=self.top, DSIZE=[keys.xsize,keys.ysize], $
          (*self.image)[self.offset[0]:self.offset[0]+self.dispsize[0]-1,$
                        self.offset[1]:self.offset[1]+self.dispsize[1]-1]
  Device, /Close_File
  Set_Plot, thisDevice
end

;=============================================================================
;       Draw - Draw the relevant portion of the draw image,
;              dispatching messages before and afterwards.
;=============================================================================
pro tvDraw::Draw,PREDRAW=pre,_EXTRA=e
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
     if 1./self.zoom eq fix(1./self.zoom) and total(s eq fix(s)) gt 1 then  $
        im=rebin(*self.imdisp,s[0],s[1]) else $
        im=congrid(*self.imdisp,s[0],s[1])
  endif else begin 
     ;; only zoom "in" by an integer factor.
     self.zoom=floor(self.zoom)
     s=self.dispsize*self.zoom
     im=rebin(*self.imdisp,s[0],s[1],SAMPLE=1) 
  endelse
  self.pan=(self.winsize-self.zoom*self.dispsize)/2>0

  ;; Actually draw it
  erase
  tv, im, self.pan[0], self.pan[1]
  
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
  if total(obj_isa(pl,'tvPlug')) ne n_elements(pl) then return
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

pro tvDraw::Start
  widget_control, self.wDraw, get_value=dw
  self.drawwin=dw
  self->Draw                    ;draw what we have
  if NOT ptr_valid(self.plug_list) then return
  
  wh=where(obj_valid(*self.plug_list),cnt)
  if cnt eq 0 then begin
     ptr_free,self.plug_list
     return
  endif 
  if cnt ne n_elements(*self.plug_list) then  $
     *self.plug_list=(*self.plug_list)[wh]
  ;; Initialize all the plug-ins
  for i=0,cnt-1 do begin
     print,'starting ',(*self.plug_list)[i]
     (*self.plug_list)[i]->Start
  endfor 
end

;=============================================================================
;       Init - Initialize a tvDraw object, creating a draw widget and
;              initializing with provided data.
;=============================================================================
function tvDraw::Init,parent,DATA=imdata,TVD_XSIZE=xs,TVD_YSIZE=ys, $
                      TOP=top,BOTTOM=bottom,_EXTRA=e
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
  if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain
  ;; set up the widget draw canvas .. unrealized as of yet
  self.wDraw=widget_draw(parent,/FRAME,EVENT_PRO='tvdraw_event',  $
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
  ;; we don't draw it here since our widget is as of yet unrealized.
  return,1
end

pro tvDraw::Cleanup
  ptr_free,self.immod,self.imdisp,self.imorig
  wdelete,self.pixwin
  if ptr_valid(self.plug_list) then obj_destroy,*self.plug_list
  ptr_free,self.plug_list
  self->ObjMsg::Cleanup         ;chain up: cleans up the recip list
end

;=============================================================================
;       tvDraw__define - Prototype the tvDraw class, including any
;                        state elements that are necessary to describe
;                        the current data displayed in the draw widget
;=============================================================================
pro tvDraw__define
  struct={tvDraw, $
          INHERITS ObjMsg, $
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
  
  ;; The member element type of tvDraw's message recipient list.
  mlist={TVDRAW_RECIP, $
         OBJ:obj_new(), $
         EXCLUSIVE:0b, $        ;The all important exclusives
         BUTTON:0b,MOTION:0b, $ 
         VIEWPORT:0b,EXPOSE:0b, TRACKING:0b, $ ;standard widget_draw events
         PREDRAW:0b, POSTDRAW:0b, REDRAW:0b} ; tvDraw specific messages
 
  
  ;; Messages (other than widget events) we send.
  msg={TVDRAW_EXCLUSIVE,obj:obj_new()} ; for exclusive plug-ins
  msg={TVDRAW_PREDRAW,im:ptr_new()}    ; sent before draw, for modification
  msg={TVDRAW_POSTDRAW,im:ptr_new()}   ; sent after draw, for interpretation
  msg={TVDRAW_REDRAW,im:ptr_new()}     ; sent when screen gets clobbered
end
