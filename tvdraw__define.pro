;+
; NAME:  
;
;    TVDraw
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Class interfacing to a draw widget, and providing plug-in
;    mechanism with ObjMsg communications.
;    
; CATEGORY:
;
;    SMART IRS Spectral Reduction, Analysis and Processing.
;    Image viewing and display.
;    	
; CALLING SEQUENCE:
;
;    smart_pro, req_arg1, req_arg2, ..., 
;       [opt_arg1,...,/BOOLEAN_KEYWORD,KEYWORD=]
;                  or
;    sm=smart_func(req_arg1, req_arg2, ..., 
;       [opt_arg1,...,/BOOLEAN_KEYWORD,KEYWORD=])
;
; INPUT PARAMETERS:
;
;    INPUT1: Describe the Input.  Don't forget to [M-q] to clean up
;       the description text.  Follow the indenting (emacs will do it
;       for you if you help).  Notice all subsequent lines of a body
;       of text which serves as a single item are indented 3 spaces.
;			
; OPTIONAL INPUT PARAMETERS:
;
;    OPTINPUT1: Describe the Optional INPUT.  Don't forget to [M-q] to 
;       clean up the description text.  May be unnecessary.  
;
;    OPTINPUT2: Leave a blank line between entries, as so.  Follow the 
;       indentation.
;			
; INPUT KEYWORD PARAMETERS:
;
;    INPKEYWORD1: Describe the input KEYWORD.  Don't forget to [M-q] to 
;       clean up the description text. May be unnecessary.
;			
; OUTPUT KEYWORD PARAMETERS:
;
;    OUTKEYWORD1: Describe the output KEYWORD.  Don't forget to [M-q] to 
;       clean up the description text. May be unnecessary.
;			
; OUTPUTS:
;
;    OUTPUT1: Describe the OUTPUT.  Don't forget to [M-q] to clean up the 
;       description text.  May be unnecessary. 
;
; OPTIONAL OUTPUTS:
;
;    OPTOUTPUT1: Describe the Optional OUTPUT.  May be unnecessary. 
;
; COMMON BLOCKS:
;
;    COMMON_BLOCK1: I hope you have a good reason for using a common
;       block.  Omit otherwise
;
; SIDE EFFECTS:
;
;    Especially for procedures with no return values.  May be unnecessary.
;
; RESTRICTIONS:
;
;    Any restrictions?
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
;       KEYWORD INPUT PARAMETERS:
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
;    METHOD1:
;  
;	DESCRIPTION:
;
;	   This method goes to market.
;	
;       CALLING SEQUENCE:
;
;          obj->Method1, req_arg1, req_arg2, ..., 
;             [opt_arg1,...,/BIN_KEYWORD,KEYWORD=]
;
;       INPUT PARAMETERS:
;
;          INPUT1
;       ...
;    ...
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
;    GrandParent1--+
;                   \
;     GrandParent2--Parent1--ThisClass  (where appropriate... usually linear)
;                            /
;    GrandParent3--Parent2--+ 
;
; EXAMPLE:
;
;    a=some_init()
;    SMART_ROUTINE,a,b,KEYWORD=foo
;    print,foo
;
; MODIFICATION HISTORY:
;    $Log$
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
;      MsgList - Add an ObjMsg inherited object to the tvDraw
;      message recipient list.  Elements of the list should be TVDRAWRECIP
;      structures of the form {TVDRAWRECIPObj:obj,ACTIVE:1b,EVS:EVLIST},
;      where object is the object to add, ACTIVE describes whether the
;      object is currently actively receiving messages, and EVLIST
;      denotes the remaining fields
;	  WIDGET_DRAW EVENTS
;      		BUTTON
; 		MOTION
; 	   	VIEWPORT
; 	   	EXPOSE
; 	  	TRACKING
; 	  OTHER MESSAGES:
; 	  	REDRAW:  When the original and/or draw image is changed,
; 	  		 or redrawn.
; 	  	EXCLUSIVE: For use by objects which require exclusive control
; 	  		 of an application.  These messages will be sent when
; 	  		 the exclusive state changes.
; 	Example {TVDRAWRECIP,Obj:ob1,ACTIVE:1b,BUTTON:1b,MOTION:1b,VIEWPORT:0b,
; 		EXPOSE:0b,TRACKING:0b,REDRAW:0b,EXCLUSIVE:0b}
; 	signs up the object "ob1" for motion and button events.
;=============================================================================
pro tvDraw::MsgList, theList
   if n_elements(theList) eq 0 then return
   self->ObjMsg::MsgList, theList ;chain to superclass
   ;; theList is added ... now adjust widget events to suit
   if Ptr_Valid(self.MsgList) then begin 
      val=intarr(5)
      ;; the widget event types start at field 2.
      for i=2,6 do begin 
         val[i-2]=total((*self.MsgList).(i)) ne 0
;        val[i]=total(((*self.MsgList).flag or self.wcFlag) and 2^i) ne 0
      endfor 
      widget_control, self.wDraw, Draw_Button_Events=val[0], $
       Draw_Motion_Events=val[1], Draw_Viewport_Events=val[2], $
       Draw_Expose_Events=val[3], Tracking_Events=val[4]
   endif 
end

;=============================================================================
;      MsgRemove - Remove an ObjMsg inherited object from the
;      tvDraw message recipient list and shutdown any relevant widget
;      events.
;============================================================================= 
pro tvDraw::MsgRemove, theList
   self->ObjMsg::MsgRemove, theList ;chain to superclass
   val=intarr(5)
   ;; re-update widget event settings on the draw widget
   for i=2,6 do begin 
      val[i-2]=total(theList.(i)) ne 0b
      ;;val[i]=total((theList.flag or self.wcFlag) and 2^i) ne 0
   endfor 
   widget_control, self.wDraw, Button_Events=val[0],Motion_Events=val[1], $
    Viewport_Events=val[2],Expose_Events=val[3],Tracking_Events=val[4]
   return
end   

function tvDraw::GetObj,list
   return,list.Obj
end 

;=============================================================================
;       MsgSendWhich - Determine which object(s) on the list would
;          like the message.
;=============================================================================
function tvDraw::MsgSendWhich, msg
   msgtype=tag_names(msg,/STRUCTURE_NAME)
   
   ;; calculate the location in the TVDRAW_RECIP struct
   case msgtype of
      'WIDGET_DRAW':      pos=(msg.type-1)>0 +2
      'WIDGET_TRACKING':  pos=6
      'TVDRAW_REDRAW':    pos=7
      'TVDRAW_EXCLUSIVE': pos=8
      else: message,'Message type has no match: '+msgtype
   endcase 
   
   ;; return those objects requesting this message type...
   ;; and send EXCLUSIVE and REDRAW to any requesting object, active or not,
   ;; but send active ones last (for REDRAW), or first (for EXCLUSIVE).
   
   good=(*self.MsgList).(pos) ne 0b 
   
   ;; get the inactive ones which we care about
   if pos eq 7 or pos eq 8 then begin 
      flags=good AND ((*self.MsgList).ACTIVE eq 0b)
      whi=where(flags,icnt)
   endif else icnt=0
   
   ;; get the active ones
   flags=good and ((*self.MsgList).ACTIVE ne 0b)
   wha=where(flags,acnt)
   
   if acnt eq 0 then begin      ;no active ones
      if icnt ne 0 then wha=whi ;only inactives
   endif else if icnt ne 0 then $ ;some of both
    if pos eq 7 then wha=[whi,wha] else wha=[wha,whi]
   
   if icnt+acnt ne 0 then return,self->GetObj((*self.MsgList)[wha])  $
   else return,-1
end

;=============================================================================
;      SetProperty - Set properties for tvDraw objects.
;============================================================================= 
pro tvDraw::SetProperty, Imorig=io,Imdraw=id,dispsize=ds, offset=off, $
          TOP=top,BOTTOM=bottom, NO_RESIZE=nrs
   type=1b                      ;default to display image redrawn 
   if n_elements(off) ne 0 then begin
      if total(self.offset eq off) le 1 then begin 
         type=type or 4b        ;image position moved
         self.offset=off
      endif 
   endif 
   if n_elements(ds) ne 0 then begin 
      if total(self.dispsize eq ds) le 1 then begin
         type=type or 4b        ;image position moved
         self.dispsize=ds 
      endif 
   endif 
   if n_elements(top) ne 0 then self.top=top
   if n_elements(bottom) ne 0 then self.bottom=bottom
   ;; change the original image
   if n_elements(io) ne 0 then begin 
      s=size(io)
      ptr_free,self.imorig
      if s(s(0)+1) eq 10 then begin ;a pointer
         self.imorig=io
      endif else begin 
         self.imorig=ptr_new(io) ;actual data
      endelse 
      ptr_free,self.image
      self.image=ptr_new(*self.imorig)
      ;; Set size if they didn't prohibit it, or if we *have* to
      if NOT keyword_set(nrs) or self.dispsize[0] eq 0 then begin 
         self.offset=0
         self.dispsize=(self.size=[s(1),s(2)])
      endif 
      type=type or 2b           ;original image reset
   endif 
   ;; change the draw image (should be derived from original image)
   if n_elements(id) ne 0 then begin 
      s=size(id)
      ptr_free,self.image
      if s(s(0)+1) eq 10 then begin ;a pointer
         self.image=id
      endif else begin 
         self.image=ptr_new(id)
      endelse 
   endif 
   self->Draw,type              ;draw it
end

;=============================================================================
;      GetProperty - Retrieve the various properties of the object
;      The returned value is a structure with fields equivalent to
;      the keyword names.
;=============================================================================
pro tvDraw::GetProperty, Imorig=io,Image=id, Pan=pn, size=sz,zoom=zm, $
               offset=off,dispsize=disp,DRAWWIN=draw, WINSIZE=ws, PIXWIN=pw, $
               TOP=top, BOTTOM=bot, DRAWWIDGET=draww, _REF_EXTRA=re
   
   if n_elements(re) ne 0 then self->ObjMsg::GetProperty,_EXTRA=re
   if arg_present(draw) then widget_control, self.wDraw,get_value=draw
   if arg_present(draww) then draww=self.wDraw
   if arg_present(top) then top=self.top
   if arg_present(bot) then bot=self.bottom
   if arg_present(pw) then pw=self.pixwin
   if arg_present(ws) then ws=self.winsize
   if arg_present(pn) then pn=self.pan
   if arg_present(sz) then sz=self.size
   if arg_present(zm) then zm=self.zoom
   if arg_present(io) then io=self.imorig
   if arg_present(id) then id=self.image
   if arg_present(off) then off=self.offset
   if arg_present(disp) then disp=self.dispsize
end

;;*************************End OverRiding methods******************************

;; flexibly convert device coordinates to pixel coordinates,
;; unless DEVICE is set, then return device coordinates of the central pixel.
;; FracDevice is for coorinates already in Fractional units (no center
;; of pixel calculation required).... e.g. pixel 1.544
function tvDraw::Convert, coord, DEVICE=dev, FRACTIONAL=frd, $
               ZOOM=zm, PAN=pn, OFFSET=off, SHOWING=sh, SINGLE_INDEX=si
   if arg_present(zm) then zm=self.zoom
   if arg_present(pn) then pn=self.pan
   if arg_present(off) then off=self.offset
   if n_elements(coord) eq 1 then $
    co=[coord mod self.size[0],coord/self.size[0]]  $
   else co=coord
   if keyword_set(dev) then begin 
      ;; Return device coordinates of a pixel
      if keyword_set(sh) then begin 
         ;; Report on pixels which are not now diplayed.
         if total(co lt self.offset)+ $
          total(co ge self.offset+self.dispsize) gt 0. then return,-1
      endif 
      inc=keyword_set(frd)?0.:.5 ;fractional pixels, not to center
      pix=floor(self.pan+self.zoom*(co+inc-self.offset))
      if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
   endif else begin 
      ;; Return pixel coordinates of a given device coordinate
      pix=self.offset+floor(float(co-self.pan)/self.zoom)
      if keyword_set(sh) then begin 
         if total(pix lt self.offset)+ $
          total(pix ge self.offset+self.dispsize) gt 0. then return,-1
      endif 
   endelse 
   if keyword_set(si) then return,pix[0]+pix[1]*self.size[0] else return,pix
end

pro tvDraw::SetWin, OLD=old
   if keyword_set(old) then dw=self.oldwin else $
    widget_control, self.wDraw, get_value=dw
   wset,dw
end

pro tvDraw::SetTitle,title
   wi=self.wDraw
   while wi gt 0 do begin 
      tlb=wi
      wi=widget_info(wi,/PARENT) 
   endwhile 
   widget_control, tlb, TLB_SET_TITLE=title
   return
end

function tvdraw_event,ev
   widget_control, ev.handler,get_uvalue=self
   return,self->Event(ev)
end

function tvDraw::Event,ev
   self->MsgSend,ev             ;broker out the event to message recipients
   return,-1                    ;no events returned
end

pro tvDraw::Postscript
  if NOT obj_valid(self.psconf) then begin 
     self.psconf=Obj_New("FSC_PSConfig")
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
;	Draw - Draw the relevant portion of the draw image
;	Redraw messages: type bits (can be or'ed together):
;		1: Redraw Display Image
;		2: Original Image Change
;		4: Position (size or offset or both) changed
;=============================================================================
pro tvDraw::Draw,type, NOREDRAW=nr,_EXTRA=e
   if NOT ptr_valid(self.image) then return
   widget_control, self.wDraw,get_value=win 
   if win ne -1 then wset,win else return ; not yet realized
   
   tvfast, TOP=self.top, BOTTOM=self.bottom, _EXTRA=e, $
    (*self.image)[self.offset[0]:self.offset[0]+self.dispsize[0]-1,$
                  self.offset[1]:self.offset[1]+self.dispsize[1]-1],zm,pan
   ;; grab a copy of it with a device copy
   wset,self.pixwin
   device,copy=[0,0,self.winsize, 0,0,win]
   self->SetWin
   
   if not keyword_set(nr) then begin 
      if n_elements(type) eq 0 then type=1b ;; default to redraw type
      if self.zoom ne 0. then sc=zm/self.zoom else sc=1.
      self.pan=pan & self.zoom=zm 
      ;; send a redraw message to all interested
      self->SendRedraw,type,SCALE=sc
   endif 

end

;=============================================================================
;	Plug: Register a plug-in module... called by tvPlug.
;=============================================================================
pro tvDraw::Plug, pl
   if total(obj_isa(pl,'tvPlug')) ne n_elements(pl) then return
   if ptr_valid(self.plug_list) then *self.plug_list=[*self.plug_list,pl] $
   else self.plug_list=ptr_new(pl)
end

;=============================================================================
;	SendRedraw - Send a redraw message of a given type to all
;	interested.  Types are bit combinations of:
;	    	1: Redraw Display Image
;		2: Original Image Reset
;		4: Position (size or offset or both) changed
;=============================================================================
pro tvDraw::SendRedraw,type, SCALE=scale
   if n_params() eq 0 then type=1b ;default to disp image redrawn
   if n_elements(scale) eq 0 then scale=1
   msg={TVDRAW_REDRAW, TYPE:type,SCALE:scale, PAN:self.pan,  $
        OFFSET:self.offset, ZOOM:self.zoom} 
   self->MsgSend,msg
end

pro tvDraw::Start
   self->Draw                   ;draw what we have
   if NOT ptr_valid(self.plug_list) then return
   
   wh=where(obj_valid(*self.plug_list),cnt)
   if cnt eq 0 then begin
      ptr_free,self.plug_list
      return
   endif 
   if cnt ne n_elements(*self.plug_list) then  $
    *self.plug_list=(*self.plug_list)[wh]
   ;; Initialize all the plug-ins
   for i=0,cnt-1 do (*self.plug_list)[i]->Start
end

;=============================================================================
;	Init - Initialize a tvDraw object, creating a draw widget and
;	initializing with provided data.
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
   if (self->ObjMsg::Init(_EXTRA=e) ne 1) then return,0 ;chain
  ;; set up the widget draw canvas .. unrealized as of yet
   self.wDraw=widget_draw(parent,/FRAME,EVENT_FUNC='tvdraw_event',  $
                          UVALUE=self,XSIZE=self.winsize[0], $
                          YSIZE=self.winsize[1],_EXTRA=e)
   window,/Free,XSIZE=self.winsize[0],YSIZE=self.winsize[1],/PIXMAP 
   self.pixwin=!D.Window
   s=size(imdata,/TYPE)
   if s ne 0 then begin         ;only set up if data is defined...
      case size(imdata,/TYPE) of
         10: begin              ;a pointer
            self.imorig=imdata
            s=size(*imdata)
         end
         
         else: begin
            self.imorig=ptr_new(imdata) ;actual data
            s=size(imdata)
         end
      endcase 
      self.dispsize=(self.size=[s(1),s(2)])
      self.image=ptr_new(*self.imorig)
   endif 
   ;; we don't draw it here since our widget is as of yet unrealized.
   return,1
end

pro tvDraw::Cleanup
   if ptr_valid(self.image) then ptr_free,self.image
   if ptr_valid(self.imorig) then ptr_free,self.imorig
   wdelete,self.pixwin
   obs=self->GetObj(*self.Msglist)
   obj_destroy,obs
   self->ObjMsg::Cleanup        ;cleans up the list
end

;=============================================================================
;	tvDraw__define.  Prototype the tvDraw class, including any
;	state elements that are necessary to describe the current data
;	displayed in the draw widget
;=============================================================================
pro tvDraw__define
   struct={tvDraw, $
           INHERITS ObjMsg, $
           wDraw:0L, $          ;the draw widget id
           oldwin:0, $          ;the old window which was set.
           psconf:obj_new(), $  ;the FSC_PSConfig object
           image:ptr_new(), $   ;the *displayed* image
           imorig:ptr_new(), $  ;saved original image, unmodified
           plug_list:ptr_new(), $ ;the list of plug-ins
           pixwin:0, $          ;pixmap window in which we save new image
           winsize:[0,0], $     ;size of window        
           size:[0,0], $        ;size of image in pixels
           zoom:0.0, $          ;(int or 1/int) amount draw image zoomed
           bottom:0, $          ;bottom color index to scale to
           top:0, $             ;top color index to scale to
           pan:[0,0], $         ;device pixel offsets for lower left corner
           offset:[0,0],$       ;the image pixeloffset of the lower left
           $                    ;    pixel drawn  [xoff,yoff]
           dispsize:[0,0]}      ;current [xsize,ysize] of displayed portion
                                ;    of array
   ;; Message Types for sending (other than widget_draw events)
   struct={TVDRAW_REDRAW, TYPE:0,SCALE:0.0, PAN:[0,0], OFFSET:[0,0], ZOOM:0.0} 
   return
end
