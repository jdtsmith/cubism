;+
; NAME:  
;
;    ObjMsg
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Superclass providing generic framework for inter-object,
;    message-based communications.  This documentation covers details
;    relevant only to programmers.
;
;    Messages are delivered to all relevant objects listed on a
;    "message recipient list" that each ObjMsg object maintains.
;    Messages are sent through each receiving objects' "Message"
;    method, which is to be overridden by derived classes to catch the
;    messages, and hopefully does useful things with thems.
;
;    ObjMsg enables an arbitrarily specifiable message-flow hierarchy,
;    rather than just the "up-the-widget-tree" event-passing paradigm
;    implicit in normal widget programming.  ObjMsg objects are free
;    to establish, break off, or change the content of communications
;    with each other dynamically.
;
;    A trivial example might be a display status line object which
;    monitors draw window motion events to update a position label, in
;    which case the status line is on the receiving end, and an object
;    with a draw widget embedded is on the sending end.
;
; CATEGORY:
;
;    Object-Based Message Passing and Inter-Communication
;
; METHODS:
;
;    Init:
;
;       CALLING SEQUENCE:
;
;          obj=obj_new("ObjMsg", [MSGLIST=])
;
;       INPUT KEYWORD PARAMETERS:
;
;          MSGLIST: The internal message list of a given ObjMsg object
;             can be passed directly with this keyword, or modified
;             with methods Signup and MsgRemove.
;
;    GetMsgObjs: 
;       
;       DESCRIPTION:
;
;          Return the message recipient list, with the option of
;          selecting only recipients of a given class.
;
;       CALLING SEQUENCE:
;
;          objs=obj->GetMsgObjs([CLASS=])
;
;       INPUT KEYWORD PARAMETERS:
;
;          CLASS: If set to a valid class (string), return objects on
;          the recipeient list which are members of that class (or any
;          of its subclasses).
;
;    GetObj: (override)
;       
;       DESCRIPTION:
;
;	   This routine allows the list to take any form, not just a list of
;          object references.  For instance, if a structure of the
;          form {MsgList,Object:obj_new(),flag:0b} was used for the
;          list element, GetObj should be along the lines:
;
;             function omInherit::GetObj,msglist
;                return,msglist.Object
;             end
;  
;       CALLING SEQUENCE:
;
;          objs=obj->GetObj(msglist)
;
;       INPUT PARAMETERS:
;
;          msglist: The list of one or more records of MSGLIST list
;             elements from which to extract the associated object
;             reference.
;
;       OUTPUT PARAMETERS:
;
;          The associated object reference for each passed list element.
;
;    GetProperty:
;       
;       DESCRIPTION:
;
;	   Get properties associated with the object.
;  
;       CALLING SEQUENCE:
;
;          obj->GetProperty,[MSGLIST=]
;
;       OUTPUT KEYWORD PARAMETERS:
;
;          MSGLIST: The objects internal message recipient list,
;             unmodified.
;
;    GetRecord:
;       
;       DESCRIPTION:
;
;	   Return a record from the message recipient list.
;  
;       CALLING SEQUENCE:
;
;          obj->GetRecord,obj
;
;       INPUT PARAMETERS:
;
;          obj: Which object you'd like the record for.
;
;       OUTPUT PARAMETERS:
;
;          The message list element corresponding to that object, if
;             it exists, or -1 if not.
;
;    Message: (override)
;       
;       DESCRIPTION:
;
;	   This is the main method which actually interprets and deals with
;          the messages sent.  If an object intends to receive
;          messages it should override this to make it useful.
;          Objects which do not intend to receive messages (but only
;          send them) need not override, since by default this method
;          simply discards any message sent.  See note above
;          concerning chaining.
;  
;       CALLING SEQUENCE:
;
;          obj->Message,msg
;
;       INPUT PARAMETERS:
;
;          msg: The message to send.
;
;    MsgSignup: (override)
;       
;       DESCRIPTION:
;
;          Add to or change the internal message recipient list.  In
;          subclasses, this should chain to the superclass's version,
;          ala:
;          
;	      pro omInherit::MsgSignup, msgList
;                self->ObjMsg::MsgSignup, msgList
;                other adding stuff... (e.g. check flags,turn
;                     ...               on widget events, etc.)
;             end
;
;       CALLING SEQUENCE:
;
;          obj->MsgSignup,msglist
;
;       INPUT PARAMETERS:
;
;          msglist: One or more list elements of the correct internal
;             form (which can vary by class), to be added to the list.
;             Only one entry per object is allowed on the list, and a
;             newer entry for the same object will displace an older
;             one.
;
;    MsgRemove: (override)
;       
;       DESCRIPTION:
;
;          Remove elements from the internal message recipient list.
;          In subclasses, this should chain to the superclass's
;          version, ala:
;          
;             pro omInherit::MsgRemove, msgList
;                other removal stuff...
;                self->ObjMsg::MsgRemove, msgList
;             end;
;
;       CALLING SEQUENCE:
;
;          obj->MsgRemove,msglist
;
;       INPUT PARAMETERS:
;
;          msglist: One or more list elements of the correct internal
;             form (which can vary by sub-class), to be removed from
;             the list.  Only one entry per object is allowed on the
;             list.
;
;    MsgSend:
;       
;       DESCRIPTION:
;
;           This method is called to actually deliver the message to
;           the intended recipient(s).  There is normally no need to
;           override it, except for special purpose delivery.  Notice
;           that since all messages pass through ObjMsg::MsgSend,
;           powerful debugging code can be included here which can
;           sample all message passing traffic in a given application
;           (see below for example).
;
;       CALLING SEQUENCE:
;
;          obj->MsgSend,msg
;
;       INPUT PARAMETERS:
;
;          msg: The message top be sent.
;
;    MsgSendWhich: (override)
;       
;       DESCRIPTION:
;
;	    This method determines which objects should be sent a given
;           message and returns them as a list.  By default all
;           objects on an object's recipient list are delivered the
;           message. It should be overridden in derived classes with
;           more complicated bookkeeping. For instance, if the "flag"
;           in the above example indicated only a few of the
;           recipients were interested in a certain kind of event,
;           MsgSendWhich should return only those objects from the
;           recipient list. It should use the objects own 'GetObj'
;           method to generate the list of object references.
;
;       CALLING SEQUENCE:
;
;          objs=obj->MsgSendWhich(msg)
;
;       INPUT PARAMETERS:
;
;          msg: The message being sent.
;
;       OUTPUT PARAMETERS:
;
;          A list of one or more object who wish to recieve the given
;          message.  Defaults to all on the Message Recipient List.
;
; NOTES:
;
;    Derived directly from the object messaging class of the same name
;    by J.D. Smith.
;
;    ObjMsg defines a common framework for event-driven object
;    communication.  The events will include those which arise from
;    widget activity within the objects, but the formalism is
;    extensible to any generic 'object events'.  Both kinds of events
;    are encapsulated by the term 'messages', and are referred to as
;    "object messages" when handled by the protocol defined in this
;    class.  ObjMsg works by maintaining a pointer to a resizable list
;    of current ObjMsg-derived objects to which to deliver messages.
;    Each object maintains its own separate list, and can send any
;    qualified message to the valid objects on its list.  Objects
;    which are no longer valid are automatically removed from the
;    list.  The message recipient list elements may be of any data
;    type, including structures or objects themselves, though it can
;    be (and often is) as simple as a list of recipient object
;    references.  Methods to override to customize ObjMsg objects are
;    suggested above with (override).  A given object may only have
;    one entry in the list at a time, and thus removal will be based
;    only on the object reference, and not other information provided
;    within the recipient list elements.
;
; EXAMPLE: 
;
;    Since ObjMsg is usually invoked from derived classes, this
;    examples section is somewhat contrived.
;    
;    om=obj_new('ObjMsg',MsgList=ml) Create a new ObjMsg object with
;    initial list of recipients ml.  om->MsgRemove,om2
;    om->MsgSignup,[om2,om3,om4,om5] ; add some to the message list
;    om->MsgSend,message om->GetProperty,MsgList=mlist
;
; MODIFICATION HISTORY:
; 
;    2001-08-17 (J.D. Smith): Sped up MsgRemove with array
;       formulation.  Minor other changes.
;
;    2001-07-30 (J.D. Smith): Migrated directly from SCORE-era source.
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

;=============================================================================
;      MsgSend - Send a message event to all listed 
;      message recipients.  Can be any valid widget event,
;      or, more generally, any 'Object Event' structure.
;      The term 'message' encapsulates both flavors.
;============================================================================= 
pro ObjMsg::MsgSend,msg
  ;; don't do a MsgListClean -- for speed reasons... this can be done
  ;; during all other list operations.
  if NOT ptr_valid(self.MsgList) then return 
  sendlist=self->MsgSendWhich(msg)
  if size(sendlist,/TYPE) ne 11 then return ;an object list *not* returned
  
  ;; Compile something in here like this to debug...
;    if tag_names(msg,/STRUCTURE_NAME) eq 'TVDRAW_POSTDRAW' then begin 
;       print,obj_class(self),': sending to: ',sendlist, ' message'
;       help,/st,msg
;       help, /TRACEBAC
;  endif 
  
                                ;send all the messages
  for i=0,n_elements(sendlist)-1 do sendlist[i]->Message,msg  
end

;=============================================================================
;      MsgSendWhich - Determine which objects from the recipient list
;      are to be sent the given message, and return that list of objects.
;============================================================================= 
function ObjMsg::MsgSendWhich,msg
  return,self->GetObj(*self.MsgList) ;defaults to all of them !
end

;=============================================================================
;      GetProperty - Get Current Message list
;=============================================================================
pro ObjMsg::GetProperty,MsgList=msglist
  if arg_present(msglist) then msglist=self.MsgList 
end

;=============================================================================
;      GetRecord - Return Message Recipient record for obj, if exists.
;                  Otherwise return -1.
;=============================================================================
function ObjMsg::GetRecord,obj
  if self->MsgListLen() eq 0 then return,-1
  wh=where(self->GetObj(*self.MsgList) eq obj,cnt)
  if cnt eq 0 then return,-1 else return,(*self.MsgList)[wh[0]]
end

;=============================================================================
;      MsgListLen - Return length of current message list.
;=============================================================================
function ObjMsg::MsgListLen
  if ptr_valid(self.MsgList) then return,n_elements(*self.MsgList) else  $
     return,0
end

;=============================================================================
;      MsgListClean - Remove any danglers from message list, return 0 if
;      list is no longer defined, 1 if it is defined.
;=============================================================================
function ObjMsg::MsgListClean
  if ptr_valid(self.MsgList) then begin
     list=self->GetObj(*self.MsgList)
     wh=where(obj_valid(list),cnt)
     if cnt gt 0 then begin     ;some are valid
        ;; find the ObjMsg's on the list.
        wh2=where(obj_isa(list[wh],'ObjMsg'),cnt)
        if cnt gt 0 then begin  ;we have some valid ObjMsg objects.
           ;; remove danglers
           if cnt ne n_elements(list) then  $
              *self.MsgList=(*self.MsgList)[wh[wh2]]
        endif else begin 
           ptr_free,self.MsgList
           return, 0
        endelse
     endif else begin
        ptr_free,self.MsgList
        return, 0
     endelse
  endif else return,0
  return,1                      ;only get here if a valid list remains.
end


;=============================================================================
;      MsgSignup - Add an ObjMsg inherited object to the message
;                  recipient list, or change existing list members.
;                  Only one list entry per object is allowed... if
;                  more than one entry is passed for a given object,
;                  the last in the list is used. As usual, the list
;                  elements can have any type, and the function GetObj
;                  is provided to extract the object reference.
;============================================================================= 
pro ObjMsg::MsgSignup, owObjlist,_EXTRA=e
  pvalid=self->MsgListClean()   ;clean and check for a valid list
  no=n_elements(owObjlist)
  if no ne 0 then addobs=self->GetObj(owObjlist) else return
  oi=Obj_Isa(addobs,'ObjMsg')
  bad=where(oi eq 0,COMPLEMENT=good,cnt)
  if cnt gt 0 then begin
     if cnt gt 1 then m='s are' else m=' is'
     print,owObjlist[bad]
     message,'Specified object'+m+' not derived from ObjMsg... Not added',$
             /INFO
  endif
  if good[0] eq -1 then return 
  addobs=addobs[good]
  if NOT pvalid then self.MsgList=Ptr_New(owObjlist[good]) else begin 
     listobs=self->GetObj(*self.MsgList)
     for i=0,n_elements(addobs)-1 do begin ; add or modify an existing list
        wh=where(listobs eq addobs[i],cnt) ;object already in the list?
        ;; *** Append recipient, or replace with possibly new 
        ;; here we trust that wh is a single element... no object ever gets
        ;; two entries in the list, so this should always be true
        if cnt eq 0 then begin 
           listobs=[listobs,addobs[i]]
           *self.MsgList=[*self.MsgList,owObjlist[i]] 
        endif else (*self.MsgList)[wh[0]]=owObjlist[i] 
     endfor
  endelse  
end

;=============================================================================
;      MsgRemove - Remove an ObjMsg inherited object from the
;      message recipient list
;============================================================================= 
pro ObjMsg::MsgRemove, rmobjs
  if NOT self->MsgListClean() then return ;none left to remove !
  listobjs=self->GetObj(*self.MsgList)
  wh=where_not_array([rmobjs],[listobjs],cnt)
  if cnt eq 0 then ptr_free,self.MsgList else *self.MsgList=(*self.MsgList)[wh]
end

;=============================================================================
;      Message - The actual routine used for recieving messages.  This
;      routine is meant to be overridden by subclasses which accept messages
;      to do something useful with them.
;=============================================================================
pro ObjMsg::Message, ev
  ;; Efficient ObjMsg derivatives should limit the number of possible
  ;; messages sent. It is the programmer's responsibility to know
  ;; which messages are sent from an object, and to send appropriate
  ;; messages of its own.  This may involve a more complex message
  ;; recipient list.  For instance, an ObjMsg derivative which
  ;; contains a draw widget might summarize the different possible
  ;; messages (e.g draw events) with a Long int flag, allow requests
  ;; for any combination of those possible messages, and turn on and
  ;; off the associated widget events in accordance with the status
  ;; of that event request list.
end

;=============================================================================
;      GetObj -- a routine to return the object reference given the list
;      msglist.  This method is meant to be overriden to allow
;      for lists of (for instance) structures or other complicated
;      data types. 
;============================================================================= 
function ObjMsg::GetObj,msglist
  return, msglist
end

;=============================================================================
;      GetMsgObjs -- a routine to return objects on the msglist
;      If the keyword "CLASS" is specifyed, only objects of that class
;      on the list are returned.  If there's noone on the list, -1 is
;      returned.
;============================================================================= 
function ObjMsg::GetMsgObjs, CLASS=class
  if NOT self->MsgListClean() then return,-1 ;none on here
  objs=self->GetObj(*self.MsgList) ;get the objects on our list.
  if size(class,/TYPE) eq 7 then begin
     wh=where(obj_isa(objs,class),cnt)
     if cnt eq 0 then return,-1 else return,objs[wh]
  endif 
  return,objs
end

;=============================================================================
;      Init - Initialize the object, and add to the list.
;============================================================================= 
function ObjMsg::Init,MsgList=ml
  s=n_elements(ml)
  if s eq 0 then return,1
  objlist=self->GetObj(ml)
  wh=where(obj_isa(objlist,'ObjMsg') eq 0, cnt)
  if cnt gt 0 then begin
     if cnt gt 1 then m='s are' else m=' is'
     print,objlist[wh]
     message,'Specified object'+m+' not derived from ObjMsg.'
  endif 
  self.MsgList=ptr_new(ml)      ;allocate and assign the list
  return,1
end

;=============================================================================
;      Cleanup - Cleanup the object, destroy the message list.
;============================================================================= 
pro ObjMsg::Cleanup
  if self->MsgListClean() then begin
     ptr_free,self.MsgList
  endif    
end

;=============================================================================
;      __define - define the ObjMsg Class structure.
;============================================================================= 
pro ObjMsg__define
  ;; define a class with a generic, redefineable,  message recipient list, for
  ;; receipt and delivery of object messages.
  struct={ObjMsg, $
          MsgList:Ptr_New()}    ;the list -- elements are of unspecified type
end
