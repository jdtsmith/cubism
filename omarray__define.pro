;+
; NAME:  
;
;    OMArray
;
; CONTACT:
;
;    UPDATED VERSIONs of SMART and more information can be found at:
;       http://isc.astro.cornell.edu/smart/download
;
; DESCRIPTION:
;    
;    Provides a specific inter-object messaging framework, as a pure
;    helper class for ObjMsg.  Whereas the latter allows the message
;    recipient list to have any form, OMArray fixes the format of each
;    list element to be a structure of type:
;
;    		{Obj:obj_new(), recip:bytarr(N)},
;    		
;    where `Obj' is the object recipient, and `recip' is a boolean
;    array of bytes which specifies which messages that object wishes
;    to receive.  An auxiliary list, MsgNames, specifies each of the N
;    message types by name.  The helper class OMArray is most useful
;    for objects which send multiple messages, and whose recipients
;    either want the message, or not (a boolean question).
;
;    The primary beneficial feature provided by OMArray is easy,
;    keyword-driven message signup/removal ala:
;    
;               object->MsgSignup,self,/THEMESSAGE
;    
; CATEGORY:
;
;    Object-Based Message Passing and Inter-Communication
;    	
; METHODS:
;
;    MsgSendWhich:
;       
;       DESCRIPTION:
;          
;          This override of the ObjMsg version (which must return
;          those objects requesting the passed message) presumes that
;          all messages are named structures, and that, for each
;          message sent, there will be a matching value to the
;          structure name in the MsgNames listing (though see TYPE).
;	
;       CALLING SEQUENCE:
;
;          objs=obj->MsgSendWhich(msg)
;
;       INPUT PARAMETERS:
;
;          msg: The message being sent.
;
;    MsgSetup: 
;       
;       DESCRIPTION:
;          
;          Used to fill the MsgNames array, which lists all message
;          types the object can send.  Must be called before MsgSend
;          for messages to be forwarded successfully.  Typically
;          called in Init().
;
;       CALLING SEQUENCE:
;
;          obj->MsgSetup,msgnames
;
;       INPUT PARAMETERS:
;
;          msgnames: A (string) list of message names.  All capitals
;             only.  Be sure to use distinct, descriptive names with
;             clear meaning (e.g. "MYOBJ_WAKEUP" instead of "WAKE").
;             Often the class name is used as part of the message
;             name, but this is not required.
;
;    MsgSignup: 
;       
;       DESCRIPTION:
;          
;          An override of the ObjMsg version, this method adds a given
;          object or objects to the message recipient list, and sets
;          message receive preferences using keywords (a.k.a "which
;          messages do they want?").  It presumes the specified
;          structure format for MsgList elements.  If an object
;          already exists on the MsgList, its receive preferences are
;          modified in place.  E.g. if obj1 was signed up for
;          MESSAGE1, obj1->MsgSignup,/MESSAGE2 would leave it signed
;          up for MESSAGE1, in addition to MESSAGE2.
;
;       CALLING SEQUENCE:
;
;          obj->MsgSignup,objs,[/ALL,/NONE,_EXTRA=]
;
;       INPUT PARAMETERS:
;
;          objs: The object(s) to add to the list.
;
;       INPUT KEYWORD PARAMETERS:
;
;          ALL: Enable reception of *all* message types.
;
;          NONE: Enable reception of *no* message types.
;             Effectively, clears the MsgList record so that no
;             messages are received.  May be (and usually is) combined
;             with additional keywords.
;
;          _EXTRA: All additional unnamed keywords should correspond
;             exactly to a message name, no abbreviations
;             allowed. N.B. MISSPELLED OR NON-SETUP KEYWORDS ARE
;             SILENTLY IGNORED.  CARE MUST BE TAKEN TO ENSURE THE
;             CORRECT MESSAGE NAMES ARE USED.
;
;    IsSet:
;       
;       DESCRIPTION:
;
;          Function returns 1 if a given message is requested by at
;          least on of the objects on the recipient list, 0 otherwise.
;
;       CALLING SEQUENCE:
;
;          set=obj->IsSet(msgname)
;
;       INPUT PARAMETERS:
;
;          msgname: The message to test if set.
;
;       OUTPUT PARAMETERS:
;
;          set: A boolean value indicating whether the given message
;             is requested by at least one object.
;          
; NOTES:
;
;    There is no such thing as an "OMArray" object.  OMArray is a pure
;    helper class designed to work with ObjMsg, and it will only work
;    properly if INHERIT'ed in a class definition which also contains
;    ObjMsg on its inheritance path.  It must also be included in a
;    class definition *before* the chain which contains ObjMsg, e.g.
;
;    	st={OMfoo,INHERITS OMArray, INHERITS ObjMsg, data:0}
;
;    An object using the OMArray helper class can pass messages to
;    plain ObjMsg-derived objects freely.  Its only addition to ObjMsg
;    is to fix the form of the message list record, which makes many
;    operations more convenient for a wide variety of message passing
;    objects.  For those OMArray methods which override ObjMsg methods
;    (currently only MsgSignup and MsgSendWhich), if you'd like to
;    chain up to the superclass, be sure to use,
;    e.g. "self->OMArray::MsgSignup,...", and otherwise use the
;    standard "ObjMsg".  (This slight hassle would be eliminated if
;    IDL had a SUPER psuedo-class).
;
;    N.B. DON'T FORGET TO CHAIN TO OMARRAY::CLEANUP IN THE DERIVED
;    CLASS' CLEANUP, IN ADDITION TO ANY OTHER PARENT CLASS.
;
; INHERITANCE TREE:
;
;    OMArray--+  (helper class)
;              \
;       ObjMsg----->Derived Class
;     
; EXAMPLE:
;
;    N.B.: Since OMArray is by necessity always invoked from derived
;    classes which also have ObjMsg among its superclasses, this
;    examples section is somewhat contrived.
;
;    oma=obj_new('OMArray_Derived_Class')
;    oma->MsgSetup,['WIDGET_TEXT','COLOR_NOTIFY','ANOTHER_MESSAGE_TYPE']
;    oma->MsgSignup,recipient_object,/COLOR_NOTIFY
;    oma->MsgSignup,recipient_object2,/ALL,WIDGET_TEXT=0
;
;    A more relevant example:
;
;    function Foo::Init
;      if self->ObjMsg::Init() eq 0 then return,0
;      self->MsgSetup,['MSG1']
;      return,1
;    end
;    
;    pro foo__define
;      st={FOO,INHERITS OMarray, INHERITS ObjMsg, data:0}
;    end
;
;    self.oma=obj_new('FOO')
;    self.oma->MsgSignup,self,/MSG1
;
; MODIFICATION HISTORY:
;
;    2001-10-01 (J.D. Smith): Written, as initial ObjMsg helper class.
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
;      GetObj - The OMArray MsgList has a fixed format
;=============================================================================
function OMArray::GetObj,r
  return,r.Obj
end 

;=============================================================================
;      MsgSendWhich - A default selector which just looks at the
;                     message type, and matches it inside the MsgNames
;                     list.  Exact matches are then searched for
;                     selected values in the MsgList, and those
;                     objects are returned.  
;=============================================================================
function OMArray::MsgSendWhich, msg
  if self->MsgListLen() eq 0b or ptr_valid(self.MsgNames) eq 0b then $
     return, -1
  if n_elements(type) eq 0 then type=tag_names(msg,/STRUCTURE_NAME)
  wh=where(*self.MsgNames eq type,cnt)
  if cnt eq 0 then return,-1
  wh=where((*self.MsgList).recip[wh[0],*],cnt)
  if cnt eq 0 then return,-1 else return,(*self.MsgList)[wh].Obj
end

;=============================================================================
;      MsgSetup - Setup the messages we can provide.
;=============================================================================
pro OMArray::MsgSetup, msgnames
  if ptr_valid(self.MsgNames) then *self.msgnames=[msgnames] $
  else self.MsgNames=ptr_new([msgnames])
end

;=============================================================================
;      IsSet - Query if a specific message is set.
;=============================================================================
function OMArray::IsSet,msgname
  if ptr_valid(self.MsgNames) eq 0 or self->MsgListClean() eq 0 then return,0
  wh=where(*self.MsgNames eq msgname,cnt)
  if cnt eq 0 then return,0
  return,total((*self.MsgList).recip[wh[0],*]) gt 0.
end

;=============================================================================
;      MsgSignup - Sign an object or objects up for messages specified
;                  by name as keyword parameters, using the same names
;                  setup in MsgSetup (which must be called before any
;                  signup occurs).  If ALL is set, turn on all
;                  messages.  If NONE is set, turn off all messages.
;                  These can be combined with specific additional
;                  message names, e.g.:
;;                    obj->MsgSignup,recip,/NONE,/MSGTYPE1
;=============================================================================
pro OMArray::MsgSignup, objs, ALL=all_on,NONE=all_off,_EXTRA=set_source
  if n_elements(objs) eq 0 then $
     message,'No objects specified for signup.'
  if ptr_valid(self.MsgNames) eq 0 then return
  all_off=keyword_set(all_off) 
  all_on=keyword_set(all_on) 
  
  if n_elements(set_source) ne 0 then begin 
     tags=tag_names(set_source)
     nt=n_tags(set_source)
     loc=make_array(nt,/INT,VALUE=-1)
     ;; Find exact matches for the tag names.
     for i=0,nt-1 do begin 
        wh=where(*self.MsgNames eq tags[i],cnt)
        if cnt gt 0 then loc[i]=wh[0]
     endfor
  endif else nt=0
  
  ;; Construct the message list elements from the keywords passed.
  for i=0,n_elements(objs)-1  do begin
     in_list=0
     if ptr_valid(self.MsgList) then rec=self->GetRecord(objs[i])
     if size(rec,/TYPE) ne 8 then $ ; No record, make one
        rec={Obj:objs[i],recip:bytarr(n_elements(*self.MsgNames))} $
     else in_list=1
     if all_on then rec.recip=1b else if all_off then rec.recip=0b
     for i=0,nt-1 do $
        if loc[i] ne -1 then rec.recip[loc[i]]=keyword_set(set_source.(i))
     ;; delete empty recipient lists
     ;if array_equal(rec.recip,0b) then begin 
     ;   if in_list then self->MsgRemove,objs[i]
     ;   continue
     ;endif
     if n_elements(recs) eq 0 then recs=[rec] else recs=[recs,rec]
  endfor
  if n_elements(recs) eq 0 then return
  self->ObjMsg::MsgSignup, recs   ;chain to superclass to actually add them
end

;=============================================================================
;      Cleanup
;=============================================================================
pro OMArray::Cleanup
  ptr_free,self.MsgNames
end

pro OMArray__define
  st={OMArray, $                ;INHERIT nothing... a pure helper class.
      MsgNames:Ptr_New()}       ;the names of all message types we can send
end
