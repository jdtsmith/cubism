;+
; NAME:  
;
;    ObjReport
;
; DESCRIPTION:
;    
;    A pure helper class for reporting errors and warnings in object
;    programs either in a GUI pop-up, or on the command line.
;    
; CATEGORY:
;
;    Error and Warning Messages
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
;    Init:
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('DerivedReporterClass',arg1,..)
;
;    Error|Warning|Info:
;  
;	DESCRIPTION:
;
;          Writes an message to the command line or as a GUI popup,
;          and (for Error) stops execution if command-line or throws
;          an error/returns to the $MAIN$ level when using a widget.
;	
;       CALLING SEQUENCE:
;
;          obj->Error, err_string,[PARENT=,TITLE=]
;
;       INPUT PARAMETERS:
;
;          err_string: The error string to post or popup.
;
;       INPUT KEYWORD PARAMETERS:
;
;          PARENT: If set to a valid widget, use this as the popup's
;             parent, instead of the test widget returned by
;             TestWidget().
;
;          TITLE: The optional title to give the widget (if it's a
;             widget).
;
;    Methods to Override:
;
;       TestWidget: Returns a widget ID to test for validity in order
;          to decide how to report errors. Defaults to
;          (*self.wInfo).Base.
;
;       useWidget: Returns a boolean which determines whether to use
;          the widget popup or not.  Can be overridden to default to a
;          fixed behaviour, but normally this isn't necessary.
;
; NOTES:
;  
;    The desired behavior of error or warning messages depends on the
;    context in which they're created.
;
;    Errors:
;
;    In widget programs, a modal pop-up error message, followed by a
;    return, either to the command line (for non-blocking widgets), or
;    some previously established catch location (for blocking widgets)
;    is often desired, so that the program can continue to run.  On
;    the command line, such an error might be fatal (for instance if
;    run as part of a script), and requires a full halt and return to
;    the $MAIN$ level.
;
;    Info/Warnings:
;
;    On both the command line and in a widget program, warnings and
;    info should not halt execution, but instead just present the user
;    with the warning info and go on.  This is accomplished with a
;    non-blocking pop-up for widget programs, or message,/CONTINUE, or
;    message,/INFO in non-widget programs.
;
;    The same program can be used in both widget and non-widget modes.
;    The function method TestWidget returns the main widget, over
;    which the error will be situated, and whose existence and
;    validity serves to discriminate between widget mode and
;    non-widget (command-line) mode.  If this widget id is not
;    accessible or valid, the message will be printed to the command
;    line.
;
;    Blocking vs non-blocking widgets:
;
;    In widgets which do not block the command line (called with
;    `xmanager, /no_block'), the event loop is always running, and a
;    simple "retall" at any error or stop point will return to the
;    running loop.  This is what ObjReport does for non-blocking
;    widget programs.  Assuming the error hasn't left your program
;    running in a broken state, this is a perfectly good way to
;    continue after an error.  For blocking widgets, the call to
;    XManager does *not* return while the widget is running, and the
;    event loop runs inside of XManager.  Using "retall" in such a
;    widget would return right past XManager and its event loop: your
;    widget would be dead.  In that case, you'll need to setup a catch
;    statement which looks for the message 'OBJREPORT-ERROR', and
;    handles it to keep the code running.  The natural place for this
;    to occur is at the beginning of the event handler or handlers
;    (the first place your code is entered via XManager).  The special
;    include file objreport_catch.pro inserts this code, and can be
;    included with "@objreport_catch" at the top of your event handler
;    procedure.  Another option is to include it before calling
;    XManager in your main level routine, and, if an error occurs,
;    call XManager again to restart the program.  This must be coded
;    with care, since it can easily result in an infinite
;    message/catch loop.  An example:
;
;         catch, err
;         if err ne 0 then begin
;            main_object->Error,!ERROR_STATE.MSG
;            ;; xmanager errors are fatal
;            if stregex(!ERROR_STATE.MSG,'^XMANAGER:',/BOOLEAN) then return
;            xmanager,'mypro',base,/BLOCK
;         endif
;         xmanager,'mypro',base,/BLOCK
;    
;    Composited objects and routines:
;
;    Objects can contain other objects in their class heirarchy, some
;    of which may not have any widget component.  In addition, they
;    may call routines which know nothing about ObjReport'ing, but
;    whose errors the main object would like to trap.  In this case,
;    as for blocking widgets, a catch mechanism must be put in place
;    to report these errors.  Unfortunately, warnings and info, which
;    do not halt execution or raise an error exception, cannot be
;    trapped in this way.  In addition, for non-blocking widgets, the
;    catch cannot occur in the main routine (which returns
;    immediately), and therefore must be included at the beginning of
;    an event handler.
;
; INHERITANCE TREE:
;
;    ObjReport--+
;                \
;       Parent1-->DerivedClass
;
; MODIFICATION HISTORY:
;    
;    2002-09-11 (J.D. Smith): Written.
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;  Boston, MA 02111-1307, USA.
;
;##############################################################################

;=============================================================================
; Error - Signal an error, returning all the way to the command line
;         or to the widget loop catch, if using a widget.
;=============================================================================
pro ObjReport::Error,msg,_EXTRA=e
  if self->useWidget() then begin 
     self->Popup,msg,/Error,PARENT=parent,_EXTRA=e
     if self->isBlocking() then begin
        ;; Send a message to be caught by the established OBJREPORT catch
        message,'OBJREPORT-ERROR',/NOPRINT
     endif else retall          ;the main level *is* the widget loop
  endif else message,msg
  !ERROR
  catch,
  return
end

;=============================================================================
; Warning - Post a warning but otherwise continue
;=============================================================================
pro ObjReport::Warning,msg,PARENT=parent,TITLE=title
  if self->useWidget() then $
     self->Popup,msg,/WARNING,PARENT=parent,TITLE=title  $
  else self->Report,msg,/WARNING,TITLE=title
end 

;=============================================================================
; Info - Post some info but otherwise continue
;=============================================================================
pro ObjReport::Info,msg,PARENT=parent,TITLE=title
  if self->useWidget() then $
     self->Popup,msg,/INFO,PARENT=parent,TITLE=title  $
  else message,msg,/INFO,TITLE=title
end 

;=============================================================================
; Popup - Create a blocking or non-blocking popup with the associated msg
;=============================================================================
pro ObjReport::Popup,msg,INFO=info,WARNING=warning,ERROR=error,PARENT=parent, $
                     TITLE=title
  title_type=keyword_set(error)?"error":keyword_set(warning)?"warning": $
             keyword_set(info)?"info":"error"
  if n_elements(title) eq 0 then title=obj_class(self)+' '+title_type
  if n_elements(parent) eq 0 then parent=self.widget else $
     if NOT widget_info(parent,/VALID_ID) then parent=self.widget
  if keyword_set(info) or keyword_set(error) then $
     void=dialog_message(msg,DIALOG_PARENT=parent, $
                         INFORMATION=keyword_set(info) , $
                         ERROR=keyword_set(error),TITLE=title) $
  else void=dialog_message(msg,DIALOG_PARENT=parent,TITLE=title)
end

;=============================================================================
; Report - Print messages to the command line
;=============================================================================
pro ObjReport::Report,msg,INFO=info,WARNING=warning,ERROR=error,TITLE=title
  title_type=keyword_set(error)?"error":keyword_set(warning)?"warning": $
             keyword_set(info)?"info":"error"
  if n_elements(title) eq 0 then title=obj_class(self)+' '+title_type
  printmsg=title+': '+msg
  if keyword_set(warning) then message,printmsg,/CONTINUE,/NOPREFIX $
  else if keyword_set(error) then message,printmsg,/NOPREFIX $
  else if keyword_set(info) then message,printmsg,/INFORMATIONAL,/NOPREFIX
  return
end

;=============================================================================
; TestWidget - Return the widget to test the validity of to decide
;              between graphical and text-based errors/warnings.
;              Should be overridden.  By default, looks for a class
;              tag "wInfo" which is a ptr to an info structure with a
;              "Base" tag.  
;=============================================================================
function ObjReport::TestWidget
  catch, err
  if err ne 0 then $
     message, "No class tag pointer `wInfo' with tag `Base' found.  " + $
              "Change/Override TestWidget Method?"
  if ptr_valid(self.wInfo) then return,((*self.wInfo).Base)
end

;=============================================================================
; isBlocking - Is the widget part of a blocking widget, or not?
;=============================================================================
function ObjReport::isBlocking
  return,widget_info(self.widget,/XMANAGER_BLOCK)
end

;=============================================================================
; useWidget - Use a widget for popup messages?
;=============================================================================
function ObjReport::useWidget
  self.widget=self->TestWidget() ;re-get the widget, it could be changing
  return,widget_info(self.widget,/VALID_ID)
end

;=============================================================================
; ObjReport - Special error and warning message helper class
;=============================================================================
pro ObjReport__define
  class={ObjReport, $
         widget:0L, $           ;the widget, which, if valid, will cause
                                ;graphicsl messages to appear
         }
end
