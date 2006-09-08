;+
; NAME:  
;
;    ObjReport
;
; DESCRIPTION:
;    
;    A pure helper class for reporting errors, warnings, and sttatus
;    messages in object programs either in a GUI pop-up, or on the
;    command line.
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
;          and (for Error) stops execution if command-line or, for a
;          GUI, throws an error (blocking widget)/returns to the
;          $MAIN$ level (non-blocking widgets).  Can be used in both
;          command-line and GUI modes.
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
;             ReportWidget().
;
;          TITLE: The optional title to give the widget (if it's a
;             widget).
;
;    Methods to Override:
;
;       ReportWidget: Returns a widget ID to test for validity in
;          order to decide how to report errors. Defaults to
;          (*self.wInfo).Base.  
;
;       StatusWidget: The widget ID of a text or label widget to
;          display status messages to.
;
;       IsWidget: Returns a boolean which determines whether to use
;          the widget popup or not.  Can be overridden to default to a
;          fixed behaviour, but normally this isn't necessary, as long
;          as ReportWidget() returns a widget ID which can be tested
;          for validity.  May also be useful directly in your method
;          classes for testing whether a widget is running or not.
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
;    The function method ReportWidget returns the main widget, over
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
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;=============================================================================
;  SetProperty 
;=============================================================================
pro ObjReport::SetProperty,REPORT_TITLE_BASE=tb,REPORT_QUIET=qt
  if n_elements(tb) ne 0 then self.or_title_base=tb
  if n_elements(qt) ne 0 then self.or_quiet=keyword_set(qt) 
end


;=============================================================================
;  SetProperty 
;=============================================================================
pro ObjReport::GetProperty,REPORT_TITLE_BASE=tb,REPORT_STATUS=status
  if arg_present(tb) then tb=self.or_title_base
  if arg_present(status) then status=self.or_status
end

;=============================================================================
;  Error - Signal an error, returning all the way to the command line
;          or to the widget loop catch, if using a widget.
;=============================================================================
pro ObjReport::Error,msg,RETURN_ONLY=ro,_REF_EXTRA=e
  on_error,2
  if self->IsWidget() then begin 
     self->orPopupReport,msg,/Error,PARENT=parent,_EXTRA=e
     if self->IsBlocking() then begin
        ;; Send a message to be caught by the established OBJREPORT catch
        message,'OBJREPORT-ERROR',/NOPRINT,LEVEL=-1
     endif else if keyword_set(ro) then return else retall 
  endif else self->orCommandLineReport,msg,/ERROR
end

;=============================================================================
;  Warning - Post a warning but otherwise continue
;=============================================================================
pro ObjReport::Warning,msg,PARENT=parent,_REF_EXTRA=e
  if self->IsWidget() then $
     self->orPopupReport,msg,/WARNING,PARENT=parent,_EXTRA=e $
  else self->orCommandLineReport,msg,/WARNING,_EXTRA=e
end 

;=============================================================================
;  Info - Post some info but otherwise continue
;=============================================================================
pro ObjReport::Info,msg,PARENT=parent,_REF_EXTRA=e
  if self->IsWidget() then $
     self->orPopupReport,msg,/INFO,PARENT=parent,_EXTRA=e $
  else self->orCommandLineReport,msg,/INFO,_EXTRA=e
end 

;=============================================================================
;  Status - Post a single line of status information
;=============================================================================
pro ObjReport::Status,msg,CLEAR=clear
  wStatus=self->StatusWidget()
  if keyword_set(clear) then self.or_status='' else self.or_status=msg[0]
  if widget_info(wStatus,/VALID) then $
     widget_control, wStatus, SET_VALUE=self.or_status $
  else if ~keyword_set(clear) then $
     self->orCommandLineReport,self.or_status,/STATUS
end

;=============================================================================
;  orPopupReport - Create a blocking or non-blocking popup with the
;                  associated msg
;=============================================================================
pro ObjReport::orPopupReport,msg,INFO=info,WARNING=warning,ERROR=error, $
                             PARENT=parent,TITLE=title,SCROLL=scroll,$
                             RESULT=result,_EXTRA=e
  title_type=keyword_set(error)?"error":keyword_set(warning)?"warning": $
             keyword_set(info)?"info":"error"
  if strlen(self.or_title_base) eq 0 then $
     self.or_title_base=obj_class(self)
  if n_elements(title) eq 0 then title=self.or_title_base+' '+title_type
  if n_elements(parent) eq 0 then parent=self.or_widget else $
     if NOT widget_info(parent,/VALID_ID) then parent=self.or_widget
  if keyword_set(scroll) then begin 
     xdisplayfile,'',TEXT=msg,/MODAL,GROUP=parent,TITLE=title, $
                  DONE_BUTTON='  OK  ',WIDTH=max(strlen(msg)), $
                  HEIGHT=n_elements(msg)<10, _EXTRA=e
  endif else begin 
     if keyword_set(info) or keyword_set(error) then $
        result=dialog_message(msg,DIALOG_PARENT=parent, $
                            INFORMATION=keyword_set(info) , $
                            ERROR=keyword_set(error),TITLE=title,_EXTRA=e) $
     else result=dialog_message(msg,DIALOG_PARENT=parent,TITLE=title,_EXTRA=e)
  endelse 
end

;=============================================================================
;  orCommandLineReport - Print messages to the command line
;=============================================================================
pro ObjReport::orCommandLineReport,msg,INFO=info,WARNING=warning, $
                                   ERROR=error,STATUS=status,TITLE=title
  on_error,2
  if self.or_quiet then return
  case 1 of 
     keyword_set(error): title_type="error"
     keyword_set(warning): title_type="warning"
     keyword_set(info): title_type="info"
     keyword_set(status): title_type="status"
     else: title_type="error"
  endcase 
  
  if strlen(self.or_title_base) eq 0 then $
     self.or_title_base=obj_class(self)  
  if n_elements(title) eq 0 then title=self.or_title_base+' '+title_type
  if n_elements(msg) gt 1 then begin
     newmsg=msg[0]
     for i=1,n_elements(msg)-1 do newmsg=newmsg+string(10b)+msg[i]
     msg=newmsg
  endif 
  printmsg=title+': '+msg
  if keyword_set(warning) then message,printmsg,/CONTINUE,/NONAME,LEVEL=-2 $
  else if keyword_set(error) then message,printmsg,/NONAME,LEVEL=-2 $
  else if keyword_set(info) || keyword_set(status) then $
     message,printmsg,/INFORMATIONAL,/NONAME,LEVEL=-2
  return
end


;=============================================================================
;  ReportWidget - Return the widget to test the validity of to decide
;                 between graphical and text-based errors/warnings.
;                 Should be overridden.  By default, looks for a class
;                 tag "wInfo" which is a ptr to an info structure with
;                 a "Base" tag.
;=============================================================================
function ObjReport::ReportWidget
  catch, err
  if err ne 0 then begin 
     catch,/cancel
     message, "No class tag pointer `wInfo' with tag `Base' found.  " + $
              "Override the ReportWidget Method?"
  endif
  if ptr_valid(self.wInfo) then return,((*self.wInfo).Base)
  return,0L
end

;=============================================================================
;  StatusWidget - Return the widget for reporting textual status
;                 messages to.
;=============================================================================
function ObjReport::StatusWidget
  catch, err
  if err ne 0 then begin 
     catch,/cancel
     message, "No class tag pointer `wInfo' with tag `Status' found.  " + $
              "Override the ReportWidget Method?"
  endif
  if ptr_valid(self.wInfo) then return,((*self.wInfo).Status)
  return,0L
end

;=============================================================================
;  isBlocking - Is the widget part of a blocking widget, or not?
;=============================================================================
function ObjReport::IsBlocking
  return,widget_info(self.or_widget,/XMANAGER_BLOCK)
end

;=============================================================================
;  isWidget - Do we use a widget for popup messages?
;=============================================================================
function ObjReport::IsWidget
  self.or_widget=self->ReportWidget() ;re-get the widget, it could be changing
  return,widget_info(self.or_widget,/VALID_ID)
end

;=============================================================================
;  ObjReport - Special error and warning message helper class
;=============================================================================
pro ObjReport__define
  class={ObjReport, $
         or_title_base:'',$     ;the root of the title
         or_status:'', $        ;the current status message displayed
         or_widget:0L, $        ;the widget, which, if valid, will cause
                                ;graphicsl messages to appear
         or_quiet:0b}
end
