;-------------------------------------------------------------------------
;+
; NAME:
;       CW_XF
; PURPOSE:
;       Compound widget file selection tool.
; CATEGORY:
;       Compound widgets. File I/O.
; CALLING SEQUENCE:
;       Widget=CW_XF(Parent)
; INPUTS:
;       PARENT - The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;       Keywords:
;         START=dir  Starting directory (default=current).
;         UVALUE - Supplies the user value for the widget.
;         FILTERLIST - List of filter choices to choose from a popup menu
;          (defaults to *, *.* )
;	  SELECT - Which of the filter list to put up in the filter input field
;		(defaults to the first member of the recent filters list)
;         RECENT - if set, a popup list of recently opened files is available
;               (defaults to 0 -- no recent item)
;         FLIMIT - the maximum number of recent files to be diplayed -- ignored
;               if RECENT is not set (defaults to 5 files)
;         DLIMIT - the maximum number of recent directories to display --
;               ignored if RECENT is not set (defaults to 5 directories).
;         NO_SHOW_ALL - if set, the show all button is initially unselected
;         	(unix only).
;         NO_HELP - if set, no help button is appended.
;         SAVEFILE - if set, the selected file is tested for existence, and
;               if it exists, the user is asked if he would like to overwrite
;               it.  If it doesn't exist the file is returned in the event
;               as ev.file.  Normally, a non-existent file results in an error,
;               and the user is asked to select another.
;               This allows the same widget to function as both an open and
;               save dialog.
;         DIRECTORY - If set, select a directory, not a file.
;	  NO_UPDATE - If RECENT is set, and this keyword is also set, the
;               recent menu will be displayed, but the .cw_xf_*_recent
;              	files will not be automatically updated. To save these files,
;              	a call of the form widget_control, id, set_value=1 must be
;              	made.  This increases the speed of programs in which this
;              	widget remains open through many file selections.
;         ON_CLICK - If set, no ok button is given, and the event is
;         	generated when a file is selected (either out of the list
;         	or out the recent file menu).  Otherwise, the selection just
;         	appears in the Selection box (where it can be modified), until
;         	ok is selected.
;	  NO_BUTTONS - If set, and ON_CLICK is set, no buttons are shown
;	  OK_BUTTON - If set to a named variable, the widget id of the OK
;	  	button, if there is one, is returned in that variable.  If
;	  	there is no 'OK' button, -1 is returned in the variable.
;         FILT_FUNC - A function to apply to the listed elements to
;         	obtain a new list.  it must take one argument -- the string
;         	list of files in a directory.  An example might be a function
;         	that sorts the files in the list by size and returns it.
;         FFNAME - A name that will label a button designed to toggle
;         	the function of the filter_function on and off. For the
;         	given example, this might be 'Sort by Size' etc.  Only
;         	applies if filt_func is set.
;	  KEEP_FOCUS - If set, the focus is not automatically put down to the
;	        selection text box each time a filename is clicked.  This is
;	        useful if you'd like to change the selected file with the
;	        arrow keys, e.g.
;	  MULTIPLE - Allow multiple selected files via the platforms native
;	  	mechanism (usually shift or command click). The select box
;	  	does not exist in this case.  THIS IS ONLY VALID IF SAVEFILE IS
;	  	NOT SET, SINCE YOU CANNOT SAVE TO MULTIPLE FILES AT ONCE.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;      CW_XF generates events with the three normal fields,
;      as well as the data field FILE, which contains a pointer to the
;      file(s) selected in the file list box as a string.  It is
;      generated when a filename is clicked or the user enters a name
;      directly.  No other events are generated.  A file in the user
;      (or IDL for Windows) home directory is created for storage of
;      recent file and directory info, called ~/.cw_xf_recent
;      (IDLHOME\cwxfrec for Windows).
;
; PROCEDURE:
;       WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;               current value of the filename (and directory).  It can also
;               be used to reset the value (as in the case of an error
;               occuring) but setting the value to 0.  In addition, setting it
;               to 1 causes the recent file and directory info to be written
;               out, in the case that /RECENT and /NO_UPDATE are both
;               specified.
;
;       WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain
;               the full filepath of the current selection.
;
;-
;---------------------------------------------------------------------------

;=========================================================================
;      xf_getrecent = Get recent files.
;=========================================================================
pro xf_getrecent ,flim,flist,dlim,dlist,filtlim,filtlist
   common cw_xfblock, sep, recfile
   on_ioerror,bad
   openr, un,/get_lun,recfile, ERROR=err
   if err ne 0 then begin
      flist=[''] & dlist=[''] & filtlist=['']
      filtlim=0 & dlim=0 & flim=0
      return
   endif
   nf=0 & nd=0 & nfilt=0
   readf, un,FORMAT='(I2,5X,I2,5X,I2)',nf,nd,nfilt
   if nf ne 0 then begin
      flist=strarr(nf)
      readf, un,FORMAT='('+strtrim(nf,2)+'(A,:,/))',flist
      if n_elements(flist) gt flim then flist=flist(0:flim-1)
   endif else begin
      flim=0 & flist=['']
   endelse
   if nd ne 0 then begin
      dlist=strarr(nd)
      readf, un,FORMAT='('+strtrim(nd,2)+'(A,:,/))',dlist
      if n_elements(dlist) gt dlim then dlist=dlist(0:dlim-1)
   endif else begin
      dlim=0 & dlist=['']
   endelse
   if nfilt ne 0 then begin
      filtlist=strarr(nfilt)
      readf, un,FORMAT='('+strtrim(nfilt,2)+'(A,:,/))',filtlist
      if n_elements(filtlist) gt filtlim then filtlist=filtlist(0:filtlim-1)
   endif else begin
      filtlim=0 & filtlist=['']
   endelse
   free_lun,un
   return
   bad:
   flist=[''] & dlist=[''] & filtlist=['']
   free_lun,un
end

;=========================================================================
;      xf_addrecent = Add a recent file(s)/direc entry (save only if
;      no_update not set). Add also the filter save.  More than one file
;      can be specified, and all will be added (except duplicates).
;=========================================================================
pro xf_addrecent, state,addfile,adddir
  adddir=strtrim(adddir[0],2)
  ;;--- get the list out of the widget
  addfile=strtrim(addfile,2)
  
  if addfile[0] ne '' then begin
     widget_control, state.recfiles_id, get_value=flist
     ;;--- ensure no multiple copies of the same item
     wh=where(flist ne '',cnt)
     if cnt ne 0 then flist=flist[wh]
     for i=0,n_elements(addfile)-1 do begin 
        wh=where(flist ne addfile[i],cnt)
         if cnt gt 0 then $
          flist=[addfile[i],flist[wh]] else flist=[addfile[i]]
      endfor
      widget_control,state.recfiles_id, set_value=flist
   endif 
   
   if strlen(adddir) ne 0 then begin
      widget_control, state.recdirs_id, get_value=dlist
      wh=where(dlist ne adddir and dlist ne '',cnt)
      if cnt gt 0 then $
       dlist=[adddir,dlist(wh)] else dlist=[adddir]
      widget_control, state.recdirs_id,set_value=dlist
   endif

   widget_control, state.filtmenu_id, get_uvalue=filtlist
   if (state.butval and 1B) eq 0 then begin ;we're using a filter
      widget_control, state.filt_id,get_value=filtcurr
      wh=where(filtlist ne filtcurr(0) and filtlist ne '',cnt)
      if cnt gt 0 then $
       filtlist=[filtcurr,filtlist(wh)] else filtlist=[filtcurr]
      widget_control, state.filtmenu_id, set_uvalue=filtlist
   endif
   ;;--- remove and replace recent file if state.nu is non-zero
   if state.nu eq 0 then xf_recent_update,flist,dlist,filtlist
end

;=========================================================================
;      xf_recent_update = Update the recent files and/or dirs.
;=========================================================================
pro xf_recent_update,flist,dlist, filtlist
   common cw_xfblock, sep, recfile
   widget_control,/HOURGLASS
   openw, un,/get_lun,recfile
   if filtlist(0) eq '' then nfilt=0 else nfilt=n_elements(filtlist)
   nf=n_elements(flist) & nd=n_elements(dlist)
   printf,un,FORMAT='(I2,5X,I2,5X,I2)',nf, nd, nfilt
   printf,un,FORMAT='(A)',flist
   printf,un,FORMAT='(A)',dlist
   if nfilt ne 0 then printf,un,FORMAT='(A)',filtlist
   free_lun,un
   return
end

;=========================================================================
;      xf_strip = strips off last filename or directory in file or
;      directory path
;=========================================================================
function  xf_strip, file_path
   common cw_xfblock, sep ,recfile

   len=strlen(file_path)
   ;; Lop off final '/' (or '\')i.e. for directories
   if (len gt 1) and strmid(file_path,len-1,1) eq sep then $
    file_path=strmid(file_path,0,len-1)
   i=0
   while (i ne -1) do begin
      store=i
      i=strpos(file_path,sep,i)
      if (i ne -1) then i=i+1
   endwhile

   return, strmid(file_path,0,store)
end

;=====================================================================
;	xf_getdir = Get directory from supplied filename
;	 (use current directory if file_path has no directory part)
;=====================================================================
function xf_getdir,file_path,FULL=full
   common cw_xfblock, sep, recfile
   ;; Parse out the directory from the supplied value
   dir=xf_strip(file_path)
   if dir eq '' then begin      ;value entered is only filename
      cd, curr=dir
      if dir ne sep then dir=dir+sep
      full=filepath(ROOT=dir,file_path)
   endif else full=file_path
   return, dir
end

;=========================================================================
;      cw_xf_event= Event handler function for the cw_xf compound widget
;=========================================================================
function cw_xf_event, ev
   common cw_xfblock, sep, recfile
                                ;--- get stashed state info
   Stash=widget_info(ev.handler,/CHILD)
   widget_control, stash, get_uvalue=state, /NO_COPY
   catch, err
   if err ne 0 then begin 
      message,!ERR_STRING,/CONTINUE
      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY ;reset state
      return,0
   endif    
   CASE ev.ID OF

      state.path_id: $          ;***path typed in
       begin
         widget_control,ev.id,get_value=dir
         dir=dir(0)
         if strmid(dir,0,1) eq '~' then dir=getenv('HOME')+strmid(dir,1)
         len=strlen(dir)
         if strmid(dir,len-1,1) ne sep then dir=dir+sep
         widget_control,ev.id,set_val=dir
         ;;Catch errors
         catch, error_stat
         if error_stat ne 0 then begin
            ;;reset to old path
            ret=widget_message(DIALOG=ev.top,[dir+' is not a valid path,',$
                    'or is forbidden. Please reenter'],Title='File Error',/ERR)

            dir=state.savepath
            widget_control, state.path_id, set_value=dir
         endif
         xf_list, state    ;Update all
         state.savepath=dir     ; No errors occured in xf_list's
                                ; and we can safely save this path
      end

      state.filt_id: $         ;***filter typed in
         xf_list, state,/FILE   ; Update files using new filter

      state.filtmenu_id: $      ;***filter selected from menu
       begin
         if (state.butval and 1B) ne 0 then begin ;if all-files is on
            state.butval=state.butval and 2B ;set 1st bit to zero
            widget_control, state.filt_id,/SENSITIVE ;sensitise filters
            widget_control, state.allfilesbut_id, SET_BUTTON=0
         endif
         widget_control,state.filt_id,set_value=ev.value
         xf_list, state,/FILE   ; Update files
      end

      state.recfiles_id: $
       begin
         ;;--- get the path
         dir=xf_getdir(ev.value)
         
         if state.sd then begin 
            if file_test(ev.value,/DIRECTORY) then selection=ev.value $
            else selection=dir
         endif else selection=ev.value
         ;;--- put menu selection into selection field
         if widget_info(state.selection_id,/VALID) then $
          widget_control, state.selection_id,set_value=selection, $
          /INPUT_FOCUS, SET_TEXT_SELECT=strlen(selection)
         ;;--- update path and files
         ;; get old directory -- change things only if dir is different
         widget_control,state.path_id,get_value=olddir
         if olddir(0) ne dir then begin
            widget_control,state.path_id,set_value=dir
            xf_list, state      ;Update Dirs and files -- errors return  here
         endif
         
         if state.sf then begin 
            ;;we're going to return.. test first if saving
            if file_test(ev.value) then begin ;it is already a file
               ret=widget_message('Overwrite '+ev.value+'?',/question, $
                                  /DEFAULT_NO,TITLE='Overwrite?', $
                                  DIALOG=ev.top)
               if ret eq 'No' then begin
                  ;;--- put state back into stash
                  widget_control, stash, set_uvalue=state, /NO_COPY
                  return,0      ;swallow event
               endif
            endif
         endif 
         ;;--- add file and dir to tops of recent menus
         xf_addrecent,state,ev.value,dir
         ;;--- put state back into stash
         widget_control, stash, set_uvalue=state, /NO_COPY
         ;;--- construct return event and return
         retevent={CW_XF_EVENT, ID:ev.handler, TOP:ev.top,HANDLER:0L,$
                   FILE:ptr_new(selection)}
         return,retevent
      end

      state.recdirs_id: $
       begin
         dir=ev.value[0]
         widget_control, state.path_id,set_value=dir
         ;;---update paths and files
         catch, error_stat
         if error_stat ne 0 then begin
            catch,/cancel
            ;;reset to old path
            ret=widget_message(DIALOG=ev.top,[dir+' is not a valid path,',$
                              'or is forbidden. Please select another.'], $
                               Title='File Error',/ERR)
            dir=state.savepath
            widget_control, state.path_id, set_value=dir
         endif
         xf_list,state
         state.savepath=dir     ; No errors occured in xf_list's
                                ; and we can safely save this path
      end

      state.allfilesbut_id: $   ;***all files button toggled
       begin
         if ev.select eq 1 then $ ;all files button turned on -- dim filter
          sensitive=0 else sensitive=1
         widget_control, state.filt_id, SENSITIVE=sensitive
         state.butval=(state.butval and 2B)+byte(ev.select)
         xf_list, state, /FILES  ;Update files
      end

      state.filtfuncbut_id: $   ;***filter function button toggled
       begin
         state.butval=(state.butval and 1B)+2B*byte(ev.select)
         xf_list, state, /FILES  ;Update files
      end

      state.dirs_id: $          ;***subdirectory was clicked
       begin
         widget_control, state.dirlist, get_uvalue=dirlist
         widget_control, state.path_id, get_value=path ;get path
         path=path[0]
         dir=dirlist[ev.index]
         if strmid(dir,0,2) eq '..' then begin
            dir=xf_getdir(path) ; lop off last dir in path
         endif else dir=filepath(ROOT_DIR=path,dir)
         if ev.clicks eq 2 then begin ;; cd to it
            widget_control, state.path_id,set_value=dir
            ;;Catch errors -- e.g. no cd permission
            catch, error_stat
            if error_stat ne 0 then begin
               ;;reset to old path
               ret=widget_message(dir+" is forbidden or cannot be accessed.", $
                                  TITLE='Directory Error',/ERROR,DIALOG=ev.top)
               dir=state.savepath
               widget_control, state.path_id, set_value=dir
            endif
            xf_list, state      ;Update All
            state.savepath=dir  ; No errors occured in xf_list's
                                ; and we can safely save this path
         endif 
         
         if state.sd then begin ;selecting directory, update selection
            if widget_info(state.selection_id,/VALID) then $
               widget_control, state.selection_id,SET_VALUE=dir[0],$
                               INPUT_FOCUS=1-state.keep_focus, $
                               SET_TEXT_SELECT=strlen(dir[0])
         endif 
      end

      state.files_id: $         ;***filename was clicked
       begin
         ;;--- get selection
         widget_control, state.path_id, get_value=path
         widget_control, state.filelist, get_uvalue=filelist
         
         if state.sd then begin 
            selection=path[0] 
         endif else begin 
            list=widget_info(state.files_id,/LIST_SELECT) 
            nli=n_elements(list) 
            if nli eq 1 then begin 
               selection=filepath(ROOT_DIR=path[0],filelist[list[0]])
            endif else begin 
               selection=filepath(ROOT_DIR=path[0],filelist[list])
            endelse 
         endelse 
         
         ;;--- put (first) selection into selection field
         if widget_info(state.selection_id,/VALID) then $
          widget_control, state.selection_id,set_value=selection[0], $
          INPUT_FOCUS=1-state.keep_focus,SET_TEXT_SELECT=strlen(selection[0])
         if state.oc ne 0 or ev.clicks eq 2 then begin ;if oc or double clicked
            if state.sf ne 0 then begin ;ask user if he wants to overwrite
               ret=widget_message('Overwrite '+selection+'?',/question, $
                                  /DEFAULT_NO,TITLE='Overwrite?',DIALOG=ev.top)
               if ret eq 'No' then begin ;no overwrite
                  ;;--- put state back into stash
                  widget_control, stash, set_uvalue=state, /NO_COPY
                  return,0      ;swallow event
               endif
            endif
            ;;--- construct return event and return
            retevent={CW_XF_EVENT, ID:ev.handler, TOP:ev.top,HANDLER:0L,$
                      FILE:ptr_new(selection)}
            ;;--- if recentmenu was made then add items to list
            if state.recfiles_id ne 0 then begin
               ;;--- add file and path to recent file list and recent dir list.
               xf_addrecent,state,selection,path
            endif
            ;;--- put state back into stash
            widget_control, stash, set_uvalue=state, /NO_COPY
            return,retevent
         endif
      end

      state.selection_id: $     ;***selection was typed in directly (or return
       begin                    ;hit after a click with on_click off)
         widget_control,ev.id,get_value=selection
         selection=strtrim(selection[0],2)
         if selection eq '' then begin
            ;;--- put state back into stash before swallowing event
            widget_control, stash, set_uvalue=state, /NO_COPY
            ;; swallow event, forcing user to choose another file
            return,0
         endif
         if strmid(selection,0,1) eq '~' then selection=getenv('HOME')+ $
          strmid(selection,1,199)
         dir=xf_getdir(selection,FULL=full)
         ;;--- if full different from selection, put in selection field
         if selection ne full then begin
            widget_control, ev.id, set_value=full
            selection=full
         endif
         ;;Catch errors from xf_list -- i.e. no cd permission
         catch, error_stat
         if error_stat ne 0 then begin
            ret=widget_message(DIALOG=ev.top, $
                               [dir+' is not a valid directory,',$
                                'or is forbidden. Please choose another.'], $
                               TITLE='Directory Error',/ERROR)
            dir=state.savepath
            ;;reset to old path
            widget_control, state.path_id, set_value=dir
            ;;--- put state back into stash before swallowing event
            widget_control, stash, set_uvalue=state, /NO_COPY
            ;; swallow event, forcing user to choose another directory
            return,0
         endif

         ;; get old directory -- change things only if dir is different
         widget_control,state.path_id,get_value=olddir
         if olddir(0) ne dir then begin
            widget_control,state.path_id,set_value=dir
            xf_list, state ;Update Dirs,files -- errors will return from here
         endif

         ;; we've made it past the directory listing so directory is valid
         ;; ... now check if file exists...
         if state.sf eq 0 then begin
               if ~file_test(selection[0],DIRECTORY=state.sd) then begin 
                  type=state.sd?'directory':'file'
                  ret=widget_message([selection[0]+ $
                                      ' is not an existing '+type+'.',  $
                                      ' Please select another.'],$
                                     Title='File Error',/ERR)
               widget_control, state.selection_id,set_value=dir
               ;;--- put state back into stash before swallowing event
               widget_control, stash, set_uvalue=state, /NO_COPY
               ;; swallow event, forcing user to choose another file
               return,0
            endif
         endif else begin ; for saving, a nonexistant file is NOT and error.
            if file_test(selection) then begin ; file is valid
               ret=widget_message('Overwrite '+selection+'?',/question, $
                                  /DEFAULT_NO,TITLE='Overwrite?',DIALOG=ev.top)
               if ret eq 'No' then begin ;no overwrite
                  widget_control, state.selection_id,set_value=dir
                   ;;--- put state back into stash before swallowing event
                  widget_control, stash, set_uvalue=state, /NO_COPY
                  return,0      ;swallow event
               endif ; if button not cancel, fall through to below
            endif
         endelse
         ;; No errors occured in xf_list's above, so
         ;;   we can safely save the given path
         state.savepath=dir

         ;;construct our return event
         retevent={CW_XF_EVENT, ID:ev.handler, TOP:ev.top,HANDLER:0L,$
                   FILE:ptr_new(selection)}
         ;;--- if recentmenu was made then add to recent files list
         if state.recfiles_id ne 0L then begin
            ;;--- add file and path to recent file list and recent dir list.
            xf_addrecent,state,selection,dir
         endif
         ;;--- put state back into stash and return event
         widget_control, stash, set_uvalue=state, /NO_COPY
         return,retevent
      end

      state.butrow_id: $        ;***button was pushed
       case strmid(strtrim(ev.value,2),0,1) of
         'R': $                   ;Reset pushed
          begin
            widget_control,state.path_id,set_value=state.origpath
            xf_list, state
         end

         'H': $
          begin                 ;Help Pushed
            ht=['File selection:',' ',$
                'A directory is displayed with its',$
                'subdirectories in the lower left window.',$
                'Files in that directory are listed on the', $
                'right hand panel, and match the filter in', $
                'the "Filter" box.  If "List all files" is', $
                'selected, no filter is used, and all files',$
                'are listed. Click this button to enable and',$
                'disable the filter.', $
                'Use the mouse to double click on a subdirectory.',$
                'It becomes the new directory and has its',$
                'subdirectories and files listed.  Double click on .. to' , $
                'move up a level. A new directory may be entered,',$
                'press RETURN to update the subdirectories.',$
                'Files and filters may also be entered directly.',$
                'or clicked on for update.  Also, filters',$
                'may be chosen from the pop-up "Filters" menu.',$
                'The Reset Original Directory button resets to' ,$
                'the directory displayed at startup.',$
                'Double-Clicking files returns immediately.']
            a=widget_message(ht,/INFO,TITLE='File Selection Help', $
                             DIALOG=ev.top)
         end

         'O': $                   ;OK pushed
          begin
            if widget_info(state.selection_id,/VALID) then begin 
               widget_control, state.selection_id, get_value=selection 
               selection=strtrim(selection[0],2)
            endif else begin
               widget_control,state.path_id,get_value=path
               widget_control, state.filelist, get_uvalue=filelist
               list=widget_info(state.files_id,/LIST_SELECT)
               if list[0] ne -1 then begin 
                  nli=n_elements(list) 
                  if nli eq 1 then begin 
                     selection=filepath(ROOT=path[0],filelist[list[0]])
                  endif else begin 
                     selection=filepath(ROOT=path[0],filelist[list])
                  endelse 
               endif else selection=''
            endelse 
             
            
            ;; test final file for validity
            selection=strtrim(selection,2)
            if selection[0] eq '' then begin
               if state.sd then begin 
                  widget_control, state.path_id,GET_VALUE=selection 
               endif else begin
                  ;;--- put state back into stash before swallowing event
                  widget_control, stash, set_uvalue=state, /NO_COPY
                  ;; swallow event, forcing user to choose another file
                  return,0
               endelse 
            endif
            if state.sf eq 0 then begin
               if ~file_test(selection[0],DIRECTORY=state.sd) then begin 
                  type=state.sd?'directory':'file'
                  ret=widget_message([selection[0]+ $
                                      ' is not an existing '+type+'.',  $
                                      ' Please select another.'], $
                                     TITLE='File Error',/ERROR,DIALOG=ev.top)
                  ;;--- put state back into stash before swallowing event
                  widget_control, stash, set_uvalue=state, /NO_COPY
                  ;; swallow event, forcing user to choose another file
                  return,0
               endif
            endif else begin    ; for saving, a nonexistant file is NOT
                                ; an error.
               if file_test(selection[0]) then begin ; file *is* valid
                  ret=widget_message('Overwrite '+selection[0]+'?',/question, $
                                     /DEFAULT_NO,TITLE='Overwrite?', $
                                     DIALOG=ev.top)
                  if ret eq 'No' then begin ;no overwrite
                     ;;--- put state back into stash before swallowing event
                     widget_control, stash, set_uvalue=state, /NO_COPY
                     return,0   ;swallow event
                  endif         ; if button not cancel, fall through to below
               endif
            endelse
            ;;--- if recentmenu was made then add items to list
            if state.recfiles_id ne 0 then begin
               widget_control, state.path_id, get_value=path
               ;;--- add file(s) and path to recent file list and recent dir
               ;;    list.
               xf_addrecent,state,selection,path
            endif
            ;;--- put state back into stash
            retevent={CW_XF_EVENT, ID:ev.handler, TOP:ev.top,HANDLER:0L,$
                      FILE:ptr_new(selection)}
            widget_control, stash, set_uvalue=state, /NO_COPY
            return,retevent
         end
      endcase
   ENDCASE
                                ; Swallow all events except the files
                                ; and the selection event (or Ok event)
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY ;reset state
   return,0
end


;=====================================================================
;	xf_list = Find and display file and directory lists
;	Use FILEONLY to update only file list (if directory didn't
;	change ... e.g All files button pressed, or filter changed.
;=====================================================================
pro xf_list, state,FILESONLY=fo
   common cw_xfblock, sep, recfile

   nfoQ=n_elements(fo) eq 0 ;not just files only -- dirs too
   widget_control,/HOURGLASS
   if (state.butval and 1B) ne 0 then filt='*' else $ ;all files pushed
    widget_control, state.filt_id,get_val=filt ; Get filter
   if nfoQ then begin
      widget_control, state.path_id,get_val=dir ; Get directory.
      dir=dir[0]
      cd, dir                   ; Change to directory
   endif
   filt=strtrim(filt[0],2)      ; remove unused blanks in filter
   if filt eq '' then filt='*'  ; defaults to all
   if (state.butval and 1B) ne 0 then $
      filetxt=file_search(/TEST_REGULAR) else $
         filetxt=file_search(filt,/TEST_REGULAR)
   if nfoQ then $         ;get directories ... new list possibly needed.
      subdirtxt=['..',file_search(/TEST_DIRECTORY)]

   if state.filtfunc ne '-1' then begin ;;apply filt_function to list
      if (state.butval and 2B) ne 0 then begin  ;;custom filt_func button on
         ;; desensitise filt menu to ensure not input to screw it up
         widget_control,state.filtmenu_id,SENSITIVE=0
         filetxt=call_function(state.filtfunc,filetxt) ;pass through function
         widget_control, state.filtmenu_id,/SENSITIVE
      endif
   endif
   widget_control, state.files_id, SET_VALUE=filetxt ; display list
   widget_control, state.filelist, SET_UVALUE=filetxt ; record list
   if nfoQ then begin           ; directories were updated...
      ;; Display list.
      widget_control, state.dirs_id, set_val=subdirtxt,SET_LIST_SELECT=0
      widget_control,state.dirlist, set_uval=subdirtxt ; Record list.
   endif
end

;=====================================================================
;	cw_xf_get_value = Get value
;=====================================================================
FUNCTION cw_xf_get_value, id

   ON_ERROR, 2

   stash = WIDGET_INFO(id, /CHILD) ; Retrieve the state
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   widget_control, state.filt_id, get_value=ret ; Get the filter value
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
   RETURN, ret
END

;=====================================================================
;	cw_xf_set_value = Set value
;=====================================================================
pro cw_xf_set_value, id, value

   ON_ERROR, 2
   ;; Retrieve the state.
   stash=widget_info(id,/CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

   s=size(value)
   case s(1) of
      7: $                      ;It's a string
      begin
         ;; Parse out the directory from the supplied value
         dir=xf_getdir(value)
         ;; Set the selection and directory value
         widget_control, state.selection_id, set_value=value
         widget_control, state.path_id, set_value=dir
      end

      2: $                      ;It's an integer, either clear the selection
       begin                    ;(0) or write out the "recent" files (nonzero)
         if value eq 0 then widget_control, state.selection_id, set_value=''$
         else begin
            if state.recfiles_id gt 0 then begin ; recent menu formed...
               widget_control, state.recfiles_id, get_value=flist
               widget_control, state.recdirs_id, get_value=dlist
               widget_control, state.filtmenu_id,get_uvalue=filtlist
               xf_recent_update,flist,dlist,filtlist
            endif
         endelse
      end
   endcase
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
end

;=====================================================================
;	cw_xf_start.pro = Initialization routine on realization
;=====================================================================
pro cw_xf_start,id
   stash = WIDGET_INFO(id, /CHILD) ; Retrieve the state
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
   ;------  Find and display subdirectory and file lists  ------
   xf_list, state
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
   return
end

;=====================================================================
;	cw_xf.pro = Compound Widget file selection tool
;=====================================================================
FUNCTION cw_xf,parent,UVALUE=uval,FILTERLIST=fl,start=start, $
               RECENT=recent,FLIMIT=flim,DLIMIT=dlim,FLTLIMIT=fltlim, $
               NO_SHOW_ALL=nsha, NO_HELP=nh, NO_BUTTONS=nb, SELECT=sel,  $
               SAVEFILE=sf,NO_UPDATE=nu, ON_CLICK=oc,OK_BUTTON=okbut, $
               FILT_FUNC=ff, FFNAME=ffname, KEEP_FOCUS=kf, MULTIPLE=mlt, $
               DIRECTORY=sd

   common cw_xfblock, sep, recfile

;------ Keyword Defaults and actions ---------
   IF (N_ELEMENTS(uval) EQ 0)  THEN uval = 0L
   IF (N_ELEMENTS(fl) EQ 0)  THEN fl = ['*','*.*','.*']
   if n_elements(flim) eq 0 then flim=20 ;set default number of files limit
   if n_elements(dlim) eq 0 then dlim=7 ; and directories limit
   if n_elements(fltlim) eq 0 then fltlim=4 ;recent filters
   sd=keyword_set(sd) 
   sf=keyword_set(sf) AND NOT sd ;set up for testing the file for save
   
   mlt=keyword_set(mlt) AND (sf eq 0)
   if n_elements(nu) eq 0 then nu=0 ;set up for updating "recent" files
   if n_elements(oc) eq 0 then oc=0 ;set up for events "on-click"
   if n_elements(ff) eq 0 then ff='-1' else $
   if n_elements(ffname) eq 0 then ffname=ff
   if n_elements(kf) eq 0 then kf=0
   nb=keyword_set(nb) 

   ;;--- Set up OS dependent stuff -- put in common block
   case !VERSION.OS_FAMILY of
      'Windows': $
         begin
         rootlim=3
         recfile=!DIR+'\cwxfrec'
         device,get_current_font=cfont
         dispfont=cfont+'*14'
      end

      else: $
         begin
         rootlim=1
         recfile=getenv('HOME')+'/.cw_xf_recent'
         window,/PIXMAP,/FREE,xsize=1,ysize=1
         device,get_fontnames=df,font='*cour*bold-r-normal*--14*'
         wdelete
         if n_elements(df) ne 0 then dispfont=df[0] else $ 
            dispfont= $
               '-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1'
      end
   endcase
   sep=path_sep()

   ;;--- if RECENT was given, set up recent file menu
   nrec=n_elements(recent)
   nfl=n_elements(fl)
   filtlist=0                   ;recent filter list
   showlist=0                   ;list of showing recent filters
   fuval=fl
   if nrec ne 0 then begin
      ;; get the recent files and directories on output the limits are
      ;; set to how many there actually are
      xf_getrecent,flim,flist,dlim,dlist,fltlim,filtlist
      match, fuval, filtlist, sub1,sub2,count=cnt ;test for duplicates
      if cnt ne 0 then begin ; remove replicated ones
         showlist=filtlist
         showlist(sub2)=''
      endif else showlist=filtlist
      wh2=where(showlist ne '',cnt)
      if cnt ne 0 then begin
         showlist=showlist(wh2)
         fltlim=n_elements(showlist)
      endif else fltlim=0
      fuval=[fuval,filtlist]
   endif else fltlim=0


   ;;--- Create struct for pulldown filter menu (cw_pdsep requires)
   numfilts=nfl+fltlim          ;passed and recent filters
   filtstruct={flags:0,name:''}
   filtstruct=replicate(filtstruct,numfilts+1)
   filtstruct(1:nfl).name=fl    ;put in preset filters
   filtstruct(0).name="Filters"
   filtstruct(0).flags=1 & filtstruct(numfilts).flags=2
   if nrec ne 0 and fltlim ne 0 then begin
      ;;set initial filter list member to show
      if n_elements(sel) eq 0 then sel=nfl
      if numfilts-1-nfl eq 0 then filtstruct(nfl+1:numfilts).name=showlist(0) $
      else $
       filtstruct(nfl+1:numfilts).name=showlist
      filtstruct(nfl+1).flags=4 ;separator line between set and recent filters
   endif
   if n_elements(sel) eq 0 then sel=0

   ;; --- get starting pathname and/or filename
   start_file=''
   
   cd, curr=curr               ;get current directory
   if size(start,/TYPE) eq 7 then begin
      if strmid(start,strlen(start)-1,1) ne sep then begin ;it's a file
         start_file=filestrip(start)
         start=strtrim(xf_strip(start),2) ;strip off file
         if strlen(start) eq 0 then start=curr ;ensure a directory
      endif 
   endif else begin 
      start=curr
      ;avoid adding separator to root...Windows gives hard drive name also
   endelse 
   if strlen(start) gt rootlim then start=start+sep
   
;------ Set up State to contain id's of expected events  ---------;
   state = {path_id:0L, $       ;id of path text box
            filt_id:0L, $       ;id of filter text box
            filtmenu_id:0L,$    ;id of filter menu
            recentmenu_id:0L,$  ;id of recent files menu
            allfilesbut_id:0L,$  ;id of the Show All Files button
            filtfuncbut_id:0L, $ ;if of button
            filtfunc:ff, $      ;function to apply to file list
            butval:0B, $        ;value of the above buttons
                                ;1st bit allfiles, 2nd bit filter button
            dirs_id:0L,$        ;id of the directory list
            files_id:0L,$       ;id of the file list
            recfiles_id:0L,$    ;id of button for recent files
            sf:sf,$             ;whether we are saving or opening
            nu:nu,$             ;whether to update the recent lists for every
            $                   ;  file selected.
            oc:oc, $            ;whether to return on click or on Ok
            sd:sd, $            ;whether we're selecting a directory
            keep_focus:kf, $    ;whether to keep the focus in the list
            recdirs_id:0L,$     ;id of button for recent directories
            selection_id:0L,$   ;id of the selection text box
            butrow_id:0L,$      ;id of the Reset and Help button row
            origpath:start,$    ;value of the original directory
            filelist:0L,$       ;id of the file list base,
            $                   ;  whose uvalue is the file list (accomodates
            $                   ;  changing list size)
            dirlist:0L,$        ;id of the directory list base, whose
            $                   ;  uvalue is the directory list
            savepath:start}     ;the last path entered...to revert to in case
                                ;  of error

;------  Lay out widget, plugging relevant id's into state --------;
   base = widget_base(parent, $
                      /COLUMN,$
                      /FRAME, $
                      EVENT_FUNC='CW_XF_EVENT', $
                      FUNC_GET_VALUE='CW_XF_GET_VALUE', $
                      PRO_SET_VALUE='CW_XF_SET_VALUE', $
                      UVALUE=uval,$
                      space=1, $
                      NOTIFY_REALIZE='CW_XF_START', $
                      /BASE_ALIGN_CENTER)

   ;;--- PATHNAME
   pathbase = widget_base(base,/ROW)
   nullid = widget_label(pathbase,VALUE='Directory:')
   state.path_id=widget_text(pathbase,VALUE=start,/EDIT,XSIZE=35)

   ;;--- FILTER SELECTION
   filtbase = widget_base(base,/ROW,space=1,/BASE_ALIGN_CENTER)
   nullid = widget_label(filtbase,VALUE='Filter:')
   state.filt_id=widget_text(filtbase,VALUE=fuval(sel),XS=10,ysize=1,/EDIT)
   
   state.filtmenu_id=cw_pdsep(filtbase,filtstruct,/RETURN_NAME, $
                               uvalue=filtlist)
   filtbutbase = widget_base(filtbase,/NONEXCLUSIVE,/COLUMN,SPACE=0)
   state.allfilesbut_id=widget_button(filtbutbase,value='Show All')
   if ff ne '-1' then begin ;a filter function was specified
      state.filtfuncbut_id=widget_button(filtbutbase,value=ffname)
   endif

   ;;--- DIRECTORIES AND FILES
   choose=widget_base(base,/ROW,space=0)
   ;;--- dirlist - we later put the directory list in the left base uvalue
   state.dirlist=widget_base(choose,/COLUMN,/FRAME,space=1)

   if nrec ne 0 then begin
      if dlim eq 0 then begin
         dlist=[''] & dlim=1    ;insert a dummy
      endif
      topline=widget_base(state.dirlist,/ROW,/ALIGN_LEFT, $
                          /BASE_ALIGN_TOP)
      if !VERSION.OS_FAMILY eq 'Windows' then bitmap='D' else $
       bitmap=[[102B],[255B],[219B],[102B],[102B],[219B],[255B],[102B]]
      state.recdirs_id=cw_bitmen(topline,bitmap,dlist,dlim,xs=16,ys=19)
      nullid=widget_label(topline,VALUE='Directories ', FONT=dispfont)
   endif else begin
      nullid=widget_label(state.dirlist,VALUE=' Directories         ')
   endelse
   state.dirs_id=widget_list(state.dirlist,YSIZE=12,XSIZE=16)
   ;;--- filelist - we later put the file list in the right base uvalue
   state.filelist=widget_base(choose,/COLUMN,/FRAME,space=1)
   if nrec ne 0 then begin
      if flim eq 0 then begin
         flist=[''] & flim=1    ;a dummy list
      endif
      topline=widget_base(state.filelist,/ROW,/ALIGN_LEFT, $
                          /BASE_ALIGN_TOP)
      if !VERSION.OS_FAMILY eq 'Windows' then bitmap='F' else $
       bitmap=[[219B],[189B],[102B],[219B],[219B],[102B],[189B],[219B]]
      state.recfiles_id=cw_bitmen(topline,bitmap,flist,flim,xs=16,ys=19)
      nullid=widget_label(topline,VALUE='Files        ',FONT=dispfont)
   endif else begin
      nullid=widget_label(state.filelist,VALUE=' Files                  ')
   endelse
   state.files_id=widget_list(state.filelist,YSIZE=12,XSIZE=22,MULTIPLE=mlt)

   ;;--- Selection
   if NOT keyword_set(mlt) then begin 
      selection=widget_base(base,/ROW)
      nullid=widget_label(selection,value='Selection:')
      state.selection_id=widget_text(selection,/EDIT,value=start_file,XSIZE=35)
   endif 
   
   ;;--- ACTION BUTTONS
   if NOT nb then begin 
      if n_elements(nh) eq 0 then begin
         btext=['Reset Original Directory','  Help  ']
      endif else btext=['Reset Original Directory']
      
      if oc eq 0 then btext=[btext,'   OK    ']
      state.butrow_id = cw_bgroup(base,  btext, /ROW, /NO_RELEASE, $
                                  /RETURN_NAME, IDS=ids)
      if arg_present(okbut) then begin 
         if oc eq 0 then okbut=ids[n_elements(ids)-1] else okbut=-1
      endif 
   endif 
   
;------- Set up initial status of buttons ------
   ;;--- Set "Show All Files" button to default
   if n_elements(nsha) eq 0 then sb=1B else sb=0B
   widget_control, state.allfilesbut_id,SET_BUTTON=sb
   ;;--- Set the state variable which indicates status of this button
   state.butval=sb
   ;;--- Affect filter button sensitivity
   widget_control, state.filt_id, SENSITIVE=(sb xor 1)

;----- Stash the state in the first child -----
   widget_control,widget_info(base,/CHILD),SET_UVALUE=state,/NO_COPY

   return,base
end
