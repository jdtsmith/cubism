;+
; NAME: MULTFLS
;
; PURPOSE: Selecting mulitple files
;
;
; CATEGORY: FILE SELECTION
;
; CALLING SEQUENCE: files=multfls()
;
; INPUTS: none.
;
; KEYWORD PARAMETERS:
;	TITLE: The tile to give the widget.
;	LISTFUNC:  The name of a function to apply to each new filename as it
;		arrives before listing.  It must take a string and return a
;		string.  As an example, the funtion lffitsobj() obtains a
;		keyword parameter from a file, and lists it along with the
;		filename only (path chopped off).
;	TOPLABEL: A string to put above the selected file region.  This is
;		useful to label the pieces of the output resulting from
;		application of the LISTFUNC function.  If not passed, no
;		label is used.  The label should conform to the format of the
;		strings returned by listfunc, to ensure that the data
;		lines up with the top label.
;	_EXTRA: Any keyword available to cw_xf may be passed.
;
; OUTPUTS: files: A string array of fully filenames (paths included).
;
; MODIFICATION HISTORY:
;   6/98  Added Pointer support, and support for multiple seclection.
;  11/97	JDS
;-

pro multfls_event,ev
   stash=widget_info(ev.handler,/CHILD)
   widget_control, stash, get_uvalue=sptr
   
   if ev.id eq (*sptr).plist then return
   
   ;; whether we have list function
   lQ=(*sptr).listfunc ne ''
   if ptr_valid((*sptr).flist) then nl=n_elements(*(*sptr).flist) else nl=0
   
   case ev.id OF
      (*sptr).floader: $          ;file(s) selected
       begin
         for i=0,n_elements(*ev.file)-1 do begin 
            if lQ then begin
               widget_control, /HOURGLASS
               sf=call_function((*sptr).listfunc,(*ev.file)[i])
            endif else sf=(*ev.file)[i]
            if ptr_valid((*sptr).flist) then begin
               wh=where(*(*sptr).flist ne (*ev.file)[i],cnt)
               if cnt gt 0 then begin 
                  *(*sptr).flist=[(*(*sptr).flist)[wh],(*ev.file)[i]] 
                  *(*sptr).slist=[(*(*sptr).slist)[wh],sf]
               endif 
            endif else begin
               (*sptr).flist=ptr_new((*ev.file)[i])
               (*sptr).slist=ptr_new([sf])
            endelse 
         endfor 
         ptr_free,ev.file
         widget_control, (*sptr).plist,set_value=*(*sptr).slist
      end
      
      (*sptr).butrow: $         ;One of the buttons was clicked
       begin
         case ev.value of
            0: $                ;Remove Clicked
             begin
               sel=widget_info((*sptr).plist,/LIST_SELECT) 
               if sel[0] ne -1 and nl gt 0 then begin 
                  i=bytarr(nl)
                  i[sel]=1b
                  wh=where(i ne 1b,cnt)
                  if cnt ne 0 then begin 
                     *(*sptr).flist=(*(*sptr).flist)[wh]
                     *(*sptr).slist=(*(*sptr).slist)[wh]
                     widget_control, (*sptr).plist, set_value=*(*sptr).slist, $
                      SET_LIST_SELECT=0>sel[0]<(cnt-1)
                  endif else begin 
                     ptr_free,(*sptr).flist,(*sptr).slist
                     widget_control,(*sptr).plist,set_value=''
                  endelse 
               endif  
            end

            1: $                ;Remove All Clicked
             begin
               if nl gt 0 then begin 
                  widget_control, (*sptr).plist, set_value=''
                  ptr_free,(*sptr).flist,(*sptr).slist
               endif 
            end 

            2: $                ;Done Clicked
             begin
               widget_control, (*sptr).floader, set_value=1 ;write out recent
               widget_control, ev.top,/DESTROY
               return
            end 
            
            3: $                ;Cancel Clicked
             begin
               ptr_free, (*sptr).flist
               widget_control, ev.top,/DESTROY
               return
            end 
         endcase
      end   
   endcase 
end

function multfls,LISTFUNC=lf,PARENT_GROUP=mfp,_EXTRA=e, TITLE=t,TOPLABEL=tl, $
                 LIST_SIZE=ls
   if n_elements(ls) eq 0 then ls=22
   if n_elements(t) eq 0 then t='Select Files'
   if n_elements(lf) eq 0 then lf=''
   sptr=ptr_new({flist:ptr_new(), $ ;File list
                  slist:ptr_new(), $ ;Show List
                  floader:0L,$  ;ID of file loading widget
                  listfunc:lf,$ ;Function to apply to files to add to list   
                  plist: 0L,$   ;ID of list of picked files
                  butrow: 0L})  ;ID of button row
   
   if keyword_set(mfp) then begin 
      g=widget_info(mfp,/GEOMETRY) 
      xoff=g.xoffset+g.scr_xsize/4
      yoff=g.yoffset+g.scr_ysize/4
      base=widget_base(TITLE=t,/ROW,/MODAL, XOFFSET=xoff,YOFFSET=yoff, $
                       GROUP_LEADER=mfp)
   endif else base=widget_base(TITLE=t,/ROW)
   
   ;;--- get a file loading compound widget
   (*sptr).floader=cw_xf(base,UVALUE='file',/ON_CLICK,/RECENT,/NO_UPDATE, $
                       _EXTRA=e)
   rightbase=widget_base(base,/COLUMN,SPACE=1)
   if n_elements(tl) ne 0 then lbl=widget_label(rightbase,value=tl,/ALIGN_LEFT)
   (*sptr).plist=widget_list(rightbase,ysize=ls,/MULTIPLE)
   (*sptr).butrow=cw_bgroup(rightbase, IDS=ids,$
                          [' Remove ',' Remove All ',' Done ',' Cancel '], $
                          /RETURN_INDEX,/ROW,/NO_RELEASE)
   widget_control, widget_info(base,/CHILD),set_UVALUE=sptr
   if keyword_set(mfp) then begin 
      widget_control, base, /REALIZE,CANCEL_BUTTON=ids[3]
   endif else widget_control, base, /REALIZE
   
   XManager,'multfls', base,/NO_BLOCK
   
   ;; get the value of the filelist, and return it
   if ptr_valid((*sptr).flist) then ret=*(*sptr).flist else ret=-1
   ptr_free,(*sptr).flist,(*sptr).slist,sptr
   return,ret
end




