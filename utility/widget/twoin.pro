;-------------------------------------------------------------
;+
; NAME:
;       twoin
; PURPOSE:
;       Widget based text input.
; CATEGORY:
; CALLING SEQUENCE:
;       err=twoin(inp1,inp2,def1,def2)
; INPUTS:
;	def1,2: Optional defaults to put in the input fields.
;	
; KEYWORD PARAMETERS:
;       Keywords:
;	  PARENT_GROUP=pg The widget id of the group leader for this widget
;         TITLE=tt   Widget title text 
;         TEXT1,2=txt  Text to put above the text entry boxes.
;	  LABEL=l An optional label to put at the top.
;	  TEXT_LAB=t An optional text widget (no events at the top)
;	  
; OUTPUTS:
;       inp1,2 = Returned text strings (null for CANCEL).
;-

pro twoin_event, ev
   widget_control, ev.id, get_uval=name
   widget_control, ev.top, get_uval=store
   
   case name of 
      'OK': $
       begin 
         widget_control, (*store).wid[0], get_val=val1
         widget_control, (*store).wid[1], get_val=val2
         (*store).input=[val1,val2]
         widget_control, ev.top, /dest
      end 
      
      'CANCEL': $
       begin 
         (*store).input=['','']
         widget_control, ev.top, /dest
      end 
      
      'TEXT1': widget_control, (*store).wid[1], /INPUT_FOCUS
      'TEXT2': widget_control, (*store).wid[0], /INPUT_FOCUS
      
   endcase 
end

;=====================================================================
;	twoin = Two entry widget text input.
;=====================================================================

function twoin, inp1, inp2,def1,def2,title=title, TEXT1=t1,TEXT2=t2, $
                PARENT_GROUP=group,LABEL=lab, TEXT_LAB=t,_EXTRA=e
   
   if n_elements(title) eq 0 then title='Enter text'
   if n_elements(t1) eq 0 then t1='Input 1'
   if n_elements(t2) eq 0 then t2='Input 2'
   if n_elements(def1) eq 0 then def1=''
   if n_elements(def2) eq 0 then def2=''
   
   ;;------  Lay out widget  ----------
   top = widget_base(/column,title=title,/MODAL,GROUP=group)
   if size(lab,/TYPE) eq 7 then l=widget_label(top,value=lab)
   if size(t,/TYPE) eq 7 then t=widget_text(top,value=t,_EXTRA=e)

   id = widget_label(top,val=t1,/ALIGN_LEFT)
   idtxt1 = widget_text(top,/edit,val=def1,uval='TEXT1')
   id = widget_label(top,val=t2,/ALIGN_LEFT)
   idtxt2 = widget_text(top,/edit,val=def2,uval='TEXT2')

   ;;------------------------------------------------
   but = widget_base(top, /row)
   bok = widget_button(but, val='   OK   ',uval='OK')
   b = widget_button(but, val='Cancel',uval='CANCEL')
   
   ;;------  Package and store needed info  ------------
   store=ptr_new({input:['',''],wid:[idtxt1,idtxt2]})
   widget_control, top, set_uval=store
   
   ;;------  realize widget  -----------
   widget_control, top, /realize,CANCEL_BUTTON=b
   
   ;;------  Event loop  ---------------
   xmanager, 'twoin', top, modal=keyword_set(wait)
   
   ;;------  Get result  ---------------
   if total((*store).input eq '') eq 2 then begin
      ptr_free,store
      return,0
   endif 
   inp1=(*store).input[0] & inp2=(*store).input[1]
   ptr_free,store
   return,1
end
