;;**************************OverRiding methods********************************
;=============================================================================
;  Message - tvDraw and CubeRec messages
;=============================================================================
pro CubeAper::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'DRAW_MOTION': begin 
        if self.working_on eq -1 then return
        ;; Compute the fractional X change from the original handle point
        dist=msg.X-self.save
        ap_ind=n_elements(*self.aps) eq 1?0:self.working_on
        ap=(*self.aps)[ap_ind]
        case self.editing of
           1b: begin            ;top
              delta_ap=ap.high[1]-ap.low[1] 
              x_size=(*self.top_corners)[[0,2],self.working_on]        
              ind=1
           end
           2b: begin            ;bottom
              delta_ap=ap.high[0]-ap.low[0] 
              x_size=(*self.bottom_corners)[[0,2],self.working_on]        
              ind=0
           end
           3b: begin            ;sides
              delta_ap=min(ap.high-ap.low,ind)
              x_size=(*([self.bottom_corners, $
                         self.top_corners])[ind])[[0,2],self.working_on]
              ind=[0,1]
           end
        endcase         
        x_size=x_size[1]-x_size[0]
        off=(float(dist)/x_size)*delta_ap
;        print,'HI-LOW:',ap.high-ap.low,'PS: ',delta_ap/x_size
;        print,'APOFF:',(ap.(self.side)[ind]+off),' OFF: ',off, $
;              ' DIST: ',dist,' X_SIZE: ',x_size,' DELTA_AP: ',delta_ap
;         print,'TOP: ';         print,*self.top_corners
;         print,'BOTTOM: '
;         print,*self.bottom_corners
;         print,'SIDES: '
;         print,*self.sides
        ap.(self.side)[ind]=(ap.(self.side)[ind]+off)>0.<1.
        if NOT array_equal(ap.high[ind]-ap.low[ind] gt .01,1b) then begin
;           print,'bottomed out'
           if self.side eq 0 then ap.low[ind]=ap.high[ind]-.01 else $
              ap.high[ind]=ap.low[ind]+.01
        endif 
           
        if array_equal(ap.(self.side)[ind], $
                       (*self.aps)[ap_ind].(self.side)[ind]) then return
        
        (*self.aps)[ap_ind]=ap
        self->UpdateApDrop
        self.oDraw->Erase,/FULL,/DOUBLE
        self->DrawOneWS,self.working_on,APERTURE=ap
        self.oDraw->DBRefresh,/FULL
        self.save=msg.X
     end
        
     'DRAW_BUTTON': begin 
        case msg.type of 
           0: begin             ;button press
              if msg.press ne 1b then return
              self.oDraw->SetWin
              n=n_elements(*self.top_corners)/4
              p=rebin(float([msg.X,msg.Y]),2,2*n,/SAMPLE)
              vals=[self.top_corners,self.bottom_corners,self.sides]
              
              for val=0,2 do begin 
                 r=reform((*vals[val]),2,2*n)
                 dist=total((p-r)^2,1)
                 wh=where(dist le 25.,cnt)
                 if cnt gt 0 then begin ;hit one!
                    if cnt gt 1 then begin 
                       min=min(dist[wh],ind)
                       wh=wh[ind]
                    endif else wh=wh[0]
                    self.working_on=wh/2
                    self.editing=val+1b ;editing WS: top, bottom, sides
                    self.side=wh mod 2 ;left or right
;                    print,'Near: ',(['top','bottom','side'])[val], $
;                          (['left','right'])[self.side],' Order ', $
;                          self.working_on
                    self.save=msg.X
                    erase
                    self.oDraw->Redraw,/SNAPSHOT
                    self.oDraw->MsgSignup,self,/DRAW_MOTION
                    self->DrawOneWS,self.working_on
                    nap=n_elements(*self.aps)
                    ;; Unlock, ensure an aperture for every order
                    if (self.mode AND 4b) eq 0b then begin 
                       if nap eq 1 then $
                          *self.aps=replicate((*self.aps)[0],self->NOrds())
                    endif
                    self->UpdateApDrop
                    return
                 endif 
              endfor 
           end
           1: begin             ;button release
              if self.working_on eq -1 then return
              nap=n_elements(*self.aps)
              if (self.mode AND 4b) ne 0b then begin ; Lock, set to this one
                 if nap gt 1 then *self.aps=(*self.aps)[self.working_on]
              endif
              self->UpdateApDrop
              self->SetAper
              self.working_on=-1
              self.oDraw->MsgSignup,self,DRAW_MOTION=0 ;turn off motion
              self.editing=0b
              self.oDraw->ReDraw,/SNAPSHOT
              ;; Send update message
           end
        endcase 
        
     end 
     
     'TVDRAW_SNAPSHOT': self->DrawWS,SKIP=self.working_on
     'CUBEREC_UPDATE': begin 
        if msg.bcd_mode eq 0 then begin 
           self->Off            ;no longer relevant
           return
        endif 
        self.cube=msg.cube
        self.cube->GetProperty,APERTURE=aps,CALIB=cal
        if ptr_valid(self.prs) then ptr_free,*self.prs,self.prs,self.orders
        nords_old=self->NOrds()
        self.prs=ptr_new(self.cube->PRs(ORDERS=ords,/FULL))
        nords=n_elements(ords) 
        self.orders=ptr_new(ords,/NO_COPY)
        if nords ne nords_old then begin 
           ptr_free,self.bottom_corners,self.top_corners,self.sides
           self.bottom_corners=ptr_new(fltarr(4,nords,/NOZERO))
           self.top_corners=ptr_new(fltarr(4,nords,/NOZERO))
           self.sides=ptr_new(fltarr(4,nords,/NOZERO))
        endif 
        ptr_free,self.aps
        widget_control, self.wWSBut,GET_VALUE=val
        val[2]=n_elements(aps) eq 1
        self.mode=(self.mode AND 3b) OR ishft(val[2],2)
        widget_control, self.wWSBut,SET_VALUE=val
        self.aps=ptr_new(aps,/NO_COPY)
        self->UpdateApDrop
        self.cal=cal
        if self.mode AND 1b then self.oDraw->ReDraw,/SNAPSHOT
     end
  endcase 
end

;=============================================================================
;  On - Editing on
;============================================================================
pro CubeAper::On
  if (self.mode AND 1b) eq 0b then return ;not display
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/TVDRAW_SNAPSHOT,/DRAW_BUTTON
  self.oDraw->ReDraw,/SNAPSHOT     
end

;=============================================================================
;  Off - Editing off
;============================================================================
pro CubeAper::Off,RESET=reset,NO_REDRAW=nrd
  self.working_on=-1
  if keyword_set(reset) then self.mode=0b else $
     self.mode=self.mode AND NOT 2b ;remove editing bit
  widget_control,self.wWSBut,GET_VALUE=val
  if keyword_set(reset) then val[0]=0
  val[1]=0
  widget_control,self.wWSBut,SET_VALUE=val
  widget_control, self.wWSButs[2],SENSITIVE=0 ;lock non-sensitive
  self.oDraw->MsgSignup,self,DRAW_BUTTON=0,DRAW_MOTION=0, $
     TVDRAW_SNAPSHOT=self.mode AND 1b
  if self->On() AND ~keyword_set(nrd) then self.oDraw->ReDraw,/SNAPSHOT
  self->tvPlug::Off
end
;;*************************End OverRiding methods******************************

;=============================================================================
;  Event Callbacks
;============================================================================
function CubeAper_Event,ev
  widget_control, ev.id,get_uvalue=self
  call_method,'Event',self,ev
  return,1
end
pro CubeAper_DiscardEvent,ev
  return
end
function CubeAper_SetEvent,ev
  widget_control, ev.id,get_uvalue=self
  call_method,ev.value,self
  return,1
end

;=============================================================================
;  Event - Handle events from the buttons
;============================================================================
pro CubeAper::Event,ev
  ;; Only the "Show WS" button there
  widget_control, ev.id,GET_VALUE=val
  if ev.value eq 1 AND val[1] AND NOT val[0] then begin ; Edit hit without show
     val[0]=1b
     widget_control, ev.id,SET_VALUE=val
  endif 
  mode=val[0] OR ishft(val[1],1) OR ishft(val[2],2)
  self.mode=mode  
  if ev.value lt 2 then begin   ;show or edit hit
     widget_control, self.wWSButs[2],SENSITIVE=(mode AND 3b) eq 3b AND $
                     n_elements(*self.prs) gt 1
     case ev.value of
        0: begin                ;show hit
           ;; turn off editing too
           if (self.mode AND 2b) ne 0b then self->Off else begin 
              self.oDraw->MsgSignup,self,TVDRAW_SNAPSHOT=mode AND 1b
              self.oDraw->ReDraw,/SNAPSHOT
           endelse 
        end
        1: $                    ;edit hit
           if (mode AND 3b) eq 3b then self->On else self->Off
     endcase 
  endif
end

;=============================================================================
;  DrawOneWS - Trim and Draw one order's WAVSAMP
;=============================================================================
pro CubeAper::DrawOneWS,which,APERTURE=ap
  if n_elements(ap) eq 0 then $
     ap=n_elements(*self.aps) eq 1?(*self.aps)[0]:(*self.aps)[which]
  prs=*(*self.prs)[which]
  nprs=n_elements(prs) 
  x=prs.x & y=prs.y
  self.cal->Trim,x,y,APERTURE=ap
  ;;mnx=min(prs.X,max=mxx)
  ;;mny=min(prs.Y,max=mxy)
  a=[0,1,2,3,0]
  for i=0,nprs-1 do oplot,x[a,i],y[a,i],COLOR=self.color
  if (self.mode AND 2b) ne 0b then begin ;edit -- draw handles
     usersym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /fill
     ;; bottom corners
     pos1=[total(x[1:2,0])/2.,total(y[1:2,0])/2] ;left
     pos2=[total(x[[0,3],0])/2.,total(y[[0,3],0])/2] ;right
     ind=[[pos1],[pos2]]
     oplot,ind[0,*],ind[1,*],PSYM=8,SYMSIZE=1.4,COLOR=self.color
     pos1=convert_coord(pos1,/DATA,/TO_DEVICE)
     pos2=convert_coord(pos2,/DATA,/TO_DEVICE)
     (*self.bottom_corners)[*,which]=[pos1[0:1],pos2[0:1]]

     ;; top corners
     pos1=[total(x[1:2,nprs-1])/2.,total(y[1:2,nprs-1])/2]
     pos2=[total(x[[0,3],nprs-1])/2.,total(y[[0,3],nprs-1])/2]
     ind=[[ind],[pos1],[pos2]]
     pos1=convert_coord(pos1,/DATA,/TO_DEVICE)
     pos2=convert_coord(pos2,/DATA,/TO_DEVICE)
     (*self.top_corners)[*,which]=[pos1[0:1],pos2[0:1]]
     
     ;; sides
     off=nprs/2+((nprs/10)<5)*([-1,1])[which mod 2]
     pos_sd1=[total(x[1:2,off])/2.,total(y[1:2,off])/2]
     pos_sd2=[total(x[[0,3],off])/2.,total(y[[0,3],off])/2]
     ind=[[ind],[pos_sd1],[pos_sd2]]
     pos_sd1=convert_coord(pos_sd1,/DATA,/TO_DEVICE)
     pos_sd2=convert_coord(pos_sd2,/DATA,/TO_DEVICE)
     (*self.sides)[*,which]=[pos_sd1[0:1],pos_sd2[0:1]]   
     
     oplot,ind[0,*],ind[1,*],PSYM=8,SYMSIZE=1.4,COLOR=self.color
  endif
end

;=============================================================================
;  DrawWS - Trim and show the WAVSAMPs
;=============================================================================
pro CubeAper::DrawWS,SKIP=skip
  if (self.mode AND 1b) eq 0b then return
  if NOT ptr_valid(self.prs) then return

  self.oDraw->GetProperty,OFFSET=off,DISPSIZE=ds,PAN=pan,ZOOM=zm
  plot,[0],[0],XRANGE=[off[0],off[0]+ds[0]],YRANGE=[off[1],off[1]+ds[1]], $
       XSTYLE=5,YSTYLE=5,POSITION=[pan,pan+ds*zm],/DEVICE,/NODATA,/NOERASE
  nords=n_elements(*self.prs)
  nap=n_elements(*self.aps)
  if n_elements(skip) eq 0 then skip=-1 ;nothing to skip
  for i=0,nords-1 do begin
     if skip eq i then continue
     self->DrawOneWS,i
  endfor
end

;=============================================================================
;  NOrds - How many ords showin
;=============================================================================
function CubeAper::NOrds
  if NOT ptr_valid(self.orders) then return,0 else $
     return,n_elements(*self.orders)
end

;=============================================================================
;  UpdateApDrop - Update the droplist showing apertures.
;=============================================================================
pro CubeAper::UpdateApDrop
  if NOT ptr_valid(self.aps) then begin 
     nap=(ind=0)
     aps=[0.0,0.0,0.0,0.0] 
  endif else begin 
     nap=n_elements(*self.aps)
     ind=nap eq 1?0:(self.working_on>0)
     aps=[(*self.aps).low,(*self.aps).high]
  endelse 
  nord=n_elements(*self.orders) 
  prefix=nap gt 0?(nap eq 1 AND nord gt 1?'All': $
                   string(FORMAT='(I2)',*self.orders)):'--'
  prefix=prefix+' : '
  widget_control,self.wapDrop,SET_DROPLIST_SELECT=ind,SET_VALUE= $
                 prefix+string(FORMAT='(%"%4.2f->%4.2f:%4.2f->%4.2f")',aps)
end

;=============================================================================
;  SetAper - Set the Aperture into the cube
;=============================================================================
pro CubeAper::SetAper
  if NOT obj_valid(self.cube) then return
  if NOT ptr_valid(self.aps) then return
  self.cube->SetProperty,APERTURE=*self.aps
end

;=============================================================================
;  SetFull - Set the Aperture to Full
;=============================================================================
pro CubeAper::SetFull
  if NOT ptr_valid(self.aps) then return
  (*self.aps).low =[0.,0.]
  (*self.aps).high=[1.,1.]
  self->UpdateApDrop
  self->SetAper
  if self.mode AND 1b then self.oDraw->ReDraw,/SNAPSHOT
end
;=============================================================================
;  Cleanup
;=============================================================================
pro CubeAper::Cleanup
  ;; The prs themselves 
  if ptr_valid(self.prs) then ptr_free,*self.prs,self.prs
  ptr_free,self.aps,self.top_corners,self.bottom_corners,self.sides
  self->tvPlug::Cleanup
end

;=============================================================================
;  Init
;=============================================================================
function CubeAper::Init,parent,oDraw,COLOR=color
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  self.working_on=-1
  lab=widget_label(parent,value='WAVSAMP:')
  base=widget_base(parent,/COLUMN,/FRAME,SPACE=1,/BASE_ALIGN_CENTER) 
  self.wWSBut=cw_bgroup(base,['Show','Edit','Lock'],/NONEXCLUSIVE, $
                        IDS=ids,EVENT_FUNC='cubeaper_event',/ROW,SPACE=1, $
                        UVALUE=self)
  self.wapDrop=widget_droplist(base,VALUE='--- : ____->____:____->____', $
                               EVENT_PRO='CubeAper_DiscardEvent')
  but=cw_bgroup(parent,['Reset to Full'],UVALUE=self, $
                EVENT_FUNC='CubeAper_SetEvent',/COLUMN, $
                BUTTON_UVALUE=['SetFull'])
  widget_control, ids[2],SENSITIVE=0
  if n_elements(color) ne 0 then self.color=color
  self.wWSButs=ids
  return,1
end

pro CubeAper__define
  st={CubeAper, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      mode:0b, $                ;0b: hide, 1b: show, 2b: edit, 4b: lock
      color:0, $                ;the color to use for ourselves
      cube:obj_new(), $         ;the cube we're dealing with
      cal:obj_new(), $          ;the calibration object
      top_corners:ptr_new(), $  ;device coords of the top corner handles (4xn)
      bottom_corners:ptr_new(), $ ;device coords of the bottom handles (4xn)
      sides:ptr_new(), $        ;the device coords of the side handles (4xn)
      save:0, $                 ;a saved device X coordinate
      wWSBut:0L, $              ;the button group
      wWSButs:lonarr(3), $      ;The WAVSAMP buttons
      wapDrop:0L, $             ;aperture droplist
      aps:ptr_new(), $          ;The aperture(s) (1 or n)
      prs:ptr_new(), $          ;The lists of pseudo-rects, for each order (n)
      orders:ptr_new(), $       ;The orders for each PR set
      working_on:0L, $          ;which PR we're working on (don't snapshot it)
      editing: 0b, $            ;Which are we editing:
                                ;  none (0), top(1), bottom (2), or sides(3)?
      side:0b}                  ;Which side are we editing,
                                ;  left (0) or right (1)?
end
