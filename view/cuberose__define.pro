;;**************************OverRiding methods********************************
;=============================================================================
;  Message - tvDraw and CubeRec messages
;=============================================================================
pro CubeRose::Message, msg
  self->tvPlug::Message,msg,TYPE=type
  case type of
     'TVDRAW_SNAPSHOT': self->DrawRose
     'CUBEREC_UPDATE': begin 
        if msg.bcd_mode then begin 
           self->Off            ;no longer relevant
           return
        endif
        self.cube=msg.cube
        self.cube->GetProperty,ASTROMETRY=astr
        self.angle=atan(-astr.cd[1,0],-astr.cd[0,0])
        if self->On() then self.oDraw->ReDraw,/SNAPSHOT
     end
  endcase 
end

;=============================================================================
;  On - Display on
;============================================================================
pro CubeRose::On
  self->tvPlug::On
  self.oDraw->MsgSignup,self,/TVDRAW_SNAPSHOT
  if obj_valid(self.cube) then self.oDraw->ReDraw,/SNAPSHOT     
end

;=============================================================================
;  Off - Display off
;============================================================================
pro CubeRose::Off,RESET=reset,NO_REDRAW=nrd
  self.oDraw->MsgSignup,self,/NONE
  if self->On() AND ~keyword_set(nrd) then self.oDraw->ReDraw,/SNAPSHOT,/ERASE
  self->tvPlug::Off
end
;;*************************End OverRiding methods******************************


;=============================================================================
;  DrawRose
;=============================================================================
pro CubeRose::DrawRose
  if ~obj_valid(self.cube) then return
  x=!D.X_SIZE*4./5 & y=!D.Y_SIZE*4./5
  s=sin(self.angle) & c=cos(self.angle)
  len=(!D.X_SIZE<!D.Y_SIZE)/8
  deltaE=[-c,-s]*len
  deltaN=[-s, c]*len
  plots,[x,x+deltaE[0]],[y,y+deltaE[1]],THICK=2,COLOR=self.color,/DEVICE
  plots,[x,x+deltaN[0]],[y,y+deltaN[1]],THICK=2,COLOR=self.color,/DEVICE
  deltaE=[-c,-s]*(len+!D.X_CH_SIZE+4) & deltaN=[-s,c]*(len+4)
  xyouts,x+deltaN[0],y+deltaN[1],/DEVICE,'N', $
         ORIENTATION=self.angle*!radeg,COLOR=self.color,CHARSIZE=1.25
  xyouts,x+deltaE[0],y+deltaE[1],/DEVICE,'E', $
         ORIENTATION=self.angle*!radeg, COLOR=self.color,CHARSIZE=1.25
end

;=============================================================================
;  Init
;=============================================================================
function CubeRose::Init,oDraw,COLOR=color
  if (self->tvPlug::Init(oDraw,_EXTRA=e) ne 1) then return,0 
  if n_elements(color) ne 0 then self.color=color
  return,1
end

pro CubeRose__define
  st={CubeRose, $
      INHERITS tvPlug,$         ;it's a tvDraw plugin
      cube:obj_new(),$          ;the cube we're drawing for
      color:0, $                ;the color to use for ourselves
      angle:0.0}                ;N angle, CCW from +y
end
