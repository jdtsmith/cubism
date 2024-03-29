;+
; NAME:  
;
;    CVLine
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Write IRS and Cube-related to a status line widget.
;    
; CATEGORY:
;
;    CUBISM Spectral Reduction, Analysis and Processing.
;    Status Information
;
; METHODS:
;
;    Init:  
;
;       CALLING SEQUENCE:
;
;          obj=obj_new('CVLine',oDraw,parent,[CALIB=,MODULE=])
;
;       INPUT PARAMETERS:
;
;          oDraw: The tvDraw object.
;
;          parent: The widget ID of the parent in which to place the
;             status widget label.
;
;       INPUT KEYWORD PARAMETERS:
;
;          CALIB: The calibration object to use.  Will be queried from
;             the cube we are communicating with by default.
;
;          MODULE: The module(s) to update status for (defaults to all).
;             
; INHERITANCE TREE:
;
;    ObjMsg-->tvPlug_lite-->CubeBackTrack
;
; MODIFICATION HISTORY:
;    
;    2002-12-06 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002-2006 J.D. Smith
;
;  This file is part of CUBISM.
;
;  CUBISM is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2, or (at your option)
;  any later version.
;  
;  CUBISM is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with CUBISM; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

;=============================================================================
;  Message - Display the values.  We have signed up for motion and
;            tracking messages, and will hear from the CubeRec 
;            on changes in mode, etc.
;=============================================================================
pro cvLine::Message,msg
  self->tvPlug_lite::Message,msg,TYPE=type ;pass it up
  case type of 
     'CUBEREC_UPDATE': begin    ;cuberec tells us about new bcd's/cubes
        self.bcd_mode=msg.bcd_mode
        self.module=msg.module
        self.cube=msg.cube
        self.unc=msg.unc
        if self.bcd_mode then begin 
           self.mask=msg.bmask
           self->UpdateWAVSAMP 
        endif else begin 
           if ptr_valid(msg.astrometry) then begin 
              ptr_free,self.astrometry
              self.astrometry=ptr_new(*msg.astrometry)
           endif 
        endelse 
     end
     
     'DRAW_MOTION': begin 
        self.oDraw->GetProperty,IMORIG=imorig
        if NOT ptr_valid(imorig) then return
        pt=self.oDraw->Convert([msg.X,msg.Y],/SHOWING,/FRACTIONAL)
        if pt[0] eq -1 then begin ;not showing
           widget_control, self.wLine,set_value=' '
           self.savpoint=[-1,-1] ;ensure rentry works
           return
        endif
        if self.bcd_mode then begin ;bcd mode
           pr=self->FindPR(pt[0],pt[1],ORDER=order)
           if array_equal(floor(pt),self.savpoint) then begin 
              if size(pr,/TYPE) ne 8 then return
              if pr.lambda eq self.savlambda AND order eq self.savorder then $
                 return         ;nothing to see here, move along
           endif 
           if size(pr,/TYPE) ne 8 then begin 
              lambda=0. & order=0
           endif else lambda=pr.lambda
           widget_control, self.wLine,SET_VALUE= $
                           self->String(imorig,pt,LAMBDA=lambda,ORDER=order)
           self.savlambda=lambda & self.savorder=order 
           self.savpoint=floor(pt)
        endif else begin        ; cube wcs mode
           if ptr_valid(self.astrometry) then begin 
              ;; ra/dec always get updated.
              xy2ad,pt[0]-.5,pt[1]-.5,*self.astrometry,a,d
              widget_control, self.wLine,set_value= $
                              self->String(imorig,pt,RA=a,DEC=d)
           endif else begin 
              widget_control, self.wLine,set_value=self->ValString(imorig,pt)
           endelse 
        endelse 
     end
     
     'WIDGET_TRACKING': begin 
        if msg.enter eq 0 then begin ;just left window -- clear status line
           widget_control,self.wLine,set_value=' '
           self.savpoint=[-1,-1]
           self.savlambda=0.0
           self.savorder=-1
        endif 
     end
     
     'TVDRAW_POSTDRAW': begin
        if self.savpoint[0] eq -1 then return ;not on a point
        self.oDraw->GetProperty,IMORIG=imorig
        if self.bcd_mode then begin 
           widget_control, self.wLine, $
                           set_value=self->String(imorig,self.savpoint, $
                                                  LAMBDA=self.savlambda, $
                                                  ORDER=self.savorder,/FORCE)
        endif else begin 
        endelse 
     end
  endcase 
end 

;=============================================================================
;  FindPR - Find the relevant PR which contains point, if any.
;=============================================================================
function cvLine::FindPR,x,y,ORDER=ord
  ;; Global order min/max cut
  wh=where(x ge (*self.PRs).MIN[0,*] AND $
           y ge (*self.PRs).MIN[1,*] AND $
           x le (*self.PRs).MAX[0,*] AND $
           y le (*self.PRs).MAX[1,*],ocnt)
  if ocnt eq 0 then return,-1
  
  ;; Go through the matching orders and return first PR with the point
  ;; inside it (if any)
  for i=0,ocnt-1 do begin       
     theseprs=*(*self.PRs)[wh[i]].PRs
     theserange=*(*self.PRs)[wh[i]].RANGE
     whclose=where(x ge theserange[0,*] AND y ge theserange[1,*] AND $
                   x le theserange[2,*] AND y le theserange[3,*],cnt)
     if cnt eq 0 then continue
     for j=0,cnt-1 do begin 
        thispr=theseprs[whclose[j]]
        xp=thispr.x & yp=thispr.y
        yp2=shift(yp,1) & xp2=shift(xp,1)
        ;; Check for odd number of crossings (remember odd numbers are true!)
        ;; based on Randolph Franklin's page:
        ;; http://www.ecse.rpi.edu/Homepages/wrf/research/geom/pnpoly.html
        if fix(total(((yp le y AND y lt yp2) OR (yp2 le y AND y lt yp)) AND $
                     (x lt ((xp2 - xp)*(y - yp)/(yp2 - yp) + xp)))) $
           then begin 
           ord=(*self.PRs)[wh[i]].ORDER
           return,thispr
        endif 
     endfor 
  endfor 
  return,-1
end

;=============================================================================
;  UpdateWAVSAMP - Get the new cube's WAVSAMP list (all orders) for the BCD
;=============================================================================
pro cvLine::UpdateWAVSAMP,MODULE=md
  if ptr_valid(self.PRs) then $
     ptr_free,(*self.PRs).PRs,(*self.PRs).RANGE,self.PRs
  if obj_valid(self.cube) then begin
     self.cube->GetProperty,CALIB=calib
     if obj_valid(calib) then self.calib=calib
     prs=self.cube->PRs(/ALL_ORDERS,ORDERS=ords,/FULL,WAVECUT=0) 
  endif else if obj_valid(self.calib) then begin
     ords=self.calib->Orders(md)
     prs=ptrarr(n_elements(ords))
     for i=0,n_elements(ords)-1 do $
        prs[i]=ptr_new(self.calib->GetWAVSAMP(md,ords[i],/PIXEL_BASED,/FULL))
  endif
  
  nords=n_elements(prs) 
  self.PRs=ptr_new(replicate({ORDER:0,PRs:ptr_new(),MIN:[0.,0.],MAX:[0.,0.], $
                              RANGE:ptr_new()},nords))
  for ord=0,nords-1 do begin
     minx=min((*prs[ord]).x,DIMENSION=1,max=maxx) ; min and max
     miny=min((*prs[ord]).y,DIMENSION=1,max=maxy) ; for each PR sample
     (*self.PRs)[ord].RANGE=ptr_new([transpose(minx),transpose(miny), $
                                     transpose(maxx),transpose(maxy)])
     (*self.PRs)[ord].PRs=prs[ord]
     minx=min(minx)& maxx=max(maxx) ;for the order in general
     miny=min(miny) & maxy=max(maxy)
     (*self.PRs)[ord].MIN=[minx,miny]
     (*self.PRs)[ord].MAX=[maxx,maxy]
     (*self.PRs)[ord].ORDER=ords[ord]
  endfor
end

;=============================================================================
;  String - The total string
;=============================================================================
function cvLine::String, im,point,LAMBDA=lambda,ORDER=order,RA=ra,DEC=dec, $
                         FORCE=force
  if ~array_equal(point,self.savpoint) || keyword_set(force) then begin 
     self.valstring=self->ValString(im,point,EXTRA_STRING=ext)
     self.extrastring=ext
  endif 
  return,self.valstring+' | '+ $
         (self.bcd_mode?self->PRString(lambda,order):self->WCSString(ra,dec))+$
         self.extrastring
end

;=============================================================================
;  ValString - The string value associated with point X,Y
;=============================================================================
function cvLine::ValString, im,point,EXTRA_STRING=ext
  pt=floor(point)
  if self.bcd_mode && ptr_valid(self.mask) then begin 
     m=(*self.mask)[pt[0],pt[1]]
     if m ne 0 then begin 
        irs_bmask,m,CODE_STRING=cs
        ext=string(FORMAT='(" ",A10)',cs)
     endif else ext='           '
  endif else ext=''
  if ptr_valid(self.UNC) then $
     val=string(FORMAT='(G10.4," ",A0," ",G10.4)', $
                (*im)[pt[0],pt[1]],string(177b), $
                (*self.UNC)[pt[0],pt[1]]) $
  else val=string(FORMAT='(G14.8)',(*im)[pt[0],pt[1]])
  if self.bcd_mode then i='I3' else i='I4'
  return,string(FORMAT='("(",'+i+',",",'+i+',")", A23)',pt, val)
end

;=============================================================================
;  PRString - The string value associated with wavelength and order
;=============================================================================
function cvLine::PRString, lambda,order
  if order eq 0 then $
     return,string(FORMAT='(%"%6sum (ord %2s)")','___','__')  else $
     return,string(FORMAT='(%"%6.3fum (ord %2d)")',lambda,order)
end

;=============================================================================
;  WCSString - The string value associated with the position
;=============================================================================
function cvLine::WCSString, ra,dec
  return,radecstring(ra,/RA)+' '+radecstring(dec)
end

;=============================================================================
;  Cleanup
;=============================================================================
pro cvLine::Cleanup
  if ptr_valid(self.PRs) then $
     ptr_free,(*self.PRs).PRs,(*self.PRs).RANGE,self.PRs
  ptr_free,self.astrometry
  self->tvPlug_lite::Cleanup
end

;=============================================================================
;  Init - Initialize the line.
;=============================================================================
function cvLine::Init,oDraw,parent,CALIB=calib,MODULE=module,_EXTRA=e
  if (self->tvPlug_lite::Init(oDraw,_EXTRA=e) ne 1) then return,0 ;chain up
  r=widget_base(parent,/ROW,/SPACE)
  self.wLine=widget_label(r,VALUE=string(FORMAT='(T64,A)',''))
  
  ;; Called outside the cube context
  if n_elements(module) ne 0 then begin 
     self.bcd_mode=1
     if obj_valid(calib) then $
        self.calib=calib else self.calib=irs_restore_calib(irs_recent_calib())
     self->UpdateWAVSAMP,MODULE=module
  endif 
     
  ;; specify motion, tracking and postdraw events... we're always on
  self.oDraw->MsgSignup,self,/DRAW_MOTION,/WIDGET_TRACKING,/TVDRAW_POSTDRAW
  return,1
end 

;=============================================================================
;  cvLine__define - Prototype the cvLine class.
;=============================================================================
pro cvLine__define
  struct={cvLine, $ 
          INHERITS tvPlug_lite,$ ;make it a plug-in
          savpoint: [0,0], $    ;point to save
          savorder:0, $         ;the saved order found
          savlambda:0.0, $      ;the saved WL found
          valstring:'', $       ;the array value string
          extrastring:'', $     ;anything else to add
          bcd_mode:0, $         ;whether bcd or cube mode
          PRs:ptr_new(), $      ;all the pseudo-rectangles
          cube:obj_new(), $     ;
          calib:obj_new(), $    ;
          mask:ptr_new(), $     ;mask data to display
          unc:ptr_new(), $      ;uncertainty data to display (if any)
          astrometry:ptr_new(),$ ;cube astrometry record
          module:'', $          ;the module that came with the latest bcd/cube
          wLine:0L}             ;widget id of text line
  return
end
