;+
; NAME:  
;
;    CU_GET_FONTS
;
; DESCRIPTION:
;    
;    Get mono-space fonts as default for displaying in widgets.
;    
; CATEGORY:
;
;    Font Utility
;
; CALLING SEQUENCE:
;
;    f=cu_get_fonts([BOLD=,SIZE=,ITALIC=]
;
; INPUT KEYWORD PARAMETERS:
;
;    BOLD: Select a bold font (default non-bold)
;
;    SIZE: Select a font of this size.  Default 12 point.
;
;    ITALIC: Select an italic font (default non-italic).
;
; OUTPUT:
;
;    f: The name of the font
;    
; MODIFICATION HISTORY:
;    
;    2001-08-17 (J.D. Smith): Initial import from SCORE-era utilities.
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2001-2002 J.D. Smith
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

function cu_get_fonts, BOLD=b,SIZE=sz,ITALIC=i
  if keyword_set(b) then b='bold' else b='medium'
  if keyword_set(i) then i='i' else i='r'
  if NOT keyword_set(sz) then sz=12
  switch !VERSION.OS_FAMILY of
     'unix': $
        begin
        device,get_fontnames=df,font='*cour*'+b+'-'+i+'-normal*--'+ $
                 strtrim(sz,2)+'*'
        dispfont=df[0] 
        break
     end

     'Macintosh':      
     'Windows': $
        begin
        windows: 
        dispfont='COURIER'
        if keyword_set(b) then dispfont=dispfont+'*BOLD'
        if keyword_set(i) then dispfont=dispfont+'*ITALIC'
        if keyword_set(sz) then dispfont=dispfont+'*'+strtrim(sz,2)
        break
     end
     
     else: message, !VERSION.OS_FAMILY+' not supported.'
  endswitch
  return,dispfont
end
