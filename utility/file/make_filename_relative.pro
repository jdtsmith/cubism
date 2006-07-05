;+
; NAME:
;
;   MAKE_FILENAME_RELATIVE
;
; PURPOSE:
;
;   Given a filename and a path, make the former relative to the latter.
;
; CATEGORY:
;
;   Filenames
;
; CALLING SEQUENCE:
;
;   new=make_filename_relative(file,root)
;
; INPUTS:
;
;   file: The absolute filename to turn into a relative filename.
;
;   root: The path of the root directory from which to make a relative
;      path to file.
;
; OUTPUTS:
;
;   new: The new relative filename.
;
;
; MODIFICATION HISTORY:
;
;   2006-07-05 (JDS): Written
;
;-
;    $Id$
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2006 J.D. Smith
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

function make_filename_relative,path,root
  up=0L
  sep=path_sep()
  
  while root ne sep do begin 
     match=path
     while match ne sep do begin 
        match=file_dirname(match)
        if file_same(match,root) then begin 
           ;; replace match by ./ or ../
           return,(up eq 0L?'.':strjoin(replicate(path_sep(/PARENT),up),sep))+ $
                  strmid(path,strlen(match))
        endif 
     endwhile 
     root=file_dirname(root)
     up++
  endwhile 
  return, '.'+path
end
