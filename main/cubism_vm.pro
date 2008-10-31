;+
; NAME:  
;
;    CUBISM_VM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.stsci.edu/cubism
;
; DESCRIPTION:
;    
;    Simple cubism main-level routine for the runtime ".sav" file.
;    Accepts arguments via IDL 6.2's new COMMAND_LINE_ARGUMENTS
;    routine.
;
; RESTRICTIONS:
;
;    Do not call interactively, only via the ".sav" file produced (see
;    compile_cubism.pro).
;    
; EXAMPLE:
;
;    idl -vm=/path/to/cubism_vm.sav -arg /starting/path -arg=/path/to/cube.cpj
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003,2005 J.D. Smith
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

;; Convert form feeds back to spaces, since idl -arg can't handle spaces
pro cubism_vm_reencode_spaces,arg
  b=byte(arg)
  wh=where(b eq 12b,fcnt)       ;form feeds
  if fcnt gt 0 then begin 
     b[wh]=32b
     arg=string(b)
  endif 
end

pro cubism_vm,pname
  @cubism_dir                   ; to freeze the relative cubism directory
  common cubism_vm_command_line_args, used_args
  command_line=~lmgr(/VM) && ~lmgr(/RUNTIME)
  if n_elements(used_args) eq 0 then begin 
     catch,err
     if err eq 0 then begin 
        args=call_function('command_line_args',COUNT=cnt)
        ;; only process the args once, in case called interactively
        used_args=1b 
        if cnt gt 0 then begin 
           wdir=args[0] ;; first arg, initial working directory
           cubism_vm_reencode_spaces,wdir
           if file_test(wdir,/DIRECTORY) then cd,wdir
           if cnt gt 1 then begin 
              file=args[1]
              cubism_vm_reencode_spaces,file
              if file_test(file,/READ) && $
                 strpos(file,'.cpj') eq strlen(file)-4 then pname=file
           endif 
        endif 
     endif
     catch,/cancel
  endif
  device,DECOMPOSED=0,RETAIN=2,TRUE=24
  if ~command_line then begin 
     XManager,CATCH=0
     catch,err
     if err ne 0 then begin 
        XManager                ;just silently restart event processing
        catch,/cancel
        return
     endif 
  endif 
  cubism,pname,NO_BLOCK=command_line
end
