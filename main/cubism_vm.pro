;+
; NAME:  
;
;    CUBISM_VM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://ssc.spitzer.caltech.edu/cubism
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
;    idl -vm=/path/to/cubism_vm.sav -arg=/path/to/cube.cpj
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
pro cubism_vm,pname
  @cubism_dir                   ; to freeze the relative cubism directory
  common cubism_vm_command_line_args, used_args
  if n_elements(used_args) eq 0 then begin 
     catch,err
     if err eq 0b then begin 
        args=call_function('command_line_args',COUNT=cnt)
        ;; only process the args once, in case called interactively
        used_args=1b 
        if cnt gt 0 then $
           if file_test(args[0],/READ) && $
           strpos(args[0],'.cpj') eq strlen(args[0])-4 then pname=args[0]
     endif
     catch,/cancel
  endif
  device,DECOMPOSED=0,RETAIN=2,TRUE=24
  cubism,pname
end
