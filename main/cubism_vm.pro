;+
; NAME:  
;
;    CUBISM_VM
;
; CONTACT:
;
;    UPDATED VERSIONS of CUBISM and more information can be found at:
;       http://sings.sirtf.edu/cubism
;
; DESCRIPTION:
;    
;    Simple cubism main-level routine for the runtime ".sav" file.  
;
; RESTRICTIONS:
;
;    Do not call interactively, only via the ".sav" file produced (see
;    compile_cubism.pro).
;    
; EXAMPLE:
;
;    idl -vm=/path/to/cubism_vm.sav
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2002,2003 J.D. Smith
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
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################
pro cubism_vm
  @cubism_dir                   ; to freeze the relative cubism directory
  device,DECOMPOSED=0,RETAIN=2
  cubism,MODAL=0
end
