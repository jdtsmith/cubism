;+
; NAME:  
;
;    WHERE_NOT_ARRAY
;
; DESCRIPTION:
;    
;    Find the indices of elements in a vector which are *not* present
;    in another vector.
;    
; CATEGORY:
;
;    Array
;    	
; CALLING SEQUENCE:
;
;    wh=where_not_array(A,B)
;
; INPUT PARAMETERS:
;
;    A: A vector of values to test for precense in B.
;    B: The vector in which elements not in A will be found.
;			
; INPUT KEYWORD PARAMETERS:
;
;    /SWITCHAB: Look for elements of A not in B instead of B not in A.
;
; OUTPUTS:
;
;    The locations in B (or A, if /SWITCHAB is used), which do not exist
;    in A (or B, if /SWITCHAB is used.)
;
; OPTIONAL OUTPUTS:
;
;    cnt: The count of elements whose indices are returned.
;    
; RESTRICTIONS:
;
;    Vectors only.
;
; EXAMPLE:
;
;    a=[1,3,4,5] & b=[2,3,5,6]
;    print,where_not_array(a,b)
;
; MODIFICATION HISTORY:
;
;       2002-08-27 (J.D. Smith): Migrated from SMART codebase.
;       2002-08-02 (J.D. Smith): Updated documentation and changed
;          keywords.
;       1998-09-09 (J.D. Smith): Written
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 1998-2002 JD Smith
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
;  along with SMART; see the file COPYING.  If not, write to the Free
;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;  02111-1307, USA.
;
;##############################################################################

function where_not_array,A,B,cnt,SWITCHAB=sw

   Na = n_elements(a)
   Nb = n_elements(b)
   l = lindgen(Na,Nb)
   AA = A(l mod Na)
   BB = B(l / Na)

   if keyword_set(sw) then wh = where(total(AA ne BB,2) eq Nb,cnt) $
   else wh = where(total(AA ne BB,1) eq Na,cnt) 
   
   return,wh
end
