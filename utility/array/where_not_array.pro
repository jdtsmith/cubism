;+
; NAME:
;       WHERE_NOT_ARRAY
;
; PURPOSE:
; 	Return the indices of those elements in vector B which
;       do not exist in vector A.  
;
; CATEGORY:
;       Array
;
; CALLING SEQUENCE:
;       result = WHERE_NOT_ARRAY(A,B)
;
; INPUTS:
;       A       vector that might contains elements of vector B
;       B       vector wthat we would like to know which of its
;               elements exist in A
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;       iA_in_B         return instead the indices of A that are not in
;                       (exist) in B
;
; OUTPUTS:
;       Index into B of elements not found in vector A.  If no
;       matches are found -1 is returned.  If the function is called
;       with incorrect arguments, a warning is displayed, and -2 is
;       returned (see SIDE EFFECTS for more info)
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;               None
;
; SIDE EFFECTS:
;       If the function is called incorrectly, a message is displayed
;       to the screen, and the !ERR_STRING is set to the warning
;       message.  No error code is set, because the program returns
;       -2 already
;
; RESTRICTIONS:
;       This should be used with only Vectors.  Matrices other then
;       vectors will result in -2 being returned.  Also, A and B must
;       be defined, and must not be strings!
;
; PROCEDURE:
;
; SEE ALSO:
;       where
;
; MODIFICATION HISTORY:
;       Written by:     
;                       Smith, JD 9/9/98
;-

function where_not_array,A,B,cnt,IA_IN_B=iA_in_B

   Na = n_elements(a)
   Nb = n_elements(b)
   l = lindgen(Na,Nb)
   AA = A(l mod Na)
   BB = B(l / Na)

   if keyword_set(iA_in_B) then wh = where(total(AA ne BB,2) eq Nb,cnt) $
   else wh = where(total(AA ne BB,1) eq Na,cnt) 
   
   return,wh
   
end

