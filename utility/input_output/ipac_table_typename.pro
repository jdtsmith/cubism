function ipac_table_typename,val,TYPE=type,FORMAT=format
  tname=["",replicate("int",3),"real","double","","char",replicate("",4), $
         replicate("int",4)]
  
  if n_elements(type) eq 0 then begin 
     if n_elements(val) eq 0 then message,'Must pass value or TYPE'
     type=size(val,/TYPE)
  endif 
  
  if arg_present(format) then $
     format=(["","I","I","I","G","G","","A",replicate("",4), $
              replicate("I",4)])[type]
  return,tname[type]
end
