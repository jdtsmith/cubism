
function jul2date,jul,FORMAT=f
  if n_elements(f) eq 0 then f='standard'
  caldat, jul,m,d,y,hr,min,sec

  case f of
     'D*T': begin 
        y=y mod 100
        form='(2(I2.2,"/"),I2.2,"*",3(I2.2,:,":"))'
        return,string(format=form,m,d,y,hr,min,sec)
     end
     
     'standard': begin
        wday=(['Sun','Mon','Tue', $
               'Wed','Thu','Fri','Sat'])[floor((jul+1.5) mod 7)]
        mon=(['Jan','Feb','Mar','Apr','May', $
              'Jun','Jul','Aug','Sep','Oct','Nov','Dec'])[m-1]
        return,string(FORMAT='(A3," ",A3," ",I2," ",2(I2.2,":"),I2.2," ",' + $
                      'I4)',wday,mon,d,hr,min,floor(sec),y)
     end
     
     else: return,string(format=f,m,d,y,hr,min,sec)
  endcase
end
