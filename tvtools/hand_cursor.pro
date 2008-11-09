pro hand_cursor, CENTER=cent
  hand =       [                               $
               [096B, 027B],                   $
               [096B, 027B],                   $
               [096B, 219B],                   $
               [096B, 219B],                   $
               [102B, 219B],                   $
               [102B, 219B],                   $
               [102B, 219B],                   $
               [238B, 255B],                   $
               [254B, 255B],                   $
               [252B, 255B],                   $
               [252B, 255B],                   $
               [252B, 255B],                   $
               [248B, 255B],                   $
               [248B, 127B],                   $
               [240B, 127B],                   $
               [240B, 063B]                    $
               ]
   
  if n_elements(cent) eq 0 then cxy=[6,16] else cxy=[8,8]
  device,cursor_image=uint(hand[1,*])+ishft(uint(hand[0,*]),8), cursor_xy=cxy
end
