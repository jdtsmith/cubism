pro make_widget_adjacent,widget,base
  repeat begin
     tlb=base
     base=widget_info(base,/PARENT)
  endrep until base eq 0
  gb=widget_info(tlb,/GEOMETRY)
  ss=get_screen_size()
  
  gw=widget_info(widget,/GEOMETRY)
  
  left_side=(gb.xoffset+gb.scr_xsize/2) gt ss[0]/2
  xpos=left_side?gb.xoffset-2*gb.margin-gw.scr_xsize-6*gw.xpad: $
       gb.xoffset+2*gb.margin+gb.scr_xsize+2*gb.xpad
  ypos=gb.yoffset-(gb.scr_ysize-gb.ysize-3*gb.ypad)
  widget_control, widget,XOFFSET=xpos,YOFFSET=ypos
end
