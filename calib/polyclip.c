/* 
NAME:

  POLYCLIP 

DESCRIPTION:

  Clips an input polygon to multiple pixels on a square grid in a
  single pass using Sutherland-Hodgeman clipping, and returns the
  clipped polygons, along with their areas.  Uses a
  REVERSE_INDICES-like vector (see HISTOGRAM) to permit decoding the
  two return vectors into individual polygons clipped for each pixel
  (if any).  Much faster (~50x) than the IDL-loop version polyclip.pro

CALLING SEQUENCE:

  tmp=call_external(polyclip_path,'polyclip', $
                    VALUE= $
		    [0b,0b,1b,   0b,0b,1b,    0b,    0b,    0b,   0b], $
		    vi,vj,npix,  px,py,npol,  px_out,py_out,areas,ri_out)

INPUTS: 

  vi,vj:  A vector of pixel coordinates against which to clip the polygon.
  npix: The number of pixels passed.
  px,py: The polygon as a series of points.  Need not be closed.
  npol: The number of points in the polygon.

OUTPUTS:

  px_out,py_out: The output polygons, concatenated together.  Must be
    pre-initialized to contain at least npix*(npol+4) points (for
    convex polygons), or more for general polygons.
  areas: The output areas of each polygon, must be pre-initialized as
    a vector of length npix.
  ri_out: The reverse index vector, pre-initialzed as a long vector of
    length npix+1, subsequent entries give the range of indices in the
    p(x|y)_out vectors that correspond to that pixel's clipped
    coordinates.

EXAMPLE:

  make_dll,'polyclip','polyclip'
  xy=[76.65864,78.83240,44.87576,79.87564,$
      44.84472,78.90629,76.62761,77.86305]
  px=xy[indgen(4)*2] & py=xy[indgen(4)*2+1] & sz=[128,128]
  plot,[px,px[0]],[py,py[0]],yrange=[77,80]
  for r=!X.CRANGE[0],!X.CRANGE[1],1 do plots,r,!Y.CRANGE
  for r=!Y.CRANGE[0],!Y.CRANGE[1],1 do plots,!X.CRANGE,r
  left=floor(min(px,max=maxx))>0 &   right=floor(maxx)<(sz[0]-1)
  bottom=floor(min(py,max=maxy))>0 & top=floor(maxy)<(sz[1]-1)
  nx=right-left+1 & ny=top-bottom+1
  ind=reform(rebin(indgen(nx),nx,ny)+left+ $
      (rebin(transpose(indgen(ny)),nx,ny)+bottom)*sz[0],nx*ny)
  vi=ind mod sz[0] & vj=ind/sz[0]                                
  tmp=call_external(polyclip_path,'polyclip', $
                    VALUE= $
		    [0b,0b,1b,   0b,0b,1b,    0b,    0b,    0b,   0b], $
		    vi,vj,npix, px,py,npol, px_out,py_out,areas,ri_out) */

/*#############################################################################
 LICENSE

  Copyright (C) 2002,2003 J.D. Smith

  This file is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 2, or (at your
  option) any later version.
  
  This file is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this file; see the file COPYING.  If not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
#############################################################################*/
/*    $Id$*/

#include <stdlib.h>

void polyclip(int argc, void *argv[]) {
  /* Calling: */
  /* polyclip(vi,vj,nv,poly_x,poly_y,np_in,px_out,py_out,areas_out,ri_out) */
  int i,j,*vi,*vj,*ri_out,np_in,np,nv,pix;
  float *poly_x, *poly_y, *px_out, *py_out,*areas_out;
  int ctype,in_s,in_p,k,k1,ind=0,ri_ind=0,tmp_ind;
  float sx,sy,px,py,*px_in,*py_in;            /* Internal storage */
  
  /* Place arguments */
  vi=(int *)argv[0];vj=(int *)argv[1];nv=(int)argv[2];
  poly_x=(float *)argv[3];poly_y=(float *)argv[4];np_in=(int)argv[5];
  px_out=(float *)argv[6];py_out=(float *)argv[7];areas_out=(float *)argv[8];
  ri_out=(int *)argv[9];

  /* Temporary storage, at most 4 additional points for convex polys */
  px_in=(float *)malloc((np+4)*sizeof(float));
  py_in=(float *)malloc((np+4)*sizeof(float));
  
  ri_out[0]=0;
  
  for(pix=0;pix<nv;pix++) {
    i=vi[pix]; j=vj[pix];
    np=np_in;
    //printf("Starting with:\n");
    for(k=0;k<np;k++) {
      px_in[k]=poly_x[k]; py_in[k]=poly_y[k];
      //printf("(%7.3f,%7.3f) ",px_in[k],py_in[k]);
    }
    //printf("\n\n");
    
    for(ctype=0;ctype<4;ctype++) {
      //printf("Round: %d, pixel (%d,%d)\n",ctype,i,j);
      ind=0;
      for(k=-1;k<np;k++) {
	k1=(k==-1)?np-1:k;
	px=px_in[k1];py=py_in[k1];
	switch(ctype) {
	case 0: 
	  in_p=(px>i)?1:0;break;
	case 1: 
	  in_p=(px<i+1)?1:0;break;
	case 2: 
	  in_p=(py<j+1)?1:0;break;
	case 3: 
	  in_p=(py>j)?1:0;break;
	}

	if(k>=0) {
	  //printf("%d: (%7.3f,%7.3f) [%s] -> (%7.3f,%7.3f) [%s]\n",
	  // k,sx,sy,in_s?"in":"out",px,py,in_p?"in":"out");

	  if (in_s^in_p) { /* Crossing boundary */
	    tmp_ind=ri_ind+ind;
	    switch(ctype) {
	    case 0: /* Left */
	      px_out[tmp_ind]=i;
	      py_out[tmp_ind]=sy+(py-sy)/(px-sx)*(i-sx);
	      break;
	    case 1: /* Right */
	      px_out[tmp_ind]=i+1;
	      py_out[tmp_ind]=sy+(py-sy)/(px-sx)*(i+1-sx);
	      break;
	    case 2: /* Top */
	      px_out[tmp_ind]=sx+(px-sx)/(py-sy)*(j+1-sy);
	      py_out[tmp_ind]=j+1;
	      break;
	    case 3: /* Bottom */
	      px_out[tmp_ind]=sx+(px-sx)/(py-sy)*(j-sy);
	      py_out[tmp_ind]=j;
	      break;
	    }
	    ind++;
	  }
	  if(in_p) { /* Inside */
	    tmp_ind=ri_ind+ind;
	    px_out[tmp_ind]=px; py_out[tmp_ind]=py; ind++; 
	  }
	}
	in_s=in_p;sx=px;sy=py;
      }
      if(ind==0 || ctype==3) break; /* Done, or entirely outside this plane */
      np=ind;
      //printf("IND: %d\n",ind,ri_ind);
      for(k=0;k<np;k++){
	//printf("(%7.3f,%7.3f) ",px_out[ri_ind+k],py_out[ri_ind+k]);
	tmp_ind=ri_ind+k;
	px_in[k]=px_out[tmp_ind]; 
	py_in[k]=py_out[tmp_ind];
      }
      //printf("\n");
    }
    //printf("IND: %d, RI_IND: %d\n",ind,ri_ind);
    /* Compute the area of this polygon */
    areas_out[pix]=0.0;
    for(k=0;k<ind;k++) {
      tmp_ind=ri_ind+k;
      k1=(k==ind-1?ri_ind:tmp_ind+1);
      //printf("pix: %d, %d %d: %f %f %f %f %f\n",pix,tmp_ind,k1,px_out[tmp_ind],
      //     py_out[tmp_ind],px_out[tmp_ind]*py_out[k1],
      //     -py_out[tmp_ind]*px_out[k1],
      //     px_out[tmp_ind]*py_out[k1]-py_out[tmp_ind]*px_out[k1]);
      areas_out[pix]+=px_out[tmp_ind]*py_out[k1]-py_out[tmp_ind]*px_out[k1];
      //printf(" ==> %f\n",areas_out[pix]/2.);
    }
    areas_out[pix]/=2.;
    if(areas_out[pix]<0.) areas_out[pix]=-areas_out[pix];
    /* Update the running index */
    ri_ind+=ind;
    ri_out[pix+1]=ri_ind;
  }
  free(px_in); free(py_in);
}

char polyclip_test() {
  return 42;
}
