
#
# vers. 1.0.0
# last modified: 23.05.16
# author: Lorenzo Milazzo
#



#
# Generation of a 3D multifractal lattice (3D grayscale image)
# by using a random multiplicative process (Meakin method)
#


#
# -- References --
#
# P. Meakin, "Random walks on multifractal lattices",
# J. Phys. A: Math. Gen. 20, L771-L777 (1987)
#
# L. Milazzo, "Multifractal analysis of three-dimensional grayscale images:
# Estimation of generalized fractal dimension and singularity spectrum",
# arXiv:1310.2719 [cond-mat.stat-mech] (2013)
#


#--

# working directory
working_dir = "<path-to-wd>"


#--

applyRandomMultiplProc3D<- function(no_pixels_x, pxls_v, probs) {

  if(no_pixels_x == 2){
    return(pxls_v)
  }

  # internal edge
  iedge<- no_pixels_x/2

  # sampling procedure;
  # generating four random probability values
  probs_s<- sample(probs)


  # pixels values associated with box 1
  pxls_v_b1<- pxls_v[1:iedge, 1:iedge, 1:iedge] * probs_s[1]

  no_pixels_x_b1<- no_pixels_x/2
  pxls_v_b1<- applyRandomMultiplProc3D(no_pixels_x_b1, pxls_v_b1, probs)

  pxls_v[1:iedge, 1:iedge, 1:iedge]<- pxls_v_b1

  
  # pixels values associated with box 2
  pxls_v_b2<- pxls_v[1:iedge, (iedge+1):no_pixels_x, 1:iedge] * probs_s[2]

  no_pixels_x_b2<- no_pixels_x/2
  pxls_v_b2<- applyRandomMultiplProc3D(no_pixels_x_b2, pxls_v_b2, probs)

  pxls_v[1:iedge, (iedge+1):no_pixels_x, 1:iedge]<- pxls_v_b2


  # pixels values associated with box 3
  pxls_v_b3<- pxls_v[(iedge+1):no_pixels_x, 1:iedge, 1:iedge] * probs_s[3]

  no_pixels_x_b3<- no_pixels_x/2
  pxls_v_b3<- applyRandomMultiplProc3D(no_pixels_x_b3, pxls_v_b3, probs)

  pxls_v[(iedge+1):no_pixels_x, 1:iedge, 1:iedge]<- pxls_v_b3


  # pixels values associated with box 4
  pxls_v_b4<- pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x, 1:iedge] * probs_s[4]

  no_pixels_x_b4<- no_pixels_x/2
  pxls_v_b4<- applyRandomMultiplProc3D(no_pixels_x_b4, pxls_v_b4, probs)

  pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x, 1:iedge]<- pxls_v_b4


  # pixels values associated with box 5
  pxls_v_b5<- pxls_v[1:iedge, 1:iedge, (iedge+1):no_pixels_x] * probs_s[5]

  no_pixels_x_b5<- no_pixels_x/2
  pxls_v_b5<- applyRandomMultiplProc3D(no_pixels_x_b5, pxls_v_b5, probs)

  pxls_v[1:iedge, 1:iedge, (iedge+1):no_pixels_x]<- pxls_v_b5


  # pixels values associated with box 6
  pxls_v_b6<- pxls_v[1:iedge, (iedge+1):no_pixels_x, (iedge+1):no_pixels_x] * probs_s[6]

  no_pixels_x_b6<- no_pixels_x/2
  pxls_v_b6<- applyRandomMultiplProc3D(no_pixels_x_b6, pxls_v_b6, probs)

  pxls_v[1:iedge, (iedge+1):no_pixels_x, (iedge+1):no_pixels_x]<- pxls_v_b6


  # pixels values associated with box 7
  pxls_v_b7<- pxls_v[(iedge+1):no_pixels_x, 1:iedge, (iedge+1):no_pixels_x] * probs_s[7]

  no_pixels_x_b7<- no_pixels_x/2
  pxls_v_b7<- applyRandomMultiplProc3D(no_pixels_x_b7, pxls_v_b7, probs)

  pxls_v[(iedge+1):no_pixels_x, 1:iedge, (iedge+1):no_pixels_x]<- pxls_v_b7


  # pixels values associated with box 8
  pxls_v_b8<- pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x, (iedge+1):no_pixels_x] * probs_s[8]

  no_pixels_x_b8<- no_pixels_x/2
  pxls_v_b8<- applyRandomMultiplProc3D(no_pixels_x_b8, pxls_v_b8, probs)

  pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x, (iedge+1):no_pixels_x]<- pxls_v_b8


  return(pxls_v)

}



#--

#
# a 3D image is constituted by a stack of 2D images
#
# in the case of grayscale images, the pixel values lay in
# the range [0,255] and can be considered as measures of mass
# (0 = black = no mass; 255 = white = mass);
#


# number of pixels (128x128x128)
no_pixels<- 2097152

# number of pixels in a given direction
no_pixels_x = no_pixels^(1/3)
no_pixels_x = ceiling(no_pixels_x)

# let us consider a set of white images (255=white) ...
# initial pixels values
pxls_v<- array(255, dim=c(no_pixels_x, no_pixels_x, no_pixels_x))

# initial probability values
probs<- c(1, 1, 1, 1, 0.5, 0.5, 0.5, 0.5)


# generating a 3D multifractal lattice
pxls_v<- applyRandomMultiplProc3D(no_pixels_x, pxls_v, probs)

# rounding the pixel values
pxls_v = round(pxls_v)


# I/O processing - writing output files
for (i in 1:no_pixels_x) {
  write.table(pxls_v[,,i], paste(working_dir,
                                 "synth_image_",
                                 formatC(i, format="d", width=3, flag="0"),
                                 ".dat",
                                 sep=""),
              row.names=FALSE, col.names=FALSE)
}
