
#
# vers. 1.0.0
# last modified: 07.04.13
#



#
# Generation of a 2D multifractal lattice (2D grayscale image)
# by using a random multiplicative process (Meakin method)
#


#
# -- References --
#
# P. Meakin, "Random walks on multifractal lattices",
# J. Phys. A: Math. Gen. 20 (1987) L771-L777
#
# L. Milazzo, "Multifractal analysis of three-dimensional grayscale
# images: Estimation of generalized fractal dimension and singularity
# spectrum", (to be submitted)
#


#--

# working directory
working_dir = "<path-to-wd>"


#--

applyRandomMultiplProc2D<- function(no_pixels_x, pxls_v, probs) {

  if(no_pixels_x == 2){
    return(pxls_v)
  }

  # internal edge
  iedge<- no_pixels_x/2

  # sampling procedure;
  # generating four random probability values
  probs_s<- sample(probs)


  # pixels values associated with box 1
  pxls_v_b1<- pxls_v[1:iedge, 1:iedge] * probs_s[1]

  no_pixels_x_b1<- no_pixels_x/2
  pxls_v_b1<- applyRandomMultiplProc2D(no_pixels_x_b1, pxls_v_b1, probs)

  pxls_v[1:iedge, 1:iedge]<- pxls_v_b1

  
  # pixels values associated with box 2
  pxls_v_b2<- pxls_v[1:iedge, (iedge+1):no_pixels_x] * probs_s[2]

  no_pixels_x_b2<- no_pixels_x/2
  pxls_v_b2<- applyRandomMultiplProc2D(no_pixels_x_b2, pxls_v_b2, probs)

  pxls_v[1:iedge, (iedge+1):no_pixels_x]<- pxls_v_b2


  # pixels values associated with box 3
  pxls_v_b3<- pxls_v[(iedge+1):no_pixels_x, 1:iedge] * probs_s[3]

  no_pixels_x_b3<- no_pixels_x/2
  pxls_v_b3<- applyRandomMultiplProc2D(no_pixels_x_b3, pxls_v_b3, probs)

  pxls_v[(iedge+1):no_pixels_x, 1:iedge]<- pxls_v_b3


  # pixels values associated with box 4
  pxls_v_b4<- pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x] * probs_s[4]

  no_pixels_x_b4<- no_pixels_x/2
  pxls_v_b4<- applyRandomMultiplProc2D(no_pixels_x_b4, pxls_v_b4, probs)

  pxls_v[(iedge+1):no_pixels_x, (iedge+1):no_pixels_x]<- pxls_v_b4


  return(pxls_v)

}



#--

#
# in the case of grayscale images, the pixel values lay in
# the range [0,255] and can be considered as measures of mass
# (0 = black = no mass; 255 = white = mass);
#


# number of pixels (128x128)
no_pixels<- 16384

# number of pixels in a given direction
no_pixels_x<- sqrt(no_pixels)

# let us consider a white image (255=white) ...
# initial pixels values
pxls_v<- array(255, dim=c(no_pixels_x, no_pixels_x))

# initial probability values
probs<- c(1, 1, 0.5, 0.5)


# generating a 2D multifractal lattice
pxls_v<- applyRandomMultiplProc2D(no_pixels_x, pxls_v, probs)

# rounding the pixel values
pxls_v = round(pxls_v)


# I/O processing - writing output file
write.table(pxls_v, paste(working_dir, "synth_image.dat", sep=""),
            row.names=FALSE, col.names=FALSE)
