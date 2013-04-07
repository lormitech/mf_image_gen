#!/usr/bin/python

#
# file name: convert_img_ascii-tif.py
# last modified: 03.04.13
#


#
# script to convert an image from an ASCII to a TIF format
#


import getopt, os, subprocess, sys
import math


#--

# Image Magick path
convert_path = '/usr/bin/convert'
# working directory
working_dir = os.getcwd() + '/'

#--

def convertImage_fromASCIItoTIF(file_name):
    convert_args = []
    convert_args.append(convert_path)
    convert_args.append(working_dir + 'image_ascii_tmp.txt')
    convert_args.append(working_dir + file_name + '.tif')
    subprocess.Popen(convert_args)


def getFileName(initial_file):
    current_working_dir = os.path.split(initial_file)[0]
    if (current_working_dir == ''):
        current_working_dir = os.getcwd()
    full_file_name = os.path.split(initial_file)[1]
    file_name = os.path.splitext(full_file_name)[0]

    return file_name


def makeImageMagickInput(init_file):
    initfile = open(init_file, 'r')
    initdata = initfile.read()
    pxls_values = initdata.split()
    no_pixels = len(pxls_values)
    no_pixels_dir = math.sqrt(no_pixels)

    outfile = open(working_dir + 'image_ascii_tmp.txt', 'w')
    line = '# ImageMagick pixel enumeration: ' + str(int(no_pixels_dir)) + ',' + str(int(no_pixels_dir)) + ',255,rgb\n'
    outfile.write(line)
    idx_pv = 0
    for i in range(int(no_pixels_dir)):
        for j in range(int(no_pixels_dir)):
            x = str(j)
            y = str(i)
            line = x + ',' + y + ': (' + pxls_values[idx_pv] + ', ' + pxls_values[idx_pv] + ', ' + pxls_values[idx_pv] + ')\n'
            outfile.write(line)
            idx_pv += 1

    outfile.close()


def usage():
    print ("Usage: %s "
           "[-h] "
           "<ASCII_image_file> " % sys.argv[0])
    sys.exit(2)



#--

if __name__== "__main__":

    try:
        (options, args) = getopt.gnu_getopt(sys.argv[1:], "h:", ["help"]) 
    except getopt.GetoptError:
        usage()
        sys.exit(1)

    if (len(args) != 1):
        usage()
        sys.exit(1)
    
    for o, a in options:
        if o in ("-h", "--help"):
            usage()
            sys.exit()

    initial_file = args[0]
    
    makeImageMagickInput(initial_file)

    file_name = getFileName(initial_file)

    convertImage_fromASCIItoTIF(file_name)
