FCN_FILE_DIRS += image

image_PRIVATE_FCN_FILES = \
  image/private/__imfinfo__.m \
  image/private/__imread__.m \
  image/private/__imwrite__.m \
  image/private/imageIO.m \
  image/private/imwrite_filename.m \
  image/private/ind2x.m

image_FCN_FILES = \
  image/autumn.m \
  image/bone.m \
  image/brighten.m \
  image/cmpermute.m \
  image/cmunique.m \
  image/colorcube.m \
  image/colormap.m \
  image/contrast.m \
  image/cool.m \
  image/copper.m \
  image/cubehelix.m \
  image/flag.m \
  image/gray.m \
  image/gray2ind.m \
  image/hot.m \
  image/hsv.m \
  image/hsv2rgb.m \
  image/iscolormap.m \
  image/image.m \
  image/imagesc.m \
  image/imfinfo.m \
  image/imformats.m \
  image/imread.m \
  image/imshow.m \
  image/imwrite.m \
  image/ind2gray.m \
  image/ind2rgb.m \
  image/jet.m \
  image/lines.m \
  image/ntsc2rgb.m \
  image/ocean.m \
  image/pink.m \
  image/prism.m \
  image/rainbow.m \
  image/rgb2hsv.m \
  image/rgb2ind.m \
  image/rgb2ntsc.m \
  image/rgbplot.m \
  image/spinmap.m \
  image/spring.m \
  image/summer.m \
  image/white.m \
  image/winter.m \
  $(image_PRIVATE_FCN_FILES)

IMAGES += \
  image/default.img

FCN_FILES += $(image_FCN_FILES)

PKG_ADD_FILES += image/PKG_ADD

DIRSTAMP_FILES += image/$(octave_dirstamp)
