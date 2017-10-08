# Small helper scripts for use in GIMP's Python console.  See fill-errors.md for more information.

import os.path

def load_layer(image, filename, layer_name):
  """Used by other functions to load a layer"""
  layer = pdb.gimp_file_load_layer(image, filename)
  pdb.gimp_drawable_set_name(layer, layer_name)
  pdb.gimp_image_insert_layer(image, layer, None, -1)
  return layer

def load_error_image(number, directory = "images", error_directory = "."):
  """Load debugging information about a failed attempt to draw an image"""
  # Load the original bitmap:
  filename = directory + "/" + str(number) + "-a.pnm"
  image = pdb.file_pnm_load(filename,filename)
  pdb.gimp_image_undo_disable(image)
  pdb.gimp_image_set_filename(image, error_directory + "/error/" + str(number) + ".pnm")
  
  # Load the paths that described the failed fill from error.svg:
  pdb.gimp_path_import(image, error_directory + "/error/" + str(number) + ".svg", False, False)
  for item in image.vectors:
    pdb.gimp_item_set_visible(item, True)
  
  # flip and resize the canvas so each pixel's GIMP co-ordinates match their Illustrator co-ordinates
  # (The Illustrator puts [0,0] in the bottom-left, and draws the topmost 96 of 176 rows)
  pdb.gimp_image_resize(image, 256, 176, 0, 0)
  pdb.gimp_item_transform_flip_simple(image.layers[0], 1, 0, 88)
  
  # Load the pixels inside (and on the border of) the failed fill from error.pnm:
  layer = load_layer(image, error_directory + "/error/" + str(number) + ".pnm", "area pixels")
  pdb.gimp_layer_add_alpha(layer)
  pdb.plug_in_colortoalpha(image, layer, 'black')
  
  # Display the image:
  display = pdb.gimp_display_new(image)
  pdb.gimp_image_undo_enable(image)
  pdb.gimp_image_clean_all(image)


def load_image(number, directory = "images"):
  """Load the complete set of information about an image that was drawn successfully"""
  # Load the paper layer:
  filename = directory + "/" + str(number) + "-paper.pnm"
  image = pdb.file_pnm_load(filename,filename)
  pdb.gimp_image_undo_disable(image)
  pdb.gimp_image_set_filename(image, str(number) + ".pnm")
  pdb.gimp_drawable_set_name(image.layers[0], "Paper")
  
  # Load the ink layer:
  layer = load_layer(image, directory + "/" + str(number) + "-ink.pnm", "Ink")
  
  # Resize the image:
  pdb.gimp_context_set_interpolation(0)
  pdb.gimp_image_scale(image, 256, 176)
  
  # Load the selection layer:
  load_layer(image, directory + "/" + str(number) + "-selection.pnm", "Ink/Paper Selection")
  
  # Load the main image:
  if os.path.isfile(directory + "/" + str(number) + "-b.pnm"):
    load_layer(image, directory + "/" + str(number) + "-a.pnm", "main image (flash off)")
    load_layer(image, directory + "/" + str(number) + "-a.pnm", "main image (flash on)")
  else:
    load_layer(image, directory + "/" + str(number) + "-a.pnm", "main image")
  
  # flip and resize the canvas so each pixel's GIMP co-ordinates match their Illustrator co-ordinates
  # (The Illustrator puts [0,0] in the bottom-left, and draws the topmost 96 of 176 rows)
  for layer in image.layers:
    pdb.gimp_item_transform_flip_simple(layer, 1, 0, 88)
  
  # Display the image:
  display = pdb.gimp_display_new(image)
  pdb.gimp_image_undo_enable(image)
  pdb.gimp_image_clean_all(image)
  return image

def select_pixels( pixels, image = None):
  """Add pixels to the current selection"""
  if image is None: image = gimp.image_list()[0]
  if isinstance(pixels, list):
    if isinstance(pixels[0], list):
      for value in pixels:
        select_pixels(value)
    else:
      pdb.gimp_image_select_rectangle(image, 0, pixels[0], pixels[1], 1, 1)
  else:
    raise "Please pass a list of pixels"
