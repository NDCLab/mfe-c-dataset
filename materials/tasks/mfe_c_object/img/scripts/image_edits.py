# Written by Kianoosh Hosseini (https://Kianoosh.info; https://NDCLab.com)
# This script loads images from object_images folder, resizes the images by 300 %,
# creates another white color image with 2444 × 1718 image dimensions,
# pastes the object image on this white image, and finally saves the new images in a different folder.
#
# Importing Required Modules
from rembg import remove
from PIL import Image
import pathlib
import math


main_dir = "/Users/kihossei/Documents/GitHub/mfe_c_object/materials/PsychopyTask/mfe_c_object/img"
# input_dir = "object_images"
input_dir = "new_50"
# input_dir = "temp2_in" # includes the images that I had to use google draw to remove their backgrounds.
# output_dir = "transp_obj_images"
output_dir = "transp_obj_images_final"
# output_dir = "temp_out" # outputs from this folder have been copied to the transp_obj_images folder
file_sep = '/'
# Make a list of files that we need to load
input_path = pathlib.Path(main_dir + file_sep + input_dir)
object_files_list = list(input_path.glob("*.png"))  # objects files list

# Looping through object images
for item in object_files_list:
    # Store path of the image in the variable input_path
    item_input_path = main_dir + file_sep + input_dir + file_sep + item.name
    # Store path of the output image in the variable output_path
    item_name = item.name
    item_output_path = main_dir + file_sep + output_dir + file_sep + item_name
    # Processing the image
    input_img = Image.open(item_input_path)
    # Resize the image
    width, height = input_img.size
    input_img_ratio = width/height
    new_height = 1200
    new_width = math.ceil(input_img_ratio * new_height)
    front_img = input_img.resize((new_width, new_height), resample=Image.NEAREST)

    # remove the background of the image
    front_img = remove(front_img)
    # Convert image to RGBA
    front_img = front_img.convert("RGBA")
    # Create a white image with RGB mode and size 2444 × 1718
    white_img = Image.new("RGB", (2444, 1718), (255, 255, 255))
    white_img = white_img.convert("RGBA")
    # Calculate width to be at the center
    width2 = (white_img.width - front_img.width) // 2
    # Calculate height to be at the center
    height2 = (white_img.height - front_img.height) // 2
    # Pasting img on white_img at (width, height)
    white_img.paste(front_img, (width2, height2), front_img)
    # Saving the image in the given path
    white_img.save(item_output_path)

# Visually inspected final output images (final check to make sure all images are good enough).
# The bad images were removed.



#%%
