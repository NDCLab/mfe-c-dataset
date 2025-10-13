
In the initial version of the face experiment (v1), participants 1 through 22 were presented with image pairs in a randomized order. However, a bug in the 'Code_6' code block in surprise_task routine prevented the intended randomization of the images **within each pair**. This occurred because the code's directory was not correctly set on the data collection computer, resulting in participants 1 through 22 receiving the same fixed set of image pairs. 
Note: The image pairs were still correctly presented in a randomized order.

This issue was rectified in the updated version of the experiment (v2), where the images within each pair were also randomized, ensuring that each participant encountered a unique set of image pairings. MFE-C-Object did not have this issue.


The object images database that we developed can be found under --> tasks/mfe_c_object/img/object_images. We encourage reuse of this Object Images database by other researchers. For reuse, please cite the accompanying manuscript, as well as the GITHUB repository.

