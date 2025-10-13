%% This script is written by Kianoosh Hosseini at NDClab (See https://Kianoosh.info and https://ndclab.com) in May 2022 and updated in september 2023.
% This script will load and use the subset created from THINGS object image dataset. 
% This script reads the objects' files in the object_images folder.

clear % clear matlab workspace
clc % clear matlab command window

%% Loading all the object database images. 
main_dir = '/Users/kihossei/Documents/GitHub/mfe_c_object/materials/PsychopyTask/mfe_c_object/img';
output_dir = '/Users/kihossei/Documents/GitHub/mfe_c_object/materials/PsychopyTask/mfe_c_object';
animals_objectData_location = [main_dir filesep 'object_images' filesep 'animals']; %Location of stored animal images 
objects_objectData_location = [main_dir filesep 'object_images' filesep 'objects']; %Location of stored object images
% Listing all animals images
cd(animals_objectData_location)
data_file_lists = dir('*.png'); % Loads all images with png extension 
data_file_lists = {data_file_lists.name};
data_file_lists = string(data_file_lists); 
allAnimals = data_file_lists';
for i=1:length(allAnimals)
    allAnimals(i) = append('img/object_images/animals/',allAnimals(i)); 
end
% Listing all object images
cd(objects_objectData_location)
data_file_lists = dir('*.png'); % Loads all images with png extension 
data_file_lists = {data_file_lists.name};
data_file_lists = string(data_file_lists); 
allObjects = data_file_lists';
for i=1:length(allObjects)
    allObjects(i) = append('img/object_images/objects/',allObjects(i)); 
end

%% After creating a list of all objects and animals, randomly sample 384 objects. 384 is the number of trials. We also select additional 20 objects as practice trials.
% We will load 394 object images and 394 animal images. 
%  
animals = randsample(allAnimals, 394);
objects = randsample(allObjects, 394);

flanker_animal_images = randsample(animals, 202); % Selecting all animal images for the practice and flanker tasks
flanker_animal_images_saved = flanker_animal_images; % saving the images shown in the flanker for later (See lines 290-295)
surprise_animal_images = ~contains(animals, flanker_animal_images, 'IgnoreCase',true); % Selecting all animal images for the surprise task
surprise_animal_images = animals(surprise_animal_images); % these are the animals that are not selected to be shown during practice and main flanker trials. We need 
% these as foil animals in the surprise memory task.
% As the surprise task is two-alternative forced choice, so we need to randomly select 192 foil animals. 
surprise_animal_images = randsample(surprise_animal_images, 192);
foil_animals = surprise_animal_images;
for rr=1:length(foil_animals) % adding a second column that mentions if this is a new animal (i.e., foil). For new objects, we have "1" as true.
    foil_animals(rr,2) = '1'; % Second Column: new?
    foil_animals(rr,3) = 'animal'; % Third Column: animal or object
end

flanker_object_images = randsample(objects, 202); % Selecting all objects images for the practice and flanker tasks
flanker_object_images_saved = flanker_object_images; % saving the images shown in the flanker for later (See lines 290-295)
surprise_object_images = ~contains(objects, flanker_object_images, 'IgnoreCase',true); % Selecting all object images for the surprise task
surprise_object_images = objects(surprise_object_images); % these are the objects that are not selected to be shown during practice and main flanker trials. We need 
% these as foil objects in the surprise memory task.
% As the surprise task is two-alternative forced choice, so we need to randomly select 192 foil objects. 
surprise_object_images = randsample(surprise_object_images, 192);
foil_objects = surprise_object_images;
for rr=1:length(foil_objects) % adding a second column that mentions if this is a new object (i.e., foil). For new objects, we have "1" as true.
    foil_objects(rr,2) = '1'; % Second Column: new?
    foil_objects(rr,3) = 'object'; % Third Column: animal or object
end

foil_images = [foil_animals; foil_objects];
foil_images = foil_images(randperm(size(foil_images,1)),:); % Shuffle the data randomly by rows.

cd(output_dir) % change directory to save outputs in the output directory
%% A loop that creates 12 CSV files for the blocks.
arrowSize = '[.035, .035]';
first_rightFlanker_location = '[0.0385,0]';
second_rightFlanker_location = '[-0.0385,0]';

rightArrow = 'img/rightArrow.png';
leftArrow = 'img/leftArrow.png';
for jaguar=1:13 % 12 of these files will be for the main blocks. The last one with 20 trials will be the practice block.
    if jaguar==13
        prac_animals = randsample(flanker_animal_images, 10); % select 10 animals.
        prac_objects = randsample(flanker_object_images, 10); % select 10 animals.
        pracDat = [prac_animals; prac_objects]; % merging animal and object images to have equal number of animals and objects in practice.
        pracDat = pracDat(randperm(size(pracDat, 1)), : ); % Shuffle the data randomly by rows.

        prac_animals_intact = prac_animals; % we need this for the 2nd csv file.
        prac_objects_intact = prac_objects; % we need this for the 2nd csv file.

        % we need to randomly select half of the images to have right arrow as the target arrow and the 
        % remaining half will have left arrow as their target.
        rightDir_objects = randsample(pracDat, 20/2); % objects with right arrow as target
        leftDir_objects = ~contains(pracDat, rightDir_objects, 'IgnoreCase',true);
        leftDir_objects = pracDat(leftDir_objects); % contains the remaining objects with left arrow as target
        
        % Randomly selecting half of the right-directed and left-directed arrows to be congruent and the remaining half be incongruent.
        %there is going to be the table that has 6 columns; the 1st column is the target arrow; the 2nd and 3rd are distractor arrows; 4th column is stimNum; 5th column is congrunet?; 6th is the target arrow direction.
        % rightDir_objects has 10 object images.
        rightCong = randsample(rightDir_objects, 20/4); % 5 of these object images will be right arrow and Congruent.
        rightCong_intact = rightCong; % We need this for the next step.
        nav_temp1 = rightCong; % stores the list of objects
        % Let's create right-directed congruent rows.
        for zebra=1:length(rightCong)
            rightCong(zebra,1)= rightArrow;
            rightCong(zebra,2)= rightArrow;
            rightCong(zebra,3)= rightArrow;
            rightCong(zebra,4)= 5; % stimNum
            rightCong(zebra,5)= 1; %congruent?
            rightCong(zebra,6)= 'right'; %target
            rightCong(zebra,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            rightCong(zebra,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            rightCong(zebra,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            
            nav_temp = nav_temp1(zebra); % the object that is going to be shown in this trial
            rightCong(zebra,10)= nav_temp; % Straight_object: The object image displayed in the background
            rightCong(zebra,11)= arrowSize; % Size for the background image
        end
        rightIncong = ~contains(rightDir_objects, rightCong_intact, 'IgnoreCase',true); % finds the remaining 5 object images that are not used above as right-directed and congruent.
        rightIncong = rightDir_objects(rightIncong); % the target arrow will be right and incongruent. 
        nav_temp2 = rightIncong; 
        % Let's create right-directed incongruent rows.
        for zebra2=1:length(rightIncong)
            rightIncong(zebra2,1)= rightArrow;
            rightIncong(zebra2,2)= leftArrow;
            rightIncong(zebra2,3)= leftArrow;
            rightIncong(zebra2,4)= 7; % stimNum
            rightIncong(zebra2,5)= 0; % congruent?
            rightIncong(zebra2,6)= 'right'; %target
            rightIncong(zebra2,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            rightIncong(zebra2,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            rightIncong(zebra2,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp2(zebra2); % the object that is going to be shown in this trial
            rightIncong(zebra2,10)= nav_temp; % Straight_object
            rightIncong(zebra2,11)= arrowSize; % Size for the image
        end

        leftCong = randsample(leftDir_objects, 20/4); % % 5 of these object images will be left arrow and Congruent.
        leftCong_intact = leftCong; % We need this in the next step in order to select remaining 5 object images to be used for leftDir Incong!
        nav_temp3 = leftCong;
        % Let's create left-directed congruent rows.
        for zebra3=1:length(leftCong)
            leftCong(zebra3,1)= leftArrow;
            leftCong(zebra3,2)= leftArrow;
            leftCong(zebra3,3)= leftArrow;
            leftCong(zebra3,4)= 6; % stimNum
            leftCong(zebra3,5)= 1; % congruent?
            leftCong(zebra3,6)= 'left'; %target
            leftCong(zebra3,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            leftCong(zebra3,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            leftCong(zebra3,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp3(zebra3);
            leftCong(zebra3,10)= nav_temp; % Straight_object
            leftCong(zebra3,11)= arrowSize; % Size for the image
        end
        leftIncong = ~contains(leftDir_objects, leftCong_intact, 'IgnoreCase',true);
        leftIncong = leftDir_objects(leftIncong); % the target objects that will be incongruent.
        nav_temp4 = leftIncong;
        % Let's create left-directed incongruent rows.
        for zebra4=1:length(leftIncong)
            leftIncong(zebra4,1)= leftArrow;
            leftIncong(zebra4,2)= rightArrow;
            leftIncong(zebra4,3)= rightArrow;
            leftIncong(zebra4,4)= 8; % stimNum
            leftIncong(zebra4,5)= 0; % congruent?
            leftIncong(zebra4,6)= 'left'; %target
            leftIncong(zebra4,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            leftIncong(zebra4,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            leftIncong(zebra4,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp4(zebra4);
            leftIncong(zebra4,10)= nav_temp; % Straight_object
            leftIncong(zebra4,11)= arrowSize; % Size for the image
        end
        % Creating the main table that contains all we have created above for the 1st CSV file (practice block).
        mainTable = table([rightCong(:,1);rightIncong(:,1);leftCong(:,1);leftIncong(:,1)],[rightCong(:,2);rightIncong(:,2);leftCong(:,2);leftIncong(:,2)],[rightCong(:,3);rightIncong(:,3);leftCong(:,3);leftIncong(:,3)],[rightCong(:,4);rightIncong(:,4);leftCong(:,4);leftIncong(:,4)],[rightCong(:,5);rightIncong(:,5);leftCong(:,5);leftIncong(:,5)],[rightCong(:,6);rightIncong(:,6);leftCong(:,6);leftIncong(:,6)],[rightCong(:,7);rightIncong(:,7);leftCong(:,7);leftIncong(:,7)],[rightCong(:,8);rightIncong(:,8);leftCong(:,8);leftIncong(:,8)],[rightCong(:,9);rightIncong(:,9);leftCong(:,9);leftIncong(:,9)],[rightCong(:,10);rightIncong(:,10);leftCong(:,10);leftIncong(:,10)],[rightCong(:,11);rightIncong(:,11);leftCong(:,11);leftIncong(:,11)]);
        mainTable = table2array(mainTable);
        mainTable = mainTable(randperm(size(mainTable, 1)), : ); % Shuffle the data randomly by rows.
        mainTable = array2table(mainTable);
        mainTable.Properties.VariableNames = {'middleStim','leftStim','rightStim', 'stimNum','congruent','target','locationC','locationR','locationL', 'straightObject','imageSize'};
        fileName = append("flanker_practice_block",".csv");
        writetable(mainTable, fileName)
        % let's update objects and images for the next round of this loop. So, they will not
        % have the 10 objects and 10 animals used in this loop.
        animalsTemp = ~contains(flanker_animal_images, prac_animals_intact, 'IgnoreCase',true);
        flanker_animal_images = flanker_animal_images(animalsTemp);

        objectsTemp = ~contains(flanker_object_images, prac_objects_intact, 'IgnoreCase',true);
        flanker_object_images = flanker_object_images(objectsTemp);
    else
        % The first 12 csv files will be used for the arrow flanker task.
        main_animals = randsample(flanker_animal_images, 16); % select 16 animals.
        main_objects = randsample(flanker_object_images, 16); % select 16 objects.
        mainDat = [main_animals; main_objects]; % merging animal and object images to have equal number of animals and objects in practice.
        mainDat = mainDat(randperm(size(mainDat, 1)), : ); % Shuffle the data randomly by rows.

        main_animals_intact = main_animals; % % we will use this to remove the animals already picked for this block. So, this will allow avoiding showing
        % repeatitive animal images in blocks.
        main_objects_intact = main_objects; % % we will use this to remove the objects already picked for this block. So, this will allow avoiding showing
        % repeatitive object images in blocks.

        % we need to randomly select half of the objects to be right directed and the
        % remaining half will be left-directed.
        rightDir_objects = randsample(mainDat, 32/2); % right-directed objects
        leftDir_objects = ~contains(mainDat, rightDir_objects, 'IgnoreCase',true);
        leftDir_objects = mainDat(leftDir_objects); % contains the remaining 192 objects that will be left-directed.
        
        % Randomly selecting half of the right-directed and left-directed objects to be congruent and the remaining half be incongruent.
        %there is going to be the table that has 6 columns; the 1st column is the target; the 2nd and 3rd are distractor objects; 4th column is stimNum; 5th column is congrunet?; 6th is the target.
        rightCong = randsample(rightDir_objects, 32/4); % Congruent right_dir objects.
        rightCong_intact = rightCong; % We need this for the next step.
        nav_temp1 = rightCong;
        % Let's create right-directed congruent rows.
        for zebra=1:length(rightCong)
            rightCong(zebra,1)= rightArrow;
            rightCong(zebra,2)= rightArrow;
            rightCong(zebra,3)= rightArrow;
            rightCong(zebra,4)= 5; % stimNum
            rightCong(zebra,5)= 1; %congruent?
            rightCong(zebra,6)= 'right'; %target
            rightCong(zebra,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            rightCong(zebra,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            rightCong(zebra,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            % in the lines below, I am creating a column consisting the
            % straight looking objects for the background.
            nav_temp = nav_temp1(zebra);
            rightCong(zebra,10)= nav_temp; % Straight_object
            rightCong(zebra,11)= arrowSize; % Size for the image

        end
        rightIncong = ~contains(rightDir_objects, rightCong_intact, 'IgnoreCase',true);
        rightIncong = rightDir_objects(rightIncong); % Incongruent right_dir objects.
        nav_temp2 = rightIncong; 
        % Let's create right-directed incongruent rows.
        for zebra2=1:length(rightIncong)
            rightIncong(zebra2,1)= rightArrow;
            rightIncong(zebra2,2)= leftArrow;
            rightIncong(zebra2,3)= leftArrow;
            rightIncong(zebra2,4)= 7; % stimNum
            rightIncong(zebra2,5)= 0; % congruent?
            rightIncong(zebra2,6)= 'right'; %target
            rightIncong(zebra2,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            rightIncong(zebra2,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            rightIncong(zebra2,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp2(zebra2);
            rightIncong(zebra2,10)= nav_temp; % Straight_object
            rightIncong(zebra2,11)= arrowSize; % Size for the image
        end

        leftCong = randsample(leftDir_objects, 32/4); % Congruent left_dir objects.
        leftCong_intact = leftCong; % We need this for the next step.
        nav_temp3 = leftCong;
        % Let's create left-directed congruent rows.
        for zebra3=1:length(leftCong)
            leftCong(zebra3,1)= leftArrow;
            leftCong(zebra3,2)= leftArrow;
            leftCong(zebra3,3)= leftArrow;
            leftCong(zebra3,4)= 6; % stimNum
            leftCong(zebra3,5)= 1; % congruent?
            leftCong(zebra3,6)= 'left'; %target
            leftCong(zebra3,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            leftCong(zebra3,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            leftCong(zebra3,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp3(zebra3);
            leftCong(zebra3,10)= nav_temp; % Straight_object
            leftCong(zebra3,11)= arrowSize; % Size for the image
        end
        leftIncong = ~contains(leftDir_objects, leftCong_intact, 'IgnoreCase',true);
        leftIncong = leftDir_objects(leftIncong); % Incongruent left_dir objects.
        nav_temp4 = leftIncong;
        % Let's create left-directed incongruent rows.
        for zebra4=1:length(leftIncong)
            leftIncong(zebra4,1)= leftArrow;
            leftIncong(zebra4,2)= rightArrow;
            leftIncong(zebra4,3)= rightArrow;
            leftIncong(zebra4,4)= 8; % stimNum
            leftIncong(zebra4,5)= 0; % congruent?
            leftIncong(zebra4,6)= 'left'; %target
            leftIncong(zebra4,7)= '[0,0]'; % Center image Location on the screen for the Psychopy
            leftIncong(zebra4,8)= first_rightFlanker_location; % Right image Location on the screen for the Psychopy
            leftIncong(zebra4,9)= second_rightFlanker_location; % Left image Location on the screen for the Psychopy
            nav_temp = nav_temp4(zebra4);
            leftIncong(zebra4,10)= nav_temp; % Straight_object
            leftIncong(zebra4,11)= arrowSize; % Size for the image
        end
        % Creating the main table that contains all we have created above for the 1st CSV file.
        mainTable = table([rightCong(:,1);rightIncong(:,1);leftCong(:,1);leftIncong(:,1)],[rightCong(:,2);rightIncong(:,2);leftCong(:,2);leftIncong(:,2)],[rightCong(:,3);rightIncong(:,3);leftCong(:,3);leftIncong(:,3)],[rightCong(:,4);rightIncong(:,4);leftCong(:,4);leftIncong(:,4)],[rightCong(:,5);rightIncong(:,5);leftCong(:,5);leftIncong(:,5)],[rightCong(:,6);rightIncong(:,6);leftCong(:,6);leftIncong(:,6)],[rightCong(:,7);rightIncong(:,7);leftCong(:,7);leftIncong(:,7)],[rightCong(:,8);rightIncong(:,8);leftCong(:,8);leftIncong(:,8)],[rightCong(:,9);rightIncong(:,9);leftCong(:,9);leftIncong(:,9)],[rightCong(:,10);rightIncong(:,10);leftCong(:,10);leftIncong(:,10)],[rightCong(:,11);rightIncong(:,11);leftCong(:,11);leftIncong(:,11)]);
        mainTable = table2array(mainTable);
        mainTable = mainTable(randperm(size(mainTable, 1)), : ); % Shuffle the data randomly by rows.
        mainTable = array2table(mainTable);
        mainTable.Properties.VariableNames = {'middleStim','leftStim','rightStim', 'stimNum','congruent','target','locationC','locationR','locationL', 'straightObject','imageSize'};
        fileName = append("flanker_main_block_",string(jaguar),".csv");
        writetable(mainTable, fileName)
        % let's update objects for the next round of this loop. So, it will not
        % have the 32 objects used in this loop.
        animalsTemp = ~contains(flanker_animal_images, main_animals_intact, 'IgnoreCase',true);
        flanker_animal_images = flanker_animal_images(animalsTemp);

        objectsTemp = ~contains(flanker_object_images, main_objects_intact, 'IgnoreCase',true);
        flanker_object_images = flanker_object_images(objectsTemp);
    end
end
% "flanker_animal_images_saved" includes all the animals shown in the practice and
% 12 main blocks of the flanker task.
flanker_animal_images_in_surprise = ~contains(flanker_animal_images_saved, prac_animals_intact, 'IgnoreCase',true); % Exclude the trials shown in the practice block.
flanker_animal_images_in_surprise = flanker_animal_images_saved(flanker_animal_images_in_surprise); % will include the list of animals that are shown in the main 12 blocks.

for tt=1:length(flanker_animal_images_in_surprise) % adding a second column that mentions  if this is a new object (i.e., foil). For old animals, we have "0" as true.
    flanker_animal_images_in_surprise(tt,2) = '0'; % new?
    flanker_animal_images_in_surprise(tt,3) = 'animal'; % animal or object?
end

% "flanker_object_images_saved" includes all the objects shown in the practice and
% 12 main blocks of the flanker task.
flanker_object_images_in_surprise = ~contains(flanker_object_images_saved, prac_objects_intact, 'IgnoreCase',true); % Exclude the trials shown in the practice block.
flanker_object_images_in_surprise = flanker_object_images_saved(flanker_object_images_in_surprise); % will include the list of objects that are shown in the main 12 blocks.

for tt=1:length(flanker_animal_images_in_surprise) % adding a second column that mentions  if this is a new object (i.e., foil). For old objects, we have "0" as true.
    flanker_object_images_in_surprise(tt,2) = '0'; % new?
    flanker_object_images_in_surprise(tt,3) = 'object'; % animal or object?
end



% create a column that has the same rows as the number of total trials in
% the flanker task. Then, create a column vector that has right and left.
% Finally, repeat that vector 384/2 in order to have a column that has 384
% rows. Half of them will be right and the other half will be left. After
% all, shuffle them randomly by rows. 
old_object_displayed_side = ["right" ;"left"];
old_object_displayed_side = repmat(old_object_displayed_side, 384/2, 1);
old_object_displayed_side = old_object_displayed_side(randperm(size(old_object_displayed_side, 1)), : ); % Shuffle the data randomly by rows.

% Create a new table with following columns:
    % First Column: old_images_in_surp (will have all the objects shown in the
    % flanker task)
    % Second Column: old image is what (object or animal)
    % Third column: new_images_in_surp (will have all the new (foil) objects)
    % Fourth column: new image is what (object or animal)
    % Fifth column: which_side_old_image_displayed 
old_images_in_surp = [flanker_animal_images_in_surprise; flanker_object_images_in_surprise];
old_images_in_surp = old_images_in_surp(randperm(size(old_images_in_surp,1)),:); % Shuffle the data randomly by rows.

surpriseTable = table(old_images_in_surp(:,1),old_images_in_surp(:,3),foil_images(:,1), foil_images(:,3), old_object_displayed_side(:,1));
surpriseTable = table2array(surpriseTable);
surpriseTable = surpriseTable(randperm(size(surpriseTable, 1)), : ); % Shuffle the data randomly by rows.
surpriseTable = array2table(surpriseTable);
surpriseTable.Properties.VariableNames = {'old_image_in_surp', 'old_image_is_what','new_image_in_surp', 'new_image_is_what','which_side_old_image_displayed'};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nRows_surpList = height(surpriseTable);
% We want to have 8 blocks of trials in the surprise task. So, we need to
% create 8 csv files as mentioned above.

surp_table1 = surpriseTable(1:(nRows_surpList/8),:);
writetable(surp_table1, "orig_surp_table1.csv")

surp_table2 = surpriseTable((nRows_surpList/8)+1:2*(nRows_surpList/8),:);
writetable(surp_table2, "orig_surp_table2.csv")

surp_table3 = surpriseTable((2*(nRows_surpList/8))+1:3*(nRows_surpList/8),:);
writetable(surp_table3, "orig_surp_table3.csv")

surp_table4 = surpriseTable((3*(nRows_surpList/8))+1:4*(nRows_surpList/8),:);
writetable(surp_table4, "orig_surp_table4.csv")

surp_table5 = surpriseTable((4*(nRows_surpList/8))+1:5*(nRows_surpList/8),:);
writetable(surp_table5, "orig_surp_table5.csv")

surp_table6 = surpriseTable((5*(nRows_surpList/8))+1:6*(nRows_surpList/8),:);
writetable(surp_table6, "orig_surp_table6.csv")

surp_table7 = surpriseTable((6*(nRows_surpList/8))+1:7*(nRows_surpList/8),:);
writetable(surp_table7, "orig_surp_table7.csv")

surp_table8 = surpriseTable((7*(nRows_surpList/8))+1:8*(nRows_surpList/8),:);
writetable(surp_table8, "orig_surp_table8.csv")





