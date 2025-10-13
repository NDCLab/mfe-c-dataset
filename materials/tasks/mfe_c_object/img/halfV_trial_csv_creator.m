%% This script is written by Kianoosh Hosseini at NDClab (See https://Kianoosh.info and https://ndclab.com) in May 2022 and updated in January 2024.
%%% For Object images ...

%This script reads the objects' files in the object_images folder and then loads all
%objects in a single column. 

clear % clear matlab workspace
clc % clear matlab command window

%% Loading all the object images. 
main_dir = '/Users/kihossei/Documents/GitHub/mfe_c_object/materials/PsychopyTask/mfe_c_object/img';
output_dir = '/Users/kihossei/Documents/GitHub/mfe_c_object/materials/PsychopyTask/mfe_c_object';
objData_location = [main_dir filesep 'object_images']; %Location of stored object images
cd(objData_location)
data_file_lists = dir; 
data_file_lists = data_file_lists(~ismember({data_file_lists.name},{'.', '..', '.DS_Store'}));
data_file_lists = {data_file_lists.name};
data_file_lists = string(data_file_lists); 
allObjs_filename_pattern = '.jpg'; % The face file that has this pattern in its name will be loaded.
allObjs = contains(data_file_lists, allObjs_filename_pattern, 'IgnoreCase',true);
allObjs = data_file_lists(allObjs);
allObjs = allObjs';
for lisar1=1:length(allObjs)
    allObjs(lisar1) = append('img/object_images/',allObjs(lisar1)); 
end

%% After creating a list of all objects, randomly sample 384 objects. 384 is the number of trials. We also select additional 20 objects as practice trials.
% Object images database has 788 object images in total. 
% So, we load 404 objects in total.
objects = randsample(allObjs, 404);
trialObjs_for_main = objects; % This stores the list of objects that are going to be shown in practice and main trials. 
surpriseObjs = ~contains(allObjs, objects, 'IgnoreCase',true);
surpriseObjs = allObjs(surpriseObjs); % these are the objects in the object_images folder that are not selected to be shown during practice and main trials. We need 
% these as foil objects in the surprise memory task.
% As the surprise task is two-alternative forced choice, so we need to randomly select 384 foil objects. 
surpriseObjs = randsample(surpriseObjs, 384);
foilObjs = surpriseObjs;
for abrak1=1:length(foilObjs) % adding a second column that mentions if this is a new object (i.e., foil). For new objects, we have "1" as true.
    foilObjs(abrak1,2) = '1'; % new?
end

cd(output_dir) % change directory to save outputs in the output directory
%% A loop that creates 12 CSV files for the blocks.
arrowSize = '[.035, .035]';
first_rightFlanker_location = '[0.0385,0]';
second_rightFlanker_location = '[-0.0385,0]';

rightArrow = 'img/rightArrow.png';
leftArrow = 'img/leftArrow.png';
for jaguar=1:13 % 12 of these files will be for the main blocks. The last one with 20 trials will be the practice block.
    if jaguar==13
        firstDat = randsample(objects, 20); % this practice block will have 20 trials.
        pracObjs_intact = firstDat; % we need this for the 2nd csv file.
        % we need to randomly select half of the objects to have right arrow as the target arrow and the 
        % remaining half will have left arrow as their target.
        rightDir_objs = randsample(firstDat, 20/2); % objects with right arrow as target
        leftDir_objs = ~contains(firstDat, rightDir_objs, 'IgnoreCase',true);
        leftDir_objs = firstDat(leftDir_objs); % contains the remaining objects with left arrow as target
        
        % Randomly selecting half of the right-directed and left-directed arrows to be congruent and the remaining half be incongruent.
        %there is going to be the table that has 6 columns; the 1st column is the target arrow; the 2nd and 3rd are distractor arrows; 4th column is stimNum; 5th column is congrunet?; 6th is the target arrow direction.
        % rightDir_Objs has 10 object images.
        rightCong = randsample(rightDir_objs, 20/4); % 5 of these object images will be right arrow and Congruent.
        rightCong_intact = rightCong; % We need this for the next step.
        nav_temp1 = rightCong; % stores the list of faces
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
            
            nav_temp = nav_temp1(zebra); % the face that is going to be shown in this trial
            rightCong(zebra,10)= nav_temp; % Straight_object: The object image displayed in the background
            rightCong(zebra,11)= arrowSize; % Size for the background image
        end
        rightIncong = ~contains(rightDir_objs, rightCong_intact, 'IgnoreCase',true); % finds the remaining 5 object images that are not used above as right-directed and congruent.
        rightIncong = rightDir_objs(rightIncong); % the target arrow will be right and incongruent. 
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

        leftCong = randsample(leftDir_objs, 20/4); % % 5 of these face images will be left arrow and Congruent.
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
        leftIncong = ~contains(leftDir_objs, leftCong_intact, 'IgnoreCase',true);
        leftIncong = leftDir_objs(leftIncong); % the target objects that will be incongruent.
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
        mainTable.Properties.VariableNames = {'middleStim','leftStim','rightStim', 'stimNum','congruent','target','locationC','locationR','locationL', 'straightObj','imageSize'};
        fileName = append("flanker_practice_block",".csv");
        writetable(mainTable, fileName)
        % let's update objects for the next round of this loop. So, it will not
        % have the 20 objects used in this loop.
        objsTemp = ~contains(objects, pracObjs_intact, 'IgnoreCase',true);
        objects = objects(objsTemp);
    else
        % The first 12 csv files will be used for the arrow flanker task.
        firstDat = randsample(objects, 32); % each block will have 32 trials.
        firstDat_intact = firstDat; % we will use this to remove the objects already picked for this block. So, this will allow avoiding showing
        % repeatitive object images in blocks. 
        % we need to randomly select half of the objects to be right directed and the
        % remaining half will be left-directed.
        rightDir_objs = randsample(firstDat, 32/2); % right-directed objects
        leftDir_objs = ~contains(firstDat, rightDir_objs, 'IgnoreCase',true);
        leftDir_objs = firstDat(leftDir_objs); % contains the remaining 192 objects that will be left-directed.
        
        % Randomly selecting half of the right-directed and left-directed objects to be congruent and the remaining half be incongruent.
        %there is going to be the table that has 6 columns; the 1st column is the target; the 2nd and 3rd are distractor objects; 4th column is stimNum; 5th column is congrunet?; 6th is the target.
        rightCong = randsample(rightDir_objs, 32/4); % Congruent right_dir objects.
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
            % straight looking faces for the background.
            nav_temp = nav_temp1(zebra);
            rightCong(zebra,10)= nav_temp; % Straight_obj
            rightCong(zebra,11)= arrowSize; % Size for the image

        end
        rightIncong = ~contains(rightDir_objs, rightCong_intact, 'IgnoreCase',true);
        rightIncong = rightDir_objs(rightIncong); % Incongruent right_dir objects.
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
            rightIncong(zebra2,10)= nav_temp; % Straight_obj
            rightIncong(zebra2,11)= arrowSize; % Size for the image
        end

        leftCong = randsample(leftDir_objs, 32/4); % Congruent left_dir objects.
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
            leftCong(zebra3,10)= nav_temp; % Straight_obj
            leftCong(zebra3,11)= arrowSize; % Size for the image
        end
        leftIncong = ~contains(leftDir_objs, leftCong_intact, 'IgnoreCase',true);
        leftIncong = leftDir_objs(leftIncong); % Incongruent left_dir objects.
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
            leftIncong(zebra4,10)= nav_temp; % Straight_obj
            leftIncong(zebra4,11)= arrowSize; % Size for the image
        end
        % Creating the main table that contains all we have created above for the 1st CSV file.
        mainTable = table([rightCong(:,1);rightIncong(:,1);leftCong(:,1);leftIncong(:,1)],[rightCong(:,2);rightIncong(:,2);leftCong(:,2);leftIncong(:,2)],[rightCong(:,3);rightIncong(:,3);leftCong(:,3);leftIncong(:,3)],[rightCong(:,4);rightIncong(:,4);leftCong(:,4);leftIncong(:,4)],[rightCong(:,5);rightIncong(:,5);leftCong(:,5);leftIncong(:,5)],[rightCong(:,6);rightIncong(:,6);leftCong(:,6);leftIncong(:,6)],[rightCong(:,7);rightIncong(:,7);leftCong(:,7);leftIncong(:,7)],[rightCong(:,8);rightIncong(:,8);leftCong(:,8);leftIncong(:,8)],[rightCong(:,9);rightIncong(:,9);leftCong(:,9);leftIncong(:,9)],[rightCong(:,10);rightIncong(:,10);leftCong(:,10);leftIncong(:,10)],[rightCong(:,11);rightIncong(:,11);leftCong(:,11);leftIncong(:,11)]);
        mainTable = table2array(mainTable);
        mainTable = mainTable(randperm(size(mainTable, 1)), : ); % Shuffle the data randomly by rows.
        mainTable = array2table(mainTable);
        mainTable.Properties.VariableNames = {'middleStim','leftStim','rightStim', 'stimNum','congruent','target','locationC','locationR','locationL', 'straightObj','imageSize'};
        fileName = append("flanker_main_block_",string(jaguar),".csv");
        writetable(mainTable, fileName)
        % let's update objects for the next round of this loop. So, it will not
        % have the 32 objects used in this loop.
        objsTemp = ~contains(objects, firstDat_intact, 'IgnoreCase',true);
        objects = objects(objsTemp);
    end
end
% "trialObjs_for_main" includes all the faces shown in the practice and
% 12 main blocks of the flanker task.
include_flankerObjs_for_surprise = ~contains(trialObjs_for_main, pracObjs_intact, 'IgnoreCase',true); % Exclude the trials shown in the practice block.
flanker_objects_to_be_in_surprise = trialObjs_for_main(include_flankerObjs_for_surprise); % will include the list of objects that are shown in the main 12 blocks.

for abrak2=1:length(flanker_objects_to_be_in_surprise) % adding a second column that mentions  if this is a new object (i.e., foil). For old objects, we have "0" as true.
    flanker_objects_to_be_in_surprise(abrak2,2) = '0'; % new?
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
    % First Column: old_object_in_surp (will have all the objects shown in the
    % flanker task)
    % Second Column: new_object_in_surp (will have all the new (foil) objects)
    % Third column: which_side_old_object_displayed 
surpriseTable = table(flanker_objects_to_be_in_surprise(:,1),foilObjs(:,1), old_object_displayed_side(:,1));
surpriseTable = table2array(surpriseTable);
surpriseTable = surpriseTable(randperm(size(surpriseTable, 1)), : ); % Shuffle the data randomly by rows.
surpriseTable = array2table(surpriseTable);
surpriseTable.Properties.VariableNames = {'old_object_in_surp', 'new_object_in_surp', 'which_side_old_object_displayed'};

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





