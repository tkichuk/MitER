"# MitER" 
Supplemental Note One:
The following is a step-by-step guide on how to run the MitER workflow on .obj or .VTK files.
Requirements
•	ParaView (https://www.paraview.org/)
•	Blender (version 3.6 https://download.blender.org/release/Blender3.6/)
•	RStudio (https://posit.co/download/rstudio-desktop/)

Section Cells and Estimate Cell Surface in ParaView
•	Open surface .VTK or .obj files in ParaView. Select apply and then hide all except your renderings of interest.
•	If your renderhas multiple cells in it, select single cells for downstream analysis. 
•	Click Select cells through -> click and drag to define ROI -> copy active selection -> extract selection.
Note: make sure to keep your ROI the same for multiple channels of the same cell.
•	Estimate cell surface by applying a Delaunay 3D filter to the cell surface signal or ER signal respectively.
Cell or ER signal -> Filters -> Alphabetical -> Delaunay 3D -> Apply 
Note: you can add this filter to your favorites for easy access during analysis of many cells
•	Ensuring only the channels you wish to analyze are visible within the frame
File -> Export Scene -> Save as .x3d File

Obtaining Raw Data from Blender 
Important: for the script to run, 3D toolbox needs to be enabled.
Initial Setup:
•	Create a folder on your desktop called MitER and put the MitER scripts here.
•	Open Blender and select the following:
Edit -> Preferences -> Add-ons-> Search and select Mesh: 3D-Print Toolbox
Running the Blender Script:
•	Open Blender
•	Select Scripting from the main toolbar.
•	In the scripting window select the following:
Text -> Open -> Navigate to MitEREngine.py in the folder on your desktop and select it.
•	Once the script is open, click run and select directory (folder) containing the .x3d files you want to analyze.
•	Your output will auto-populate in .csv files within a Data folder within the MitER directory on desktop. 

Analyzing Raw Data in R
•	Open R script from your MitER desktop directory and set the path your data folder
•	Run the script and the resulting collated .csv files will output into your data folder
•	From here you can plot the data as you see fit
![image](https://github.com/tkichuk/MitER/assets/116237022/62bce86d-e72a-4779-b347-5e06e55cd0f4)
