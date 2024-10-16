//Macro for batch processing of calcium signals
//It uses FIJI distribution plus BAR and ResultsToExcel plugins from the update sites in ImageJ

//setBatchMode(true); // Uncomment to run the macro in batch mode, which speeds up processing by suppressing GUI updates.
wd = getDirectory("Choose Working Directory "); //working directory with the videos to analyze in the input folder, and an output folder 
input = wd + "/Input/"; // Define the input directory for video files.
output = wd + "/Output/"; // Define the output directory for results.
filename = getFileList(input); // Get a list of all files in the input directory.

for (i = 0; i < filename.length; i++) { // Loop through each file in the input directory.
    open(input + filename[i]); // Open the current file.
    title = getTitle(); // Store the title of the opened image for later use.
    Stack.getDimensions(width, height, channels, slices, frames); // Get dimensions of the image stack.

    // Detect ROIs
    run("Z Project...", "projection=Median"); // Create a median projection of the stack to enhance ROI visibility.
    run("Unsharp Mask...", "radius=4 mask=0.70"); // Apply unsharp masking to improve edge definition.

    run("Enhance Contrast", "saturated=0.35"); // Enhance contrast to make features more distinct.
    getMinAndMax(min, max); // Retrieve minimum and maximum pixel values from the current image.
    setMinAndMax((max-min)*0.1+min, max); // Adjust minimum and maximum values for better visualization.

    for(l=0; l<5; l++){ run("Despeckle"); } // Apply despeckling multiple times to reduce noise.

    run("Fire"); // Apply a fire LUT for visualization enhancement.
    setOption("ScaleConversions", true); // Enable scaling conversions for further processing.
    run("8-bit"); // Convert the image to 8-bit format for compatibility with analysis tools.
    setAutoThreshold("Huang dark"); // Automatically set a threshold using Huang's method focused on dark regions.
    run("Convert to Mask"); // Convert thresholded image into a binary mask.
    rename("ROIs"); // Rename the binary mask to "ROIs".
    run("Watershed"); // Apply watershed algorithm to separate touching objects in the binary mask.
    
    run("Analyze Particles...", "size=50-Infinity pixel circularity=0.5-1 show=Overlay add"); 
    // Analyze particles based on defined size and circularity criteria, overlaying results on the image.

    // Drop the first 180 frames due to an artifact in the acquisition
    selectWindow(title); 
    run("Duplicate...", "title=temp duplicate range=180-" + slices); 
    // Duplicate the stack while dropping the first 180 frames due to acquisition artifacts.

    // Normalize the trace using BAR plugin, and the new first 60 frames as basal
    run("Normalize Against F0", "output=F/F0 f0end=60"); 
    // Normalize the trace using BAR plugin with first 60 frames as baseline (F0).
    
    run("Plot Z-axis Profile"); 

    // Choose a time window where you want to check for positive responses
    waitForUser("Check the time window where you want to look up for a thresholded response");
    
    init = 0; 
    final = 0; 
    Dialog.create("Select the time window to evaluate for a positive response");
    
    Dialog.addNumber("Initial:", init); 
    Dialog.addNumber("Final:", final);
    Dialog.show(); 
    init = Dialog.getNumber(); 
    final = Dialog.getNumber(); 

    selectWindow("temp_FdivF0."); 
    run("Duplicate...", "title=Threshold duplicate range=" + init + "-" + final); 
    // Duplicate normalized data within user-defined time window.

    // Project with max intensity and measure the mean intensity for each ROI,
    // as parameter to compare with the threshold
    run("Z Project...", "projection=[Max Intensity]"); 
    rename("Maxs"); 
    run("Set Measurements...", "mean redirect=None decimal=3"); 
    roiManager("Multi Measure"); 
   
	ROIS = roiManager("count"); 
	max = newArray; 
    
	for (j = 0; j < ROIS; j++) { 
        max[j] = getResult("Mean" + j + 1, 0); 
        // Store mean intensity values for each ROI into an array for further analysis.
	}

	// Plot the max parameter created for all the ROIs, and sorted, to choose the best threshold
	Plot.create("Simple Plot", "ROI", "Max F/F0"); 
	Plot.add("circles", max); 
	Plot.show(); 
	
	waitForUser("Check for the desired threshold value");

	threshold = 1.2; 
	Dialog.create("Select a Threshold");
	
	Dialog.addNumber("Threshold:", threshold); 
	Dialog.show(); 
	threshold = Dialog.getNumber(); 

	// Classify and rename the ROIs
	for (k = 0; k < ROIS; k++) { 
        if (max[k] > threshold) { 
            roiManager("select", k); 
            roiManager("rename", "Supra_" + k); 
            // Rename ROIs with mean intensity above threshold as 'Supra_' followed by index number.
        } else {
            roiManager("select", k);
            roiManager("rename", "Sub_" + k);
            // Rename ROIs with mean intensity below or equal to threshold as 'Sub_' followed by index number.
        }
	}
    
	roiManager("sort"); 
	
	// Create BCKG and save results
	selectWindow(title); 
	close("\\Others"); 

	run("Tile"); 
	
	setTool("oval"); 
	
	waitForUser("Select and save a ROI for the background");
	
	roiManager("select", ROIS); 
	roiManager("rename", "BCKG"); 
	roiManager("Save", output + replace(title, ".stk","") + " RoiSet.zip");
	
	close("Results");
	run("Set Measurements...", "area mean min integrated redirect=None decimal=3");
	
	roiManager("deselect");
	roiManager("Multi Measure");
	saveAs("Results", output+"/"+replace(title, ".stk","")+".csv");
	
	waitForUser("The next file will be processed. Click OK to continue");
	run("Close All");
	close("Results");
	close("ROI Manager");
}
print("All files have been processed"); // Notify user that all files have been processed successfully.
IJ.freeMemory(); // Free up memory used by ImageJ after processing is complete.
// Ignacio Vega Vasquez igvegvas@uni-wuerzburg.de
