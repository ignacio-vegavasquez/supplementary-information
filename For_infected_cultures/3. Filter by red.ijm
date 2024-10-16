input = getDirectory("Choose Working Directory "); //working directory with the videos to analyze in the input folder, and an output folder 
filename = getFileList(input+"ROI");
print(input);

for (i = 0; i < filename.length; i++) { // Loop through each file in the ROI directory.
	if (endsWith(filename[i], ".zip")) { // Check if the current file is a ZIP file.
	    print(filename[i]);
	    print(i+1+" of "+filename.length+" files");
	    open(input+"RFP/"+replace(filename[i],"1_Channel-B RoiSet.zip",".tif")); // Open the corresponding TIFF file.
	        run("Brightness/Contrast...");
	        waitForUser("Manually adjust the minimum and maximum of the image in order to \n only visualize the regions of interest  (background must be black).");
	        run("Apply LUT");
	        setOption("ScaleConversions", true);
	        run("8-bit");
	        setAutoThreshold("Huang dark");
	        run("Convert to Mask");
	        run("Watershed");
	    }
		waitForUser("Pause in case of binary filters needed"); // Pause for potential user adjustments.
	    open(input + "ROI/"+filename[i]); // Open the current ROI file for analysis.
	    counter=0; // Initialize a counter for ROIs without red signal.
	    print("The following ROIs have to be erased since they do not have red signal");
	    
	    for (r = 0; r < roiManager("count"); r++) { // Loop through all ROIs managed by ImageJ.
	        roiManager("select", r);
	        roiManager("measure");
	        if (getResult("Mean", 0) == 0) { // Check if the mean intensity is zero.
	            print(Roi.getName); // Print the name of the ROI that will be removed.
	            counter=counter+1; // Increment the counter for each ROI lacking signal.
	        }
	        close("Results"); // Close the results window after measurement.
	    }
	    
	    print("ROIs to eliminate="+counter+" and the total ROIs are="+roiManager("count")); // Report on ROIs to eliminate.
	    print("Only "+roiManager("count")-1-counter+" ROIs had both channels"); // Report on remaining ROIs with both channels.
	    waitForUser("All windows will be closed"); // Wait for user confirmation before closing all windows.
	    close("ROI Manager"); // Close the ROI Manager window.
	    close("ROIs"); // Close all opened ROI files.
	    close(); // Close any remaining open images or windows.  
	}