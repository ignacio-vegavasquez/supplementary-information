# supplementary-information
Repository created to upload supplementary information.

October, 2024 Update:

The folder "For Infected Cultures" was added, for complementary R scripts and ImageJ macros that were used to processed calcium signals recorded from primary hippocampal cultures transduced with nuclear and mitochondrial calcium sensors.
The files are listed with numbers following the analysis pipeline used in a paper soon to be published. Briefly:
1. This file is a .ijm ImageJ macro that generates the Roiset.zip and calcium-signals table from videos that have multiple cells of interest.
2. The updated R script take into account that the new data is not in triplicated form, and could be extracted the mean or intden values depending on what is stated. Adapted from the already shared R script.
3. This ImageJ macro was used to open the Roiset from 1. and compare the calcium sensor channel and a "red" channel from a transduced shRNA RFP expressed in the cells with knockdown. The output is a list of the ROIs that does not contain the knockdown and should be erased for further analysis.
4. The updated bleaching correcction R script is used mainly in mitochondrial calcium signal analysis, since several ROIs are detected in contrast of the nuclear calcium sensor. The mean trace of all the ROIs in each video is shown at first, and other modifications are make to make it easier to manage more than 100 ROIs per video.

   

March, 2023 Update:

Currently, this repository only contains two semi-automatized scripts: 
  1) One for  F/F0 calculation, created for use in calcium imaging
  2) One for linear bleaching correction, also created for use in calcium imaging

An example folder is uploaded as a .rar file, and both codes are self-explanatory as
they are sufficiently commented. They are not limited to calcium signaling analysis,
but they were created considerating that data as an input.

Both codes were used in the analysis of data in:

-Lobos, P., Córdova, A., Vega-Vásquez, I., Ramírez, O. A., Adasme, T., Toledo, J., ... & Hidalgo, C. (2021). RyR-mediated Ca2+ release elicited by neuronal activity induces nuclear Ca2+ signals, CREB phosphorylation, and Npas4/RyR2 expression. Proceedings of the National Academy of Sciences, 118(33), e2102265118.

-Gleitze, S., Ramírez, O. A., Vega-Vásquez, I., Yan, J., Lobos, P., Bading, H., ... & Hidalgo, C. (2023). Ryanodine Receptor Mediated Calcium Release Contributes to Ferroptosis Induced in Primary Hippocampal Neurons by GPX4 Inhibition. Antioxidants, 12(3), 705.


If more details are required for the use of any of them, please feel free to contact me:
ignaciovega@uchile.cl
