############### Working directory (WD) and Saving directory (SD) ###############
#Due to the automatization of this script, you should always use "Source" to run the script

WD<-"C:/Users/Igveg/Desktop/R_Stat/Datos/Excel/Paper RyR/"
SD<-"C:/Users/Igveg/Desktop/R_Stat/Datos/ExcelN/Paper RyR/"
X0<-"Sampling Fluo4" 
#X0 must be a .xlsx file which contain the names (whithout extension) of the files that contained the output 
#of the ROI manager from ImageJ/FIJI that will be normalized. All the files indicated in the X0.xlsx must have:

  #1) the background as the last ROI
  #2) Integrated Density as a parameter to quantify
  #3) the original headers names
  #4) all ROIs in triplicate
  #5) ROIs with the same size

# The X0.xlsx file will also have headers, being the first column the Name of the files, 
# and the second column the sampling values in seconds.

############### Libraries ###############
#Install them with the next line if you have not got them yet
#install.packages(c("ggplot2","writexl","gridExtra","readxl"))

library(ggplot2) 
library(writexl)
library(gridExtra)
library(readxl)

############### Based on MATLAB, this create a function clc() ################
#With this you can clear all the environment, console, and plots

clc <- function() {
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  ll <- ll[ll != "clr"]
  rm(list = ll, envir = ENV)
  cat("\014")
}

############### SCRIPT ###############
setwd(SD) #Set SD as this directory have the X0 file which have the raw data
XI<-paste0(X0,'.xlsx') #Creates the name of the latter file but with the extension .xlsx
DataBase<-as.data.frame(read_xlsx(path=XI))  #Creates a dataframe with the data exported from ImageJ/FIJI's ROI manager
Basal=50 #Set the number of initial frames by which you will normalize the data (which determine F_0)

PC<-NULL #Empty list which will be filled with the number of files with problems in the order or number of columns
PG<-NULL #Empty list which will be filled with the number of files with problems in the normalization

#Loop with the normalization protocol that will apply to all the data mentioned in X0.xlsx (now in data base)

for(y in 1:nrow(DataBase)){
  setwd(WD) #Set WD where the files indicated in X0.xlsx (now data base) are
  Sampling=as.numeric(DataBase[y,2]) #Reading the sampling from the row 'y' in the data base (second column)
  InitialSeconds<-Basal*Sampling #Creates a variable that declares how many seconds from the begining are used to normalize
  Name<-DataBase[y,1] #Create a variable with the name of the file in the row 'y' in X0.xlsx
  X1<-paste0(Name,'.xlsx') #Add the .xlsx extension to the Name variable to generate the X1 path
  
  #Reading the data
  Data<-as.data.frame(read_xlsx(path=X1)) #Creates a dataframe with the data exported from ImageJ/FIJI saved in the .xlsx files
  colnames(Data)[1]<-"X" #Giving the name 'X' to the first column header (ImageJ left it empty initially)
  
  #Generating a list with all the column names with the measurements IntDen
  #This is due to the fact that the headers are Parameter(ROI_Name), and only IntDen is used in the normalization
  IntRaw<-grep("IntDen",colnames(Data),value=TRUE) #Creates a list which include 'IntDen' and 'RawIntDen'
  Raw<-grep("Raw",colnames(Data),value=TRUE) #Creates a list which include only 'RawIntDen'
  IntDen<-setdiff(IntRaw,Raw) #By setting the diference between the two last lists, a IntDen only list is generated
  #This imply that no "Raw"-containg name had to be used in the ROI names, or with "IntDen".
  
  
#Posible additional step in which you can eliminate ROIs which you do not want to include in the normalization
#Example: if some ROIs are lines, the ROI name could include "line", and this will be the "Key_word"
  Exception<-grep("Key_word",colnames(Data),value=TRUE)
  IntDen<-setdiff(IntDen,Exception)

#This print in the console the final columns that should be all the triplicated ROIs but only IntDen parameter
  columns<-c("X",IntDen)
  print(columns)

#In this line the Normalization starts
  Answer <- readline(paste(paste0('Normalize? ',y,'/',nrow(DataBase)),'Write "y" and then press enter for continue','Anything else will skip this file',' ',sep="\n")) 
  #Checkpoint which ask you if normalize, where you must check if the columns are composed only by the first
  #one named "X" and the ROI triplicates, with also the background at the end
  
  #If you answer with a lower-case "y" meaning yes (upper-case "Y" will not be taken), and then press enter key, the 
  #normalization will continue. Any other answer will taken as a negative answer, and the next file will be analized.
  
  if (substr(Answer, 1, 1) == "y"){
      writeLines("Normalizing...")
      writeLines(" ")
    DF<-subset(Data,select=columns) #Creating dataframe DF which is a subset of Data, only with IntDen and the frame column ("X")
      
    number<-ncol(DF) #Creates a variable with the number of columns of DF
    Last3<-(number-2) #Creating a variable which indicate the number of column that correspond to the first background ROI
    BG<-DF[,Last3:number] #Creating a subset of DF which only contain the background ROIs
    Mean <- rowMeans(BG) #Creating a vector with all the average background values of all the rows
    Final<-setdiff(DF,BG)   #Creating dataframe which excludes the background
    Subtraction<-subset(Data,select=X) #Creating preliminar dataframe which only include the frame column "X"
    #This dataframe will be filled in the next loop
      
    #Loop that subtract the background to the other ROIs and save them in the Subtraction dataframe
    for(k in 1:(number-4)){
      MinusBG<-(as.numeric(Final[,(k+1)]) - Mean)
      Subtraction[ , paste0("MinusBG", k)] <- MinusBG
    }
      
    #Creating preliminar dataframe which only include the frame column "X"
    Normalized<-subset(Data,select=X) #This dataframe will be filled in the next loop
      
    #Loop for normalization by the mean of the basal of each IntDen (determined by the Basal variable at the begining)
    for(h in 1:(number-4)){
      j<-as.numeric(mean(Subtraction[1:Basal,(h+1)]))
      Normalization<-(as.numeric(Subtraction[,(h+1)])/j)
      Normalized[ , paste0("Normalization", h)] <- Normalization
    }
    
    #Creating preliminar dataframe which only include the frame column "X"
    ROIs<-subset(Data,select=X) #This dataframe will be filled in the next lopp
    
    n=(number-4)/3 #Creating variable which indicates how many ROIs (not triplicate) are in total
      
    #Loop for make an average from all the triplicates in each frame, which is already background-subtracted and normalized by F0
    for (i in 1:n){
      mean <- rowMeans(Normalized[,(3*i-1):(3*i+1)])
      ROIs[ , paste0("mean", i)] <- mean
    }
      
    Time<-as.numeric(ROIs$X)*Sampling #Creating the variable "Time" in seconds
    DataNormalized<-data.frame(Time,ROIs[,2:(n+1)]) #Creating dataframe with the final F/F0 values and Time (s)
      
    #PLOTS
    names<-colnames(Final, do.NULL = TRUE, prefix = "col") #Get the original names
    names<-gsub("IntDen.","",names,ignore.case=T) #Subtract "IntDen" to the original name
    names<-gsub(".{2}$","",names,ignore.case=T) #Subtract the last two characters (which would be .1 .2 .3 of the triplicate)
    
      #Loop to generate a scatter plot to view the graph of the data and check if all is correct to continue
    #Maybe you realized it is necessary to change the basal
    
      s<-list() #Creates a list in which save the plots
      
      #Loop that plot the data and save the image in the list
      for(g in 1:(n)){
        p<-names[2+3*(g-1)]
        s[[g]]<-(ggplot(DataNormalized, aes_string(Time,DataNormalized[,(g+1)])) + geom_point() + labs(x = "Time (s)", y = "F/F0")+ggtitle((p))+theme(plot.title = element_text(hjust = 0.5)))
      }
      
      grafico<-do.call(grid.arrange, s) #plot a grid.arrange of the plots of all the ROIs in the file
      
      #Loop to generate a list with the name of the plots
      list<-c()
      for(d in 1:(n)){
        l<-names[2+3*(d-1)]
        list[[l]]<-l
      }
      
      colnames(DataNormalized)<-c("Time",list) #Renames the columns
      writeLines(paste0('The first ',floor(InitialSeconds),' seconds are taken to generate the F0'))
      writeLines(" ")
      
      #the latter line informs you how many seconds are taken to calculate F0
      
      ANSWER <- readline(paste(paste0('Save normalized data and plots? ',y,'/',nrow(DataBase)),'Write "y" and then press enter for continue','Anything else will skip this file',' ',sep="\n")) 
        
      #Checkpoint where you decide if save the normalization or not
        if (substr(ANSWER, 1, 1) == "y"){
          writeLines(paste0('Saving plots from file ',y,' of ',nrow(DataBase)))
          setwd(SD) #Sets the saving directory
          write_xlsx(DataNormalized,path=X1) #Save the normalized data in a .xlsx
          names<-colnames(DataNormalized, do.NULL = TRUE, prefix = "col")
          
          p<-list() #Creates a list that will contain the plots
          
          #Loop that saves the plots in the p list
          for(g in 1:(n)){
            k<-names[g+1]
            p[[g]]<-(ggplot(DataNormalized, aes_string(DataNormalized$Time,DataNormalized[,(g+1)])) + geom_point() + labs(x = "Time (s)", y = "F/F0")+ggtitle((k))+theme(plot.title = element_text(hjust = 0.5)))
          }
          
          graficopdf<-do.call(grid.arrange, p) #saving the plots of p list in a grid arrange
          X3<-paste0(Name,'.pdf') #Creating the name of the .pdf summary
          pdf(X3,paper='letter')  #Initiliazing the pdf
          plot(graficopdf) #Plotting
          dev.off() #Finishing the pdf
          
          PG[y]<-1 #Informs a successfull saving
        }
        
      else{
          writeLines("The analysis has not been saved")
          PG[y]<-0}
      dev.off() 
      PC[y]<-1
    }
    else{
      writeLines("Normalization of this file has been cancelled")
      PC[y]<-0
      } 
  }

writeLines(" ")
writeLines("Normalization has ended, generating summary...")

############### SUMMARY  ###############
writeLines(" ")
writeLines(paste('The following file numbers had troubles with the columns: ', paste(unlist(which(0==PC)),collapse=' '),sep=''))
writeLines(paste0('The following file numbers had troubles with the normalization: ', paste(unlist(which(0==PG)),collapse=' '),sep=''))


#Script writed by Ignacio Vega-Vásquez
#Contact: ignaciovega@ug.uchile.cl
