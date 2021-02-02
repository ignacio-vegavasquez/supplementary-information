
############### Working directory (WD) and Saving directory (SD) ###############
#Due to the automatization of this script, you should always use "Source" to run the script
#You only have to fill WD and SD

WD<-"C:/Users/Igveg/Desktop/R_Stat/Datos/Corrección de Bleaching/Originales/Ejemplo"
SD<-"C:/Users/Igveg/Desktop/R_Stat/Datos/Corrección de Bleaching/Corregidos/Ejemplo"

#This script will scan all the files in the folder WD, so make sure to only put a manageble number of
#records to correct, since this script may take a while depending on the number of ROIs of the files.
#The files must be like the output of the Automatized Normalization script (.xlsx and only Time and ROI columns)

############### Libraries ###############
#Install them with the next line if you have not got them yet
#install.packages(c("ggplot2","readxl","gridExtra","writexl","officer","grid","webshot"))

library(ggplot2)
library(readxl)
library(gridExtra)
library(writexl)
library(officer)
library(grid)
library(webshot)

############### Based on MATLAB, this create a function clc() ################

clc <- function() {
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  ll <- ll[ll != "clr"]
  rm(list = ll, envir = ENV)
  dev.off()
  cat("\014")
}

#With this (clc() in the console) you can clear all the environment, console, and plots

############################ SCRIPT #########################
setwd(WD) #Set the directory to WD where the .xlsx files

files<-gsub(".{5}$","",Sys.glob("*.*"),ignore.case=T) #creating a list of the files in the WD without the .xlsx
ROIs<-list() #Create a empty list that will be fill

#Loop for correct the bleaching to all files

for(w in 1:length(files)){
setwd(WD)
Name=files[w] #Creating variable Name with the name of the w file of the list
X0<-paste0(Name,'.xlsx') #Creating the path of the file

#Reading the data
Data<-as.data.frame(readxl::read_xlsx(path=X0)) #Creating dataframe with the file
p<-list() #Empty list to fill later
names<-colnames(Data, do.NULL = TRUE, prefix = "col") #Getting the column names (ROIs)
ROIs[[w]]<-list(names[-1]) #Saving the ROI names as a component of the ROI list

#Loop that plot all the ROIs to have a preview
for(g in 1:(ncol(Data)-1)){
  k<-names[g+1]
  p[[g]]<-(ggplot(Data, aes_string(Data[,1],Data[,(g+1)])) + geom_point() 
           + labs(x = "Time (s)", y = "F/F0")+ggtitle((k))
           +theme(plot.title = element_text(hjust = 0.5))
           +scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
             scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))
}

graficopdf<-do.call(grid.arrange, p) #Plotting the ROIs as a grid arrange

sampling<-Data[1,1] #Getting the sampling time as the first value in the Time column
readline(paste(paste0('File ',w,'/',length(files)," "),'Press any key to continue when the plots finish to load',' ',sep="\n"))
#This is a checkpoint where you wait to the plots to load and see which file is

#Loop to correct one ROI at a time
  for (h in 2:ncol(Data)){
    print(p[h-1]) #Plots the current ROI
    Backup<-Data[,h] #Creates a backup of the Time,F/F0 value for this ROI
    ANSWER1<-readline(paste(paste0('Correct Bleaching? ',h-1,'/',ncol(Data)-1," "),' ',
                            'Write "y" and then press enter for continue',
                            'Anything else will be taken as a negative answer to the question',
                            ' ',sep="\n"))
    #Checkpoint to makes sure if the bleaching must be corrected or not
    
    #Entering a repeat loop to try over and over the parameters if you want to
    if(substr(ANSWER1,1,1) == "y"){
      repeat{
            
        #Checkpoint to see if the time window taken to make the linear regresion starts or not at the begining
        writeLines(" ")
        ANSWER2 <- readline(paste('The representative trace starts at the begining? ', ' ',
                                  'Write "y" and then press enter for continue',
                                  'Anything else will be taken as a negative answer to the question',
                                  ' ',sep="\n"))
        
        #If starts at the begining, only tf is necessary to inform
        writeLines(" ")
        if(substr(ANSWER2, 1, 1) == "y"){
          ti<-sampling; writeLines('At what time the representative trace ends?')  
          tf<-scan(what=numeric(),nmax=1,quiet=TRUE)} 
        
        #It ask for both initial and final time of the representative trace
        else{writeLines('At what time the representative trace starts?')
          ti<-scan(what=numeric(),nmax=1,quiet=TRUE); writeLines('At what time the representative trace ends?')
          tf<-scan(what=numeric(),nmax=1,quiet=TRUE)} 

        writeLines(" ")
        writeLines(paste0('Doing the linear regression ',h-1,' of ',ncol(Data)-1,'...')) #Starts the regression
        i=floor(ti/sampling) #Creates a variable which is the first frame of the representative trace
        f=floor(tf/sampling) #Creates a variable which is the last frame of the representative trace
        
        Reg<-Data[i:f,c(1,h)] #Creates a subset of the data which is taken to the linear regression
        plot(Reg) #Plots the subset
        LR<-lm(Reg[,2]~Reg[,1]) #Do the linear regression
        #plot(Reg) #Plot the subset again
        abline(LR) #Plot the linear regression on the subset plot
        
        #Checkpoint to see if the regression is good to go or it is needed to try other parameters
        writeLines(" ")
        ANSWER3 <- readline(paste(paste0('Is it a good adjust to the curve? ',h-1,'/',ncol(Data)-1," "),' ',
                                  'Write "y" and then press enter for continue',
                                  'Anything else will be taken as a negative answer to the question',
                                  ' ',sep="\n"))
        
        #If it is correct, it will do the correction and plot the result
        writeLines(" ")
        if (substr(ANSWER3, 1, 1) == "y"){
          writeLines(paste0('Correcting bleaching ',h-1,' of ',ncol(Data)-1,'...')) 
          Temp<-Data[,h]+Data[,1]*(-coef(LR)[2]) #Temporal correction saved as Temp
          
          pp<-list() #Empty list to fill with the original and corrected plot
          
          #original plot
          pp[[1]]<-(ggplot(Data, aes_string(Data[,1],Data[,h]))
                   + geom_point() 
                   + labs(x = "Time (s)", y = "F/F0")
                   + ggtitle("Original") 
                   + theme(plot.title = element_text(hjust = 0.5))
                   +scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 
                   +scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))
          
          #Corrected plot
          pp[[2]]<-(ggplot(Data, aes_string(Data[,1],Temp)) 
                   + geom_point() 
                   + labs(x = "Time (s)", y = "F/F0")
                   + ggtitle("Corrected") 
                   + theme(plot.title = element_text(hjust = 0.5))
                   + scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 
                   + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)))
          
          grafico<-do.call(grid.arrange, pp) #Plots both original and corrected in a grid arrange to be compared
          
          #Checkpoint to evaluate the correction
          writeLines(" ")
          ANSWER4 <- readline(paste(paste0('Is it a successfull correction? ',h-1,'/',ncol(Data)-1," "),' ',
                              'Write "y" and then press enter for continue',
                              'Anything else will be taken as a negative answer to the question',
                              ' ',sep="\n"))
          
          writeLines(" ")
          if (substr(ANSWER4, 1, 1) == "y"){
            writeLines(paste0('Saving the correction ',h-1,' of ',ncol(Data)-1))
            Data[,h]<-Temp #Guardando solo si fue una buena correccion
            }
          
          else{writeLines("Failed correction")}}
        
            else{writeLines("Bad adjust")}
        
#This allowed you to try again another pair of ti and tf
        writeLines(" ")
        ANSWER5<-readline(paste('Do you want to try another time window? ',' ',
                  'Write "y" and then press enter for continue',
                  'Anything else will be taken as a negative answer to the question',
                  ' ',sep="\n"))
writeLines(" ")
if(substr(ANSWER5,1,1)=="y"){
  Data[,h]<-Backup} #This will start over again the loop

else{
  writeLines('Next ROI...') #Anything else will break the loop and the next ROI will be taken
  break}}}
    
    else{writeLines("Bleaching not corrected")}}
  
  #Checkpoint to see if you want to save the correction or not
  writeLines(" ")
  ANSWER6 <- readline(paste(paste0('Save the correction in an excel file? ',w,'/',length(files)," "),' ',
                      'Write "y" and then press enter for continue',
                      'Anything else will be taken as a negative answer to the question',
                      ' ',sep="\n"))
  
  writeLines(" ")  
  if(substr(ANSWER6, 1, 1) == "y"){
      setwd(SD)  
      X1<-paste0(Name,' BLEACHING CORREGIDO','.xlsx') #Saving the correction
      write_xlsx(Data,path=X1)
      writeLines("Bleaching correction saved")}
      
      else{print("Bleaching correction not saved")}}


#Script writed by Ignacio Vega-Vásquez
#Contact: ignaciovega@ug.uchile.cl
