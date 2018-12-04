require(data.table)
library(data.table)
require(GGally)
library(GGally)
require(lubridate)
library(lubridate)
require(rpart)
library(rpart)
require(stringr)
library(stringr)
#**
#
# Function Name:
#   changeDTaddAC <- [change] [D]ata [T]ype and [add] [A]uxiliar [C]olumns
# Arguments:
#   (data.frame) dFrame
# Objective:
#   Cast the columns and add auxiliar columns with values easier to handle. 
# Return:
#   (data.frame) dFrame
# Author:
#   Javier
# Last Modification:
#   Nov 13, 2018
#
#**
changeDTaddAC <- function(dFrame) { 
  #Get the hour (hh:mm:ss) from PlannedTime as string
  dFrame$PTimeStr <- str_remove(dFrame$PlannedTime, "[0-9]{4}-[0-9]{2}-[0-9]{2} ")
  #Get the hour (hh:mm) from PTimeStr as string
  dFrame$PTimeStr <- str_remove(dFrame$PTimeStr, ":[0-9]{2}$")
  #Cast PTimeStr to a hm with lubridate
  dFrame[,PlanTime := hm(PTimeStr)]
  #Change TrafficDate "yyyy-mm-dd" to weekday, weekday start from Sunday(1) to Saturday(7)
  dFrame[,Weekday := wday(TrafficDate)]
  return(dFrame)
}
#**
#
# Function Name:
#   wfDsCur20m <- [w]ide [f]ormat [D]ata[s]et [Act]ual and [Aft]er [20] [m]inutes
# Arguments:
#   (data.frame) dFrame
#   (Character) name
# Objective:
#   Change the dFrame to wide format, observation with actual and 20 min after.
# Return:
#   NA
# Author:
#   Javier
# Last Modification:
#   Dec 2, 2018
#
#**
wfDsActAft20m <- function(dFrame, name) {
	
	col_names = TRUE
	aux <- data.frame(
	  TrafficDate = c(""),
		Trainnumber = c(""),
		Trainseries = c(""),
		TrainCharacteristic = c(""),
		Weekday = c(""),
		Location = c(""),
		Activity = c(""),
		PlannedTime = c(""),
		Delay_min = c(""),
		DelayJump = c(""),
		Location_20 = c(""),
		Activity_20 = c(""),
		PlannedTime_20 = c(""),
		Delay_min_20 = c(""),
		DelayJump_20 = c("")
	)
	
	numObs <- 0
	numFile <- 1
	ext <- ".csv"
	fname <- paste(c(name, toString(numFile), ext), collapse = "")
	
	n <- nrow(dFrame) #number of observation
	
	i <- 1 #control variable
	j <- i + 1 #control variable
	#actual information 
	while(i <= n) { #read until last observation
	  
	  #read the i-st observation
		iSub <- dFrame[i]
		#get train number from iSub
		iTrain <- iSub$Trainnumber
		#get planned time for iSub and add 20 minutes
		iTime20 <- dFrame$PlanTime[i]  + minutes(20)
		
		#information after 20 minutes
		while(j <= n) {#read until last obsevation
		  #read j-st observation 
			jSub <- dFrame[j]
			#get train number from jSub
			jTrain <- jSub$Trainnumber
			#get planned time for jSub
			jTime <- dFrame$PlanTime[j]
			
			#check if the information is for the same subject
			if(iTrain == jTrain) {
			  
			  #check if jTime is at least 20 minutes after iTime
				if(iTime20 < jTime) {
					
					#aux data frame
					#add the actual information iSub
				  aux$TrafficDate <- iSub$TrafficDate
					aux$Trainnumber <- iTrain
					aux$Trainseries <- iSub$Trainseries
					aux$TrainCharacteristic <- iSub$TrainCharacteristic
					aux$Weekday <- iSub$Weekday
					aux$Location <- iSub$Location
					aux$Activity <- iSub$Activity
					aux$PlannedTime <- iSub$PTimeStr
					aux$Delay_min <- iSub$Delay_min
					aux$DelayJump <- iSub$DelayJump
					
				  
				  #Cause jTime is bigger than iTime+20minutes
				  #check if the station before is closser to iTime+20
					#the closer one to iTime will be the information for 20 min after
				  
				  #read (j-1)-st observation 
					pSub <- dFrame[j-1]
					#get planned time for pSub
					pTime <- dFrame$PlanTime[j-1]
					
					#get the difference between each pTime and jTime, and iTime 
					#use absolute to work with postitive numbers
					pSDif <- abs(iTime20 - pTime) #[pS]ub [Dif]ference
				  jSDif <- abs(jTime - iTime20) #[jS]ub [Dif]ference
					
					
					if(pSDif < jSDif) {#pSub is closer to iSub than jSub
					  
					  #add information for 20 min after pSub
					  aux$Location_20 = pSub$Location
						aux$Activity_20 = pSub$Activity
						aux$PlannedTime_20 = pSub$PTimeStr
						aux$Delay_min_20 = pSub$Delay_min
						aux$DelayJump_20 = pSub$DelayJump
						
					} else {#jSub is closer to iSub than pSub
						
						#add information for 20 min after jSub
						aux$Location_20 = jSub$Location
						aux$Activity_20 = jSub$Activity
						aux$PlannedTime_20 = jSub$PTimeStr
						aux$Delay_min_20 = jSub$Delay_min
						aux$DelayJump_20 = jSub$DelayJump
						
					}
					write.table(aux, 
					            file = fname, 
					            sep = ",", row.names = FALSE, 
					            col.names = col_names, 
					            append = !col_names)
					if(col_names) {
						col_names <- !col_names
					}
					numObs <- numObs + 1
					if(numObs == 100000) {
					  numObs <- 0
					  numFile <- numFile + 1
					  fname <- paste(c(name, toString(numFile), ext), collapse = "")
					  col_names <- !col_names
					}
					
					break
				}
			} else { #iSub and jSub are observation for different trains
			  i <- j - 1 #it wil be equal to j after increment
			  break
			}
			j <- j + 1
		}
		i <- i + 1
	}
}
realisation = fread(file = "OctRealisationData.csv", header = TRUE, colClasses = c("Date", "character", "character", "character", "character", "character", "Date", "Date", "integer", "integer", "character" ))
nrow(realisation)
Sys.time()
realisation <- changeDTaddAC(realisation)
Sys.time()
#**
#*
# This code call the function that generates wide format dataset
# ---------------------------------------------
# |                 Warning                   |
# |     This function takes a lot of time     |
# |     N° Observation    Time (minutes)      |
# |          100000           13.1399         |
# |          500000          128.75178        |
# |         1000000          478.9899         |
# ---------------------------------------------
#*
#**
s <- Sys.time()
s
wfDsActAft20m(realisation, "OctRealisationWF")
e <- Sys.time()
e
e - s