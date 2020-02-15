  
Rasch_Function <- function(filename){
  
  if (tryCatch(require("eRm")) == FALSE){
    install.packages("eRm")
  }
  library(eRm)
  
  if (tryCatch(require("ltm")) == FALSE){ #Where do we need this?
    install.packages("ltm")
  }
  library(ltm)
  
  if (tryCatch(require("difR")) == FALSE){ #Where do we need this?
    install.packages("difR")
  }
  library(difR)
  
  if (tryCatch(require("CTT")) == FALSE){
    install.packages("CTT")
  }
  library(CTT)
  
  if (tryCatch(require("WrightMap")) == FALSE){
    install.packages("WrightMap")
    library(WrightMap)
  }
  
  #rawData <- read.csv("~/Documents/BYU Center for Languages/Practice.csv", na.strings = c("", "NA"))
  rawData <- read.csv(filename, na.strings = c("", "NA"))
  
  # THIS SECTION CUTS THE DATA TO FIT THE ERM FUNCTIONS
      # SOME DATA STARTS AT COLUMN 2, SOME AT COLUMN 3; THIS DETERMINES WHICH IS TRUE
      intStart <- FALSE
      firstCol <- 1
      while(intStart == FALSE){
        if(class(rawData[,firstCol]) != "integer"){
          firstCol <- firstCol + 1
        } else{
          intStart = TRUE
        }
        
      }
  
      # REMOVE COLUMNS AND ROWS THAT ARE ALL NA
      myData <- rawData[, colSums(is.na(rawData)) != nrow(rawData)]
      myData <- myData[rowSums(is.na(myData)) != ncol(myData), ]        
      
      #KEEP TRACK OF STUDENTS WHO HAVE ENOUGH DATA TO BE PROCESSED
      studentData <- myData[2:nrow(myData), 1:2]
      
      # CONVERT TRUE FALSE TO 1'S AND 0'S; ALL THAT'S LEFT ARE THE RESPONSES
      key <- rawData[1,firstCol:ncol(rawData)]
      dat_1 <- as.data.frame(t(apply(myData[2:nrow(myData), firstCol:ncol(myData)], 1, function(x) as.numeric(x == key))))
      
      # CALCULATE THE STUDENTS SCORES
      studentScores <- as.vector(rowSums(dat_1, na.rm = TRUE)) # this is hard because we don't know how many questions they answered
      studentData$"Student_Means" <- rowMeans(dat_1, na.rm = TRUE)
      if(sum(is.na(studentData$Student_Means)) > 0){
        studentData$Student_Means[is.na(studentData$Student_Means)] <- 0 # IF THEY HAD ALL NA'S, CONSIDER THEIR SCORE TO BE 0
      }
      
      # KEEP TRACK OF THE OUTLIERS
      students100 <- studentData[studentData$Student_Means == 1,]
      students0 <-studentData[studentData$Student_Means == 0,] 
      
      # REMOVE STUDENTS THAT SCORED 100% (THE ERM FUNCTION MIGHT DO THIS, BUT WE NEED TO KEEP TRACK OF THE STUDENTS NAME AND EMAIL)
      dat_1 <- dat_1[rowMeans(dat_1, na.rm = TRUE) != 1, ]
      studentData <- studentData[studentData$Student_Means != 1,]
      
      # REMOVE THE STUDENTS THAT SCORED 0% OR NA
      dat_1 <- dat_1[rowSums(dat_1, na.rm = TRUE) != 0,]
      studentData <- studentData[studentData$Student_Means != 0,]
      
      # ERM DOESN'T HANDLE ITEMS WHOSE RESPONSES ARE ALL 1'S, ALL 0'S, OR ALL NA'S
      # REMOVE THESE ITEMS; KEEP TRACK OF WHICH ONES ARE LEFT
      itemNames <- colnames(myData[, firstCol:ncol(myData)])
      itemNumber <- c(1:length(itemNames))
      itemScores <-colMeans(dat_1, na.rm = TRUE)
      items <- as.data.frame(cbind(itemNames, itemNumber, itemScores))
      
      if(sum(is.na(items$itemScores)) > 0){
        items$itemScores[is.na(items$itemScores)] <- 0
      }
      
      zeroQuestions <- items[items$itemScores == 0,]
      hundredQuestions <- items[items$itemScores == 1,]
      
      items <- items[items$itemScores != 0,]
      items <- items[items$itemScores != 1,]
      
      dat_1 <- dat_1[, (colMeans(dat_1, na.rm = TRUE) != 1)]
      dat_1 <- dat_1[, (colMeans(dat_1, na.rm = TRUE) != 0)]
    
  
  # RASCH MODEL FUNCTION - RETURNS LIST OF 14 ITEMS
  res_rm_1 <- RM(dat_1)
  
  
      # THIS IS AN "ability parameter for every person... ML estimation does not allow an estimate for persons who solved none or all items"
      pp_ml_1 <- person.parameter(res_rm_1)
      
      # THIS GIVES US A DATA TABLE FOR EACH STUDENT(PERSON_PARAMETER, NAgroup,  INTERPOLATED)
      person <- as.data.frame(pp_ml_1$theta.table) 
  
      ### ADDS A COLUMN OF JUST NUMBERS (TO SHOW WHICH PERSON IS WHICH) and orders rows based on missing pattern group
      person[,(ncol(person)+1)] = seq.int(from = 1, to = nrow(person), by=1)
      person <- person[order(person$NAgroup),] 
      
      ### TAKES SE.THETA (STANDARD ERRORS OF THE PERSON PARAMETERS)
      se.vector = unlist(pp_ml_1$se.theta[1])
      for (k in 2:length(pp_ml_1$se.theta)) { 
        se.vector = append(se.vector,unlist(pp_ml_1$se.theta[k]))  
      } # if there is more than one missing pattern
      
      ## ABOVE, WE TAKE THE FIRST OBJECT OF THE LIST - THEN WE ITERATE THROUGH AND ADD EACH ADDITIONAL ITEM
      ## WHY NOT JUST SAY se.vector <- unlist(pp_ml_1$se.theta) ?  I am not sure. They yield the same result.
      
      # COMBINES THE STANDARD ERROR WITH THE PERSON DATA
      pSE <- as.data.frame(se.vector)
      temp_1 <- cbind(person,pSE)
      temp_2 <- temp_1[order(temp_1$V4),] # use the noted order to arrange the rows
            
      ### EVERYTHING ABOVE COMBINES WITH THE RAWDATA PULLED IN TO MAKE THE DATA TABLE DESCRIBED BELOW; IT THEN OUTPUTS A CSV
      pParam <- cbind(studentData[, 1:2], temp_2[, c(1,5)])
      pParam[1,5] <- SepRel(pp_ml_1)$sep.rel 
      colnames(pParam) <- c("Student Name", "Student Email", 
                            "Person Ability Estimate", "Standard Deviation", "Separation Reliability")
      # HERE WE COULD DO SOMETHING WITH THE OTHER STUDENTS THAT SCORED 100 OR 0 OR NA
  
      write.csv(pParam,file = "PersonEstimates.csv",row.names = F) # write person estimates to csv
      
  
  
  # ITEM FIT SECTION 
  iFit <- itemfit(pp_ml_1)              # summary of item fit statistics
  etas <- as.data.frame(-coef(res_rm_1))     # Item difficulty parameters
  row.names(etas) <- items$itemNames
  iParam <- cbind(etas,iFit$i.outfitMSQ, iFit$i.infitMSQ, iFit$i.outfitZ, iFit$i.infitZ)
  colnames(iParam) <- c("difficulty","outfitMSQ","infitMSQ","outfitZ", "infitZ")
  
  write.csv(iParam,file = "itemStatistics.csv",row.names = T) # write item statistics to csv
  
  
  
  
  
  
  # WRIGHT MAP SECTION
  #Wright Map
  newItemNames <- as.character(items$itemNames)
  
  nLevel1 = length(newItemNames[(sapply(strsplit(newItemNames, "\\."), function(x) x[3])) == "1"])
  nLevel2 = length(newItemNames[(sapply(strsplit(newItemNames, "\\."), function(x) x[3])) == "2"])
  nLevel3 = length(newItemNames[(sapply(strsplit(newItemNames, "\\."), function(x) x[3])) == "3"])
  
  itemcolor <- matrix(nrow=ncol(dat_1),ncol = 1)
  itemcolor[1:nLevel1,1]="black"
  if (nLevel1<ncol(dat_1)) {
    itemcolor[(nLevel1+1):(nLevel1+nLevel2),1]="red"
  }
  if ((nLevel1+nLevel2)<ncol(dat_1)) {
    itemcolor[(nLevel1+nLevel2+1):(nLevel1+nLevel2+nLevel3),1]="blue"
  }
  wrightMap(pp_ml_1$theta.table$`Person Parameter`, etas,show.thr.lab = FALSE,
            thr.sym.pch = 16,thr.sym.cex = 2, thr.sym.col.fg=itemcolor,label.items.srt=90)
  dev.copy(png,"wright_map.png",width=8,height=6,units="in",res=100)
  dev.off()
  
  
  
  
  # Distractor analysis
  
  colnames(dat_1) <- newItemNames;
  dat_1[is.na(dat_1)] <- "x" # code unanswered items to be x
  distractorAnalysis(items = dat_1, key = c(rep(1,ncol(dat_1))), pTable = FALSE, csvReport = "distractor.csv")
  
} # END RASCH FUNCTION

filename <- "~/Documents/BYU Center for Languages/English-L-BAT2018.csv"
#filename <- "~/Documents/BYU Center for Languages/Practice.csv"
Rasch_Function(filename)








