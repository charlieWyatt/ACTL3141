# ACTL3141 Assignment

library(dplyr)
library(data.table) # for setDT
library("lifecontingencies") # used for calculating life expectancies
library('ggplot2')
library('reshape2') # for melt
library("demography") # for graduation
library(splines) # for splines
library("matrixStats") # for rowVars
library('fitdistrplus') # for fitting mortality > 90


setwd("C:/Users/Charlie/Documents/University/year3/T1/actl3141/Assignment")

data <- read.csv("deathspopsimdengland20012018.csv")

                          # Task 1
ESP <- c(
  1000, # < 1
  4000, # 1- 4
  5500, # 5 - 9
  5500, # etc
  5500,
  6000,
  6000,
  6500,
  7000,
  7000,
  7000,
  7000,
  6500,
  6000,
  5500,
  5000,
  4000,
  2500,
  1500,
  1000 # 90+
)


# directly standardised mortality rate
data <- data %>%
  mutate(mx = as.double(deaths/population)) %>%
  mutate(qx = as.double(1-exp(-mx))) # ASSUMING CONSTANT FORCE OF MORTALITY BETWEEN INTEGER AGES-


# Ratio of men to women
  femData <- data %>%
    filter(sex == 'female')
  
  maleData <- data %>%
    filter(sex == 'male')
  
  
  sexRatio <- sum(femData$population)/sum(data$population)
  


regList <- list() # breaks data up into registration lists

numRegoYears <- length(unique(data$Registration.year))

for(j in 1:numRegoYears) {
  regList[[j]] <- data %>%
    filter(Registration.year == 2000+j)
}

# For females-
LtFem <- list() # holds LT a list of all decile of a particular rego year and female
LtFemList <- list() # holds all female rego years broken into deciles

for(j in 1:length(unique(data$Registration.year))) {
  for(i in 1:length(unique(data$IMD.Decile))) {
    LtFem[[i]] <- regList[[j]] %>%
      filter(IMD.Decile == i, sex == 'female')
  }
  LtFemList[[j]] <- LtFem
}


fittedDec <- c(rep(0,10))
par(mfrow = c(2, 5))
for(i in 1:10) {
  # Attempting to fit an exponential distribution to approximate higher mx values
  mortModel8090 <- LtFemList[[1]][[i]]
  mortModel8090$age <- as.numeric(levels(mortModel8090$age))[mortModel8090$age] 
  mortModel8090 <- mortModel8090 %>%
    filter(age >= 80)
  qxModel8090 <- mortModel8090$qx
  
  # trying to fit exponential
  expFit <- fitdistr(qxModel8090, "exponential")
  ksexpFit <- ks.test(qxModel8090, "pexp", expFit$estimate)
  
  if(ksexpFit$p.value > 0.05) {
    fittedDec[i] <- ksexpFit$p.value
  }
  
  # trying to fit linear line
  linFit <- lm(qx ~ age, data = mortModel8090)
  plot(x = 80:89, y = mortModel8090$qx, ylab = "q_x", xlab = "ages", main = paste0("Decile ", i))
  abline(as.numeric(linFit$coefficients[1]), as.numeric(linFit$coefficients[2]), col = "red")
}
par(mfrow= c(1,1))

# Adding each projected years to the data
proj90Plus <- function(sexDF) {
  for(i in 1:18) {
    for(j in 1:10) {
      baseData <- sexDF[[i]][[j]]
      baseData$age = as.numeric(as.character(baseData$age))
      # calculating linear model
      mortModel8090 <- baseData %>%
        filter(age >= 80)
      linFit <- lm(qx ~ age, data = mortModel8090)
      yint <- as.numeric(linFit$coefficients[1])
      slope <- as.numeric(linFit$coefficients[2])
      
      baseData[nrow(baseData), 3] = 90
      baseData[nrow(baseData), 5] = 0 # not actually zero just dont need to calculate
      baseData[nrow(baseData), 6] = 0
      baseData[nrow(baseData), 7] = 0
      baseData[nrow(baseData), 8] = yint + 90*slope
      
      for(k in 91:110) {
        baseData <- rbind(baseData, baseData[nrow(baseData),])
        baseData[nrow(baseData), 3] = k
        
        baseData[nrow(baseData), 5] = 0 # not actually zero just dont need to calculate
        baseData[nrow(baseData), 6] = 0
        baseData[nrow(baseData), 7] = 0
        if(k == 110) {
          baseData[nrow(baseData), 8] = 1 # All die at year 110
        } else {
          baseData[nrow(baseData), 8] = yint + k*slope
        }
      }
      sexDF[[i]][[j]] <- baseData
    }
  }
  return(sexDF)
}

LtFemList <- proj90Plus(LtFemList)





# For Males-
LtMale <- list() # holds LT a list of all decile of a particular rego year and Male
LtMaleList <- list() # holds all male rego years broken into deciles

for(j in 1:length(unique(data$Registration.year))) {
  for(i in 1:length(unique(data$IMD.Decile))) {
    LtMale[[i]] <- regList[[j]] %>%
      filter(IMD.Decile == i, sex == 'male')
  }
  LtMaleList[[j]] <- LtMale
}

LtMaleList <- proj90Plus(LtMaleList)

# in LtFemList[[i]][[j]] this represents the data structure at dec j and rego year i

# How to plot lifetables
# plot(LtFemList[[5]][[2]], type = "l")

# could plot all males together with the best in green and the worst in red
# then do the same with females


deciles <- 1:length(unique(data$IMD.Decile))



# HAVENT DEALT WITH THE 90+ ISSUE IN THIS!!! MAY HAVE TO FORECAST THE FUTURE
# EXPECTED AGES FIRST

lifeExpecMale <- as.data.frame(deciles)
lifeMale <- list()

for(i in 1:length(unique(data$Registration.year))) {
  varName <- paste0(2000+i)
  lifeExpecMale <- lifeExpecMale %>%
    mutate(!!varName := 0)
  lifeMale[[i]] <- list()
  for(j in 1:length(unique(data$IMD.Decile))) {
    
    # make as lifetable datatype
    lifeMale[[i]][[j]] <- probs2lifetable(probs = LtMaleList[[i]][[j]]$qx, 
                                   type ="qx", radix = 100000, 
                                   name = paste0("Male. dec: ", i, ". Rego: ", j))
    
                                   
    lifeExpecMale[j,i+1] <- exn(lifeMale[[i]][[j]], x = 0, type = "complete")
  } # i + 1 to skip decile column 
}

# uncomment this if you want to remove first column
# row.names(lifeExpecMale) <- lifeExpecMale[[1]]
# lifeExpecMale <- lifeExpecMale[,-1]

lifeExpecFem <- as.data.frame(deciles)
lifeFem <- list()

for(i in 1:length(unique(data$Registration.year))) {
  varName <- paste0(2000+i)
  lifeExpecFem <- lifeExpecFem %>%
    mutate(!!varName := 0)
  lifeFem[[i]] <- list()
  for(j in 1:length(unique(data$IMD.Decile))) {
    
    # make as lifetable datatype
    lifeFem[[i]][[j]] <- probs2lifetable(probs = LtFemList[[i]][[j]]$qx, 
                                          type ="qx", radix = 100000, 
                                          name = paste0("Female dec: ", i, ". Rego: ", j))
    
    
    
    lifeExpecFem[j,i+1] <- exn(lifeFem[[i]][[j]], x = 0, type = "complete")
  } # i + 1 to skip decile column 
}

### Plotting changes in life expectancy through time
meanYrlyImprovFem = colMeans(lifeExpecFem[,-1])
meanYrlyImprovMale = colMeans(lifeExpecMale[,-1])

fmRatio <- c(rep(0, 18)) # calculates ratio of women to men each year
numYrFem <- c(rep(0, 18))
numYrMale <- c(rep(0, 18))
for(i in 1:18) {
  for(j in 1:10) {
    numYrFem[i] <- numYrFem[i] + sum(LtFemList[[i]][[j]]$population)
    numYrMale[i] <- numYrMale[i] + sum(LtMaleList[[i]][[j]]$population)
  }
  fmRatio[i] <- numYrFem[i]/numYrMale[i]
}

meanYrlyImprovPop = fmRatio/(1+fmRatio)*meanYrlyImprovFem + # female contribution
  1/(1+fmRatio)*meanYrlyImprovMale # male contribution

plot(2001:2018, meanYrlyImprovFem, pch = 20, type = "l", # plots life expectancy across rego years
     xlab = "Registration Year", ylab = "Life Expectancy",
     main = "Comparison between male and female life expectancy",
     ylim = range(c(min(meanYrlyImprovMale), max(meanYrlyImprovFem))),
     col = 'red')
lines(2001:2018, meanYrlyImprovMale, type = 'l', col = 'blue')
lines(2001:2018, meanYrlyImprovPop, type = 'l', lty = 2)
legend("bottomright", legend = c("Female", "Population", "Male"), 
       col = c("red", "black", "blue"), lty = c(1, 2, 1), 
       cex = 1.2, box.lty = 0, text.width = 5)



### THIS IS JUST FOR FEMALES
  femLifeChange <- t(lifeExpecFem)
  femLifeChange <- femLifeChange[-1,]
  femLifeChange <- cbind(2000+1:length(unique(data$Registration.year)), femLifeChange)
  femLifeChange <- cbind(femLifeChange, meanYrlyImprovFem)
  colnames(femLifeChange) <- c("year", deciles, "Mean")
  
  femLifeChangeVar <- cbind(femLifeChange[,-1], "Var" = rowVars(femLifeChange[, -1]))
  
  femLifeChange <- reshape2::melt(femLifeChange,  id = "year")
  
  # I DONT KNOW HOW TO MAKE THIS PROCESS TIDIER
  femLifeChange <- femLifeChange[-(1:18),]
  fMeanLifeChange <- femLifeChange %>%
    filter(Var2 == "Mean")
  femLifeChange <- femLifeChange %>%
    filter(Var2 != "Mean")
  
  
  ggplot(data = femLifeChange,
         aes(x = Var1, y = value, colour = Var2)) +
    geom_line(size = 1.5) + labs(title = "Fem. life expectancy over registration years") +
    guides(col = guide_legend(ncol=2)) + ylab("Complete LE") + labs(color = "Deciles") +
    xlab("Registration Year") + ylim(70, 87) + 
    geom_line(data = fMeanLifeChange, aes(x = Var1, y = value, colour = "Mean"), colour = "Black",
              linetype = "dashed")
  
### THIS IS JUST FOR MALES
  maleLifeChange <- t(lifeExpecMale)
  maleLifeChange <- maleLifeChange[-1,]
  maleLifeChange <- cbind(2000+1:length(unique(data$Registration.year)), maleLifeChange)
  maleLifeChange <- cbind(maleLifeChange, meanYrlyImprovMale)
  colnames(maleLifeChange) <- c("year", deciles, "Mean")
  
  
  # Plot for change in variance over time
  maleLifeChangeVar <- cbind(maleLifeChange[,-1], "Var" = rowVars(maleLifeChange[, -1]))

  
  plot(2001:2018, maleLifeChangeVar[,ncol(maleLifeChangeVar)], type = 'l', ylim = c(3, 8.5),
       ylab = "Variance", col = 'blue',
       xlab = "Registration Years", lwd = 2, main = "Life Expectancy Variance between Deciles")
  lines(2001:2018, femLifeChangeVar[,ncol(femLifeChangeVar)], col = 'red', type = 'l', lwd = 2)
  legend("right", legend = c("Male", "Fem."), col = c("blue", "red"), lty = 1, 
         cex = 0.8, box.lty = 0, text.width = 5)
  
  
  
  
  maleLifeChange <- reshape2::melt(maleLifeChange,  id = "year")
  
  # I DONT KNOW HOW TO MAKE THIS PROCESS TIDIER
  maleLifeChange <- maleLifeChange[-(1:18),]
  mMeanLifeChange <- maleLifeChange %>%
    filter(Var2 == "Mean")
  maleLifeChange <- maleLifeChange %>%
    filter(Var2 != "Mean")
  
  
  ggplot(data = maleLifeChange,
         aes(x = Var1, y = value, colour = Var2)) +
    geom_line(size = 1.5) + labs(title = "Male life expectancy over registration years") +
    guides(col = guide_legend(ncol=2)) + ylab("Complete LE") + labs(color = "Deciles") +
    xlab("Registration Year") + ylim(70, 87) + 
    geom_line(data = mMeanLifeChange, aes(x = Var1, y = value, colour = "Mean"), colour = "Black",
              linetype = "dashed")


# could check for a confounding variable by comparing number of men to women per decile
  femPerDec = c(rep(0, 10))
  malePerDec = c(rep(0,10))
  for(i in 1:10) {
    femPerDec[i] = sum(data[data$IMD.Decile == i & data$sex == "female",]$population)
    malePerDec[i] = sum(data[data$IMD.Decile == i & data$sex == "male",]$population)
  }
  # working out limits for graph
  if(max(malePerDec) > max(femPerDec)) {
    max = max(malePerDec)
  } else {
    max = max(femPerDec)
  }
  if(min(malePerDec) < min(femPerDec)) {
    min = min(malePerDec)
  } else {
    min = min(femPerDec)
  }

  ratio <- data.frame("Male" = c(rep(0, 10)), "Female" = c(rep(0,10)))
  ratio$Male <- malePerDec/(malePerDec+femPerDec)
  ratio$Female <- femPerDec/(malePerDec+femPerDec)
  ratio <- t(ratio)
  colnames(ratio) <- c(1:10)
  
  barplot(ratio, ylim = c(0.48, 0.53), ylab = "Population", col = c('darkblue',"red"),
          xlab = "Deciles", legend = colnames(counts), beside = TRUE, xpd = FALSE # clips bottom
          )
  
  plot(1:10, malePerDec, type = 'h', ylim = c(min, max), ylab = "Population", col = 'blue',
       xlab = "Deciles", lwd = 6)
  lines(1:10, femPerDec, col = 'red', type = 'h')
  legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1)

  
  
  # Combined life expectancy changes by decile
  # how life expectancy changes for men, women and popuation by decile in one graph
  
  
  
### Hypothesis testing

  px = c(rep(0, 10))
  nDec <- c(rep(0, 10))
  dDec <- c(rep(0,10))
  varpx <- c(rep(0,10))
  
  pxDecile2018 <- function(decileNum) {
    hypData <- data %>%
      filter(IMD.Decile == decileNum) %>%
      filter(Registration.year == 2018) %>%
      summarise(deaths = sum(deaths), population = sum(population))
    return(hypData)
  }
  
  for(i in 1:10) {
    nDec[i] <- pxDecile2018(i)$population
    dDec[i] <- pxDecile2018(i)$deaths
    px[i] <- dDec[i]/nDec[i]
    varpx[i] <- px[i]*(1-px[i])/nDec[i]
  }
  
  avg2018 <- data %>%
      filter(Registration.year == 2018) %>%
      summarise(deaths = sum(deaths), population = sum(population))
  avgqx2018 <- avg2018$deaths/avg2018$population
  
  decHypP <- c(rep(0,10))
  for(i in 1:10) {
    hypTest <- binom.test(x = dDec[i], n = nDec[i], p = avgqx2018, 
             alternative = "two.sided",
             conf.level = 0.95)
    decHypP[i] <- round(hypTest$p.value, digits = 3)
  } # giving weird values
  
  #using decile 1 as comparison
  
  
  
    
    
  
  
  
  
  
  
  
# creates new data frame and converts ages to numeric
calc_DASDR <- function(my_data) {
  GroupedData <- my_data %>%
    mutate(age = as.integer(as.character(age)))
  GroupedData$age[is.na(GroupedData$age)] <- 90 # changes 90+ to a numeric 90 value
  
  # create bins
  agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,150)
  agelabels <- c("0","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                 "70-74","75-79","80-84","85-89", "90+")
  
  setDT(GroupedData)[ , agegroups := cut(age, 
                                         breaks = agebreaks, 
                                         right = FALSE, 
                                         labels = agelabels)]
  
  # Group to bins and place relevant ESP value
  GroupedData <- GroupedData %>%
    group_by(agegroups) %>%
    summarise(deaths = sum(deaths), population = sum(population)) %>%
    mutate(ESP = ESP[which(agegroups == agelabels)])
  
  # calculating Age-specific and age standardised value
  GroupedData <- GroupedData %>%
    mutate("Age-specific" = deaths/population * 100000) %>%
    mutate("Age-Standardised" = `Age-specific` * ESP)
  
  DASDR <- sum(GroupedData$`Age-Standardised`/100000)
  return(DASDR)
}

# DASDR for Males throughout time
  maleTimeDASDR <- as.data.frame(deciles)
  
  for(i in 1:length(unique(data$Registration.year))) {
    varName <- paste0(2000+i)
    maleTimeDASDR <- maleTimeDASDR %>%
      mutate(!!varName := 0)
    for(j in 1:length(unique(data$IMD.Decile))) {
    
      maleTimeDASDR[j,i+1] <- calc_DASDR(LtMaleList[[i]][[j]])
    } # i + 1 to skip decile column 
  }
  
  
  
  
# DASDR for Females throughout time
  femTimeDASDR <- as.data.frame(deciles)
  
  for(i in 1:length(unique(data$Registration.year))) {
    varName <- paste0(2000+i)
    femTimeDASDR <- femTimeDASDR %>%
      mutate(!!varName := 0)
    for(j in 1:length(unique(data$IMD.Decile))) {
      
      femTimeDASDR[j,i+1] <- calc_DASDR(LtFemList[[i]][[j]])
    } # i + 1 to skip decile column 
  }
  

  
  
  groupData <- function(df, gender) {
    GroupedData <- df %>%
      filter(sex == gender) %>%
      mutate(age = as.integer(as.character(age)))
    GroupedData$age[is.na(GroupedData$age)] <- 90 # changes 90+ to a numeric 90 value
    
    # create bins
    agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,150)
    agelabels <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                   "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                   "70-74","75-79","80-84","85-89", "90+")
    
    setDT(GroupedData)[ , agegroups := cut(age, 
                                           breaks = agebreaks, 
                                           right = FALSE, 
                                           labels = agelabels)]
    
    # Group to bins and place relevant ESP value
    GroupedData <- GroupedData %>%
      group_by(agegroups, IMD.Decile) %>%
      summarise(deaths = sum(deaths), population = sum(population)) %>%
      mutate(ESP = ESP[which(agegroups == agelabels)])
    
    # calculating Age-specific and age standardised value
    GroupedData <- GroupedData %>%
      mutate("Age-specific" = deaths/population * 100000) %>%
      mutate("Age-Standardised" = `Age-specific` * ESP)
    
    return(GroupedData)
  }
  
  
  
  
  
  maleGroup = groupData(data, "male")
  femaleGroup = groupData(data, "female")
  
  
  
  
# Combines all registration years and calculates the DASDR per Decile
# If you want can break it up into age specific as well
  DecDASDR <- c(rep(0,10))
    
  
  GroupedData <- data %>%
    mutate(age = as.integer(as.character(age)))
  GroupedData$age[is.na(GroupedData$age)] <- 90 # changes 90+ to a numeric 90 value
  
  # create bins
  agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,150)
  agelabels <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                 "70-74","75-79","80-84","85-89", "90+")
  
  setDT(GroupedData)[ , agegroups := cut(age, 
                                         breaks = agebreaks, 
                                         right = FALSE, 
                                         labels = agelabels)]
  
    ####### To be used for population pyamid
          pyramidData <- GroupedData
          
          pyramidData$IMD.Decile <- as.factor(pyramidData$IMD.Decile)
          
          pyramidData <- pyramidData %>%
            filter(Registration.year == 2018) %>%
            group_by(agegroups, IMD.Decile, sex) %>%
            summarise(deaths = sum(deaths), population = sum(population))
          
  
          
          
          ggplot(pyramidData, aes(x = agegroups, fill = IMD.Decile,
                                  ifelse(test = sex == "male",
                                         yes = -population/1000000, no = population/1000000)), 
                 ylim = max(pyramidData$population)/1000000 * c(-1,1)) + 
            geom_bar(stat = "identity", position = "stack") +
            labs(title = "English population Pyramid for 2018", x = "Age", y = "Population (millions)") +
            scale_fill_brewer(palette = "Set1") +
            geom_hline(yintercept = 0, color = "black", lwd = 2) +
            coord_flip() +
            annotate("text", x = 19, y = 1.5, label = "bold(Female)", parse = TRUE) +
            annotate("text", x = 19, y = -1.5, label = "bold(Male)", parse = TRUE)
         # IF YOU WANT ABS ON BOTTOM TRY labels = abs
          
  
          
          
  # Group to bins and place relevant ESP value
  GroupedData <- GroupedData %>%
    group_by(agegroups, IMD.Decile) %>%
    summarise(deaths = sum(deaths), population = sum(population)) %>%
    mutate(ESP = ESP[which(agegroups == agelabels)])
  
  # calculating Age-specific and age standardised value
  GroupedData <- GroupedData %>%
    mutate("Age-specific" = deaths/population * 100000) %>%
    mutate("Age-Standardised" = `Age-specific` * ESP)
  
  for(i in 1:length(unique(data$IMD.Decile))) {
    decGroupData <- GroupedData[GroupedData$IMD.Decile == i,]
    DecDASDR[i] <- sum(decGroupData$`Age-Standardised`/100000)
  }

  
  # FEMALE DECDASDR
    femDecDASDR <- c(rep(0,10))
    
    
    for(i in 1:length(unique(data$IMD.Decile))) {
      decGroupData <- femaleGroup[femaleGroup$IMD.Decile == i,]
      femDecDASDR[i] <- sum(decGroupData$`Age-Standardised`/100000)
    }
  
    
    # MALE DECDASDR
    maleDecDASDR <- c(rep(0,10))
    
    
    for(i in 1:length(unique(data$IMD.Decile))) {
      decGroupData <- maleGroup[maleGroup$IMD.Decile == i,]
      maleDecDASDR[i] <- sum(decGroupData$`Age-Standardised`/100000)
    }
  
  
  
  
  
  plot(x = 1:10, y = DecDASDR, ylim = c(700, 1800),
       xlab = "Deciles", ylab = "DASDR", main = "DASDR per Decile", 
       type = 'l', lty = 2)
  lines(x = 1:10, y = maleDecDASDR, col = "blue")
  lines(x = 1:10, y = femDecDASDR, col = "red")
  legend("topright", legend = c("Female", "Population", "Male"), 
         col = c("red", "black", "blue"), lty = c(1, 2, 1), 
         cex = 1.2, text.width = 3)

  
  
  

# FEMALE DASDR GRAPH SET UP
  femDASDRChange <- t(femTimeDASDR)
  femDASDRChange <- femDASDRChange[-1,]
  femDASDRChange <- cbind(2000+1:length(unique(data$Registration.year)), femDASDRChange)
  colnames(femDASDRChange) <- c("year", deciles)
  
  femDASDRVar <- cbind(femDASDRChange[,-1], "Var" = rowVars(femDASDRChange[, -1]))
  
  femDASDRChange <- reshape2::melt(femDASDRChange,  id = "year")
  
  # I DONT KNOW HOW TO MAKE THIS PROCESS TIDIER
  femDASDRChange <- femDASDRChange[-(1:18),]


  ggplot(data = femDASDRChange,
         aes(x = Var1, y = value, colour = Var2)) +
    geom_line(size = 1.5) + labs(title = "Female DASDR over registration years") +
    guides(col = guide_legend(ncol=2)) + ylab("DASDR") + labs(color = "Deciles") +
    xlab("Registration Year") + ylim(600, 2050)

  
  
  
  
  maleDASDRChange <- t(maleTimeDASDR)
  maleDASDRChange <- maleDASDRChange[-1,]
  maleDASDRChange <- cbind(2000+1:length(unique(data$Registration.year)), maleDASDRChange)
  colnames(maleDASDRChange) <- c("year", deciles)
  
  
  # Plot for change in variance over time
  maleDASDRVar <- cbind(maleDASDRChange[,-1], "Var" = rowVars(maleDASDRChange[, -1]))
  
  
  plot(2001:2018, maleDASDRVar[,ncol(maleDASDRVar)], type = 'l', ylim= c(19000, 70000),
       ylab = "Variance", col = 'blue',
       xlab = "Registration Years", lwd = 2, main = "DASDR Variance between Deciles")
  lines(2001:2018, femDASDRVar[,ncol(femDASDRVar)], col = 'red', type = 'l', lwd = 2)
  legend("right", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1, 
         cex = 1.2, box.lty = 0, text.width = 5)
  
  
  maleDASDRChange <- reshape2::melt(maleDASDRChange,  id = "year")
  
  # I DONT KNOW HOW TO MAKE THIS PROCESS TIDIER
  maleDASDRChange <- maleDASDRChange[-(1:18),]
  
  ggplot(data = maleDASDRChange,
         aes(x = Var1, y = value, colour = Var2)) +
    geom_line(size = 1.5) + labs(title = "Male DASDR over registration years") +
    guides(col = guide_legend(ncol=2)) + ylab("DASDR") + labs(color = "Deciles") +
    xlab("Registration Year") + ylim(600, 2050)
  
  
  
########### Rectangularisation

  
plot(1, type ="n", xlim = c(0, 100), ylim = c(1000, 100000), 
     xlab = "Ages", ylab = "Population Alive", main = "Rectangularisation over Deciles")
abline(v = 100, col = "red")
abline(h = 100000, col = "red")
for(i in 1:9) {
  lines(x = lifeMale[[18]][[i]]@x, y = lifeMale[[18]][[i]]@lx, type = 'l', col = "gray")
}
lines(x = lifeMale[[18]][[10]]@x, y = lifeMale[[18]][[10]]@lx, type = 'l', lwd = 2)



                          # Task 2
# for my student id 5194905 - I am gradutating females and decile 5 for the year 2018
# is for age range of 40 - 89

gradData <- LtFemList[[18]][[5]]

# issue is that many are bunchde up into the 90+ age bracket...
# my issue to fix it is just to remove it for now
gradData <- gradData[-(length(gradData$age)),]

Ex1 <- gradData$population
mx1 <- gradData$mx
Dx1 <- gradData$deaths

age <- as.numeric(as.character(gradData$age)) # gradData$age was a factor type
plot(age, Dx, type = "l", xlab = "age", ylab = "Number of deaths",
     main = "Australian Women: 2018")

plot(age, Ex, type = "l", xlab = "age", ylab =
       "Exposed to risk",
     main = "Australian Women: 2018")

plot(log(mx), type = 'l' , xlab = "age", ylab =
       "Central mortality rate (log scale)",
     main = "Australian Women: 2018")



x <- 40:89
mx <- mx1[x] # these all had as.character(x) and I dont know why
Ex <- Ex1[x]
Dx <- Dx1[x]
 
# setting up models

  ### GOMPERTZ ###
startGomp <- list(b0=1, b1=0)
gompertz <- nls(mx ~ exp(b0 + b1*x), start = startGomp, weights = Ex/mx)
### MAKEHAM ###
startMake <- list(A = 0, b0 = 0.01, b1 = 0.01)
makeham <- nls(mx ~ A + exp(b0 + b1*x), start = startMake, weights = Ex/mx)
### SPLINES ###
knots <- c(7, 12, 16, 18, 20, 32, 53, 54, 61, 66, 77)
cubic_basis <- ns(x, knots = knots)
cubSpline <- lm(mx ~ cubic_basis, weights = Ex/mx )
### SMOOTHED SPLINES ###
# cv = t picks best spar
smSpline <- smooth.spline(x, mx, cv = T)


shrtx <- 43:89
shrtmx <- mx1[shrtx]
shrtEx <- Ex1[shrtx]
shrtDx <- Dx1[shrtx]
shrtmakeham <- nls(shrtmx ~ A + exp(b0 + b1*shrtx), start = startMake, weights = shrtEx/shrtmx)

  # Graphically show best spar
    x = 40:89
    y = mx
    splineres <- function(spar){
      res <- rep(0, length(x))
      for (i in 1:length(x)){
        mod <- smooth.spline(x[-i], y[-i], spar = spar)
        res[i] <- predict(mod, x[i])$y - y[i]
      }
      return(sum(res^2))
    }
    
    spars <- seq(0, 1.5, by = 0.001)
    ss <- rep(0, length(spars))
    for (i in 1:length(spars)){
      ss[i] <- splineres(spars[i])
    }
    plot(spars, ss, 'l', xlab = 'spar', ylab = 'Cross Validation Residual Sum of Squares' , main = 'CV RSS vs Spar')
    points(x = smSpline$spar, y = splineres(smSpline$spar), col = "red")
    legend("bottomright", legend = "Spar Chosen\n(0.546,0.0001)", pch = 1, col = "red", box.lty = 0)
    bestSpar <- spars[which.min(ss)]
    


# fitting models
mx_gompertz <- fitted(gompertz)
mx_makeham <- fitted(makeham)
mx_cubSpline <- fitted(cubSpline)
mx_smSpline <- fitted(smSpline)

mx_shrtmakeham <- fitted(shrtmakeham)



# plotting models

  # seperate
    # gomp
      plot(x, log(mx), pch = 20, xlab = "age", 
           ylab = "Central mortality rate (log scale)",
           main = "Australian Women: Gompertz law")
      lines(x, log(mx_gompertz), col ='blue')

    # makeham
      plot(x, log(mx), pch = 20, xlab = "age", ylab =
             "Central mortality rate (log scale)",
           main = "Australian Women: Makeham law")
      lines(x,log(mx_makeham), col = 'blue')

    # splines
      plot(x, log(mx), pch = 20, xlab = "age", ylab
           = "Central mortality rate (log scale)",
           main = "Australian Women: Natural cubic spline")
      lines(x, log(mx_cubSpline), col = 'blue')
      
    # smoothed splines
      plot(x, log(mx), pch = 20, xlab = "age", ylab
           = "Central mortality rate (log scale)",
           main = "Australian Women: Smoothed spline")
      lines(x, log(mx_smSpline), col = 'blue')
  
  # together
    plot(x, log(mx), pch = 20, xlab = "Age", ylab = "Central mortality rate (log scale)",
        main = "Australian Women: All Graduation Models", cex = 0.8)
      lines(x, log(mx_gompertz), col ='blue', lwd = 1)
      lines(x,log(mx_makeham), col = 'red', lwd = 1)
      lines(x, log(mx_cubSpline), col = 'orange', lwd = 1)
      lines(x, log(mx_smSpline), col = 'purple', lwd = 1)
      legend("bottomright", legend = c("Gompertz", "Makeham", "Cubic Spline", "Smoothed Splines"), 
             col = c("blue", "red", "orange", "purple"), lty = 1)


# Testing Models
  # standardised deviations 
    zx_makeham <- (Dx - Ex * mx_makeham) / sqrt(Ex* mx_makeham)
    zx_gompertz <- (Dx - Ex * mx_gompertz) / sqrt(Ex * mx_gompertz)
    zx_cubSpline <- (Dx - Ex * mx_cubSpline) / sqrt(Ex * mx_cubSpline)
    zx_smSpline <- (Dx - Ex * mx_smSpline) / sqrt(Ex * mx_smSpline)
    
    zx_shrtmakeham <-  (shrtDx - shrtEx * mx_shrtmakeham) / sqrt(shrtEx * mx_shrtmakeham)
    
  # Finding the number of estimated parameters
    numP_makeham <- length(coef(makeham))
    numP_gompertz <- length(coef(gompertz))
    numP_cubSpline <- cubSpline$rank
    numP_smSpline <- smSpline$df
    
    numP_shrtmakeham <- length(coef(shrtmakeham))
    
    
    
  # Defining chi squared function
    chi2Test <- function(O, E, npar, alpha = 0.05){
      chi2 <- sum((O - E)^2 / E) #Test statistic
      df <- length(O) - npar
      chi2_alpha <- qchisq(1 - alpha, df) #Critical value
      p.value <- 1 - pchisq(chi2, df) #p.value
      list(statistic = chi2, c.value = chi2_alpha,
           df = df, p.value = p.value)
    }
  
  
  # Chi Squared tests
    chi_makeham <- chi2Test(Dx, Ex * mx_makeham, numP_makeham)
    chi_gompertz <- chi2Test(Dx, Ex * mx_gompertz, numP_gompertz)
    chi_cubSpline <- chi2Test(Dx, Ex * mx_cubSpline, numP_cubSpline)
    chi_smSpline <- chi2Test(Dx, Ex * mx_smSpline, numP_smSpline)
    
    chi_shrtmakeham <- chi2Test(shrtDx, shrtEx * mx_shrtmakeham, numP_shrtmakeham)
    
      
  
  # Standard Deviations Test defintion
    stdTest <- function(zx, breaks = c(-Inf, -1, 0,
                                       1, Inf)){
      observed <- table(cut(zx, breaks)) #count observation in each interval
      expected.p <- diff(pnorm(breaks)) #expected probabilities for standard normal
      chisq.test(observed, p = expected.p) #apply chisquare test
    }
    
  # Standard Deviations Test-
    stdTest_gompertz <- stdTest(zx_gompertz)
    stdTest_makeham <- stdTest(zx_makeham)
    stdTest_cubSpline <- stdTest(zx_cubSpline)
    stdTest_smSpline <- stdTest(zx_smSpline)
    
    stdTest_shrtmakeham <- stdTest(zx_shrtmakeham)
    # if p value < sig value reject the null that residuals follow normal
    # CHECK THE TUTORIAL FOR DOING A Q-Q PLOT WITH THESE AS WELL
    
    
  # Cumulative Deviations Test Definition
    cumDevTest <- function(A, E, alpha = 0.05){
      cumDev <- sum(A - E) / sqrt(sum(E)) #Test statistic
      z_alpha <- qnorm(1 - alpha/2) #Critical value
      p.value <- 2 *(1 - pnorm(abs(cumDev))) #p.value (Note it is two-tailed)
      list(statistic = cumDev, c.value = z_alpha,
     p.value = p.value)
    }
    
  # Cumulative Deviations Test
    cumDevTest_gompertz <- cumDevTest(Dx, Ex * mx_gompertz)
    cumDevTest_makegam <- cumDevTest(Dx, Ex * mx_makeham)
    cumDevTest_cubSpline <- cumDevTest(Dx, Ex * mx_cubSpline)
    cumDevTest_smSpline <- cumDevTest(Dx, Ex * mx_smSpline) # THIS HAS A P value > 1??
    
    cumDevTest_shrtmakeham <- cumDevTest(shrtDx, shrtEx * mx_shrtmakeham)
    

  # Signs Test
    nages <- length(x)
    signTest_gompertz <- binom.test(sum(zx_gompertz > 0), nages)
    signTest_makeham <- binom.test(sum(zx_makeham > 0), nages)
    signTest_cubSpline <- binom.test(sum(zx_cubSpline > 0), nages)
    signTest_smSpline <- binom.test(sum(zx_smSpline > 0), nages)
    
    signTest_shrtmakeham <- binom.test(sum(zx_shrtmakeham > 0), nages)
    
  # Grouping of signs test definition
    groupSignTest <- function(zx, alpha = 0.05){
      #Count +'s and -'s
      signs <- sign(zx)
      n1 <- sum(signs == 1)
      n2 <- sum(signs == -1)
      #Count runs
      y <- c(-1, sign(zx))
      G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1]
               != -1) # No Runs
      #Normal approximation
      mu <- n1 * (n2 + 1) / (n1 + n2)
      s2 <- (n1 * n2)^2 / (n1 + n2)^3
      G_alpha <- qnorm(alpha, mean = mu, sd = sqrt
                       (s2)) #Critical value
      p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) #p.value (one sided)
      list(statistic = G, c.value = G_alpha, p.value = p.value)
    }
    
  # Grouping of Signs test
    groupSignTest_gompertz <- groupSignTest(zx_gompertz)
    groupSignTest_makeham <- groupSignTest(zx_makeham)
    groupSignTest_cubSpline <- groupSignTest(zx_cubSpline)
    groupSignTest_smSpline <- groupSignTest(zx_smSpline)
  
    groupSignTest_shrtmakeham <- groupSignTest(zx_shrtmakeham)
  # Serial Correlations Test
    par(mfrow=c(2,2))
    acf(zx_gompertz) # up to lag 7
    acf(zx_makeham) # 
    acf(zx_cubSpline) # errors oscilate
    acf(zx_smSpline) # errors oscilate
    par(mfrow=c(1,1))
    
    acf(zx_shrtmakeham)
    
# Now need to do tests for undergraduation
  # Testing for smoothness
  diffArray <- function(modelVal) {
    diff = c() # an array
    for(i in 1:(length(modelVal)-1)) {
      diff[i] <- modelVal[i+1] - modelVal[i]
    }
    return(diff)
  }
  
  findThirdDiff <- function(modelVal) {
    firstDiff <- diffArray(modelVal)
    secondDiff <- diffArray(firstDiff)
    thirdDiff <- diffArray(secondDiff)
    return(thirdDiff)
  }
  
  diff_gompertz <- findThirdDiff(mx_gompertz)
  diff_makeham <- findThirdDiff(mx_makeham)
  diff_cubspline <- findThirdDiff(mx_cubSpline)
  diff_smSpline <- findThirdDiff(mx_smSpline)
  
  propdiff_gompertz <- abs(diff_gompertz/mx_gompertz[-50:-48])
  propdiff_makeham <- abs(diff_makeham/mx_makeham[-50:-48])
  propdiff_cubspline <- abs(diff_cubspline/mx_cubSpline[-50:-48])
  propdiff_smSpline <- abs(diff_smSpline/mx_smSpline[-50:-48])
  
  
  diff_mins <- c(min(propdiff_gompertz), min(propdiff_makeham), 
                 min(propdiff_cubspline), min(propdiff_smSpline))
  diff_maxs <- c(max(propdiff_gompertz), max(propdiff_makeham), 
                 max(propdiff_cubspline), max(propdiff_smSpline))
  

  plot(40:86, propdiff_gompertz, 
       xlab = "Ages", ylab = "% of model value", 
       main = "Third differences of models as proportion of their fitted value",
       ylim = c(min(diff_mins), max(diff_maxs)), 
       col = "blue", pch = 20)
  lines(40:86, propdiff_makeham, col = "red", type = "p", pch = 20)
  lines(40:86, propdiff_cubspline, col = "orange", type = 'p', pch = 8)
  lines(40:86, propdiff_smSpline, col = "purple", type = 'p', pch = 17)
  legend("topright", legend = c("Gompertz", "Makeham", "Cubic Spline", "Smoothed Splines"), 
         col = c("blue", "red", "orange", "purple"), pch = c(20, 20, 8, 17), 
         cex = 0.8, text.width = 8)
  
  
  
  mean(propdiff_gompertz)
  mean(propdiff_makeham)
  mean(propdiff_cubspline)
  mean(propdiff_smSpline)

  summaryStats <- as.data.frame(c("Gompertz", "Makeham", "Cubic Splines", "Smoothed Splines"))
  summaryStats <- summaryStats %>% 
    rename(
      Model = `c("Gompertz", "Makeham", "Cubic Splines", "Smoothed Splines")`
    )
  
  
  summaryStats$chi_p <- c(chi_gompertz$p.value, chi_makeham$p.value, chi_cubSpline$p.value, chi_smSpline$p.value)
  summaryStats$std_p <- c(stdTest_gompertz$p.value, stdTest_makeham$p.value, stdTest_cubSpline$p.value, stdTest_smSpline$p.value)
  summaryStats$cumDev_p <- c(cumDevTest_gompertz$p.value, cumDevTest_makegam$p.value, cumDevTest_cubSpline$p.value, cumDevTest_smSpline$p.value)
  summaryStats$signs_p <- c(signTest_gompertz$p.value, signTest_makeham$p.value, signTest_cubSpline$p.value, signTest_smSpline$p.value)
  summaryStats$groupSign_p <- c(groupSignTest_gompertz$p.value, groupSignTest_makeham$p.value, groupSignTest_cubSpline$p.value, groupSignTest_smSpline$p.value)
  summaryStats$meanPropdiff <- c(mean(propdiff_gompertz), mean(propdiff_makeham), mean(propdiff_cubspline), mean(propdiff_smSpline))
  
  summaryStats[,-1] = round(summaryStats[,-1], digits = 3)
  
  chi_shrtmakeham$p.value
  stdTest_shrtmakeham$p.value
  cumDevTest_shrtmakeham$p.value
  signTest_shrtmakeham$p.value
  groupSignTest_shrtmakeham$p.value
  
                   # Task 3

