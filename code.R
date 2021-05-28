# load libraries
library(dplyr)
library(ggplot2)
library(smbinning)

# disable scientific notation
options(scipen = 999) 


# read data
data.raw <- read.csv("data/test.csv")

summary(data.raw)
dim(data.raw) # 25976 rows x 25 cols

tibble::glimpse(data.raw)
# Rows: 25,976
# Columns: 25
# $ X                                 <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, …  <- to remove  
# $ id                                <int> 19556, 90035, 12360, 77959, 36875, 39177, 7943…  <- to remove  
# $ Gender                            <chr> "Female", "Female", "Male", "Male", "Female", …  <- categorical 
# $ Customer.Type                     <chr> "Loyal Customer", "Loyal Customer", "disloyal …  <- categorical 
# $ Age                               <int> 52, 36, 20, 44, 49, 16, 77, 43, 47, 46, 47, 33…  <- numerical 
# $ Type.of.Travel                    <chr> "Business travel", "Business travel", "Busines…  <- categorical 
# $ Class                             <chr> "Eco", "Business", "Eco", "Business", "Eco", "…  <- ordinal  
# $ Flight.Distance                   <int> 160, 2863, 192, 3377, 1182, 311, 3987, 2556, 5…  <- numerical 
# $ Inflight.wifi.service             <int> 5, 1, 2, 0, 2, 3, 5, 2, 5, 2, 4, 2, 5, 1, 2, 3…  <- ordinal 
# $ Departure.Arrival.time.convenient <int> 4, 1, 0, 0, 3, 3, 5, 2, 2, 2, 1, 5, 5, 1, 2, 4…  <- ordinal 
# $ Ease.of.Online.booking            <int> 3, 3, 2, 0, 4, 3, 5, 2, 2, 2, 1, 5, 5, 4, 2, 0…  <- ordinal 
# $ Gate.location                     <int> 4, 1, 4, 2, 3, 3, 5, 2, 2, 2, 1, 5, 5, 1, 2, 3…  <- ordinal 
# $ Food.and.drink                    <int> 3, 5, 2, 3, 4, 5, 3, 4, 5, 3, 5, 1, 4, 5, 5, 2…  <- ordinal 
# $ Online.boarding                   <int> 4, 4, 2, 4, 1, 5, 5, 4, 5, 4, 1, 3, 5, 5, 5, 0…  <- ordinal 
# $ Seat.comfort                      <int> 3, 5, 2, 4, 2, 3, 5, 5, 5, 4, 5, 4, 5, 4, 4, 2…  <- ordinal 
# $ Inflight.entertainment            <int> 5, 4, 2, 1, 2, 5, 5, 4, 5, 4, 3, 2, 5, 5, 4, 2…  <- ordinal 
# $ On.board.service                  <int> 5, 4, 4, 1, 2, 4, 5, 4, 2, 4, 3, 2, 5, 5, 4, 4…  <- ordinal 
# $ Leg.room.service                  <int> 5, 4, 1, 1, 2, 3, 5, 4, 2, 4, 4, 2, 5, 5, 4, 2…  <- ordinal 
# $ Baggage.handling                  <int> 5, 4, 3, 1, 2, 1, 5, 4, 5, 4, 3, 2, 5, 5, 4, 4…  <- ordinal 
# $ Checkin.service                   <int> 2, 3, 2, 3, 4, 1, 4, 5, 3, 5, 1, 3, 5, 3, 3, 4…  <- ordinal 
# $ Inflight.service                  <int> 5, 4, 2, 1, 2, 2, 5, 4, 3, 4, 3, 2, 5, 5, 4, 5…  <- ordinal 
# $ Cleanliness                       <int> 5, 5, 2, 4, 4, 5, 3, 3, 5, 4, 4, 4, 3, 5, 5, 2…  <- ordinal 
# $ Departure.Delay.in.Minutes        <int> 50, 0, 0, 0, 0, 0, 0, 77, 1, 28, 29, 18, 0, 11…  <- numerical 
# $ Arrival.Delay.in.Minutes          <dbl> 44, 0, 0, 6, 20, 0, 0, 65, 0, 14, 19, 7, 0, 11…  <- numerical 
# $ satisfaction                      <chr> "satisfied", "satisfied", "neutral or dissatis…  <- y

# check missing values
colSums(is.na(data.raw)) # 83 NA in Arrival.Delay.in.Minutes 

# remove rows that contain missing values
data <- data.raw[complete.cases(data.raw), ]

# remove X and ID 
data <- data %>% 
  select(c(-1, -2))


## CHANGING VAR TO FACTOR ---------------------------------------------------------------------------------

# Gender
data$Gender <- factor(data$Gender, levels = c("Male", "Female"), labels = c("Male", "Female"))

# Customer.Type
data$Customer.Type <- factor(data$Customer.Type, levels = c("Loyal Customer", "disloyal Customer"), labels = c("Loyal Customer", "disloyal Customer") )

# Type.of.Travel
data$Type.of.Travel <- factor(data$Type.of.Travel, levels = c("Business travel", "Personal Travel"), labels = c("Business travel", "Personal Travel"))

# Class
data$Class <- factor(data$Class, levels = c("Eco", "Business"), labels = c("Eco", "Business"))

# Ratings
for (col_num in c(7:20)) {
  data[, col_num] <- factor(data[ , col_num], levels = c(0:5), labels = c(0:5))
}

# satisfaction
data$satisfaction <- factor(data$satisfaction, levels = c("satisfied", "neutral or dissatisfied"), labels = c("satisfied", "neutral or dissatisfied"))

tibble::glimpse(data)

colnames(data) <- gsub("\\.", "_", colnames(data))

# dividing variables into FACTORS and NUMERICS
num_vars <- data[, sapply(data, is.numeric)]
fct_vars <- data[, sapply(data, is.factor)]

## FINE CLASSING ------------------------------------------------------------------------------------------

# checking the basic statistics of numeric variables
summary(num_vars)

percentile <- apply(X = num_vars, MARGIN = 2, FUN = function(x) round(quantile(x, seq(0.1,1,0.1), na.rm = TRUE),2))

# unique values per column
unique <- apply(num_vars, MARGIN = 2, function(x) length(unique(x)))

num_vars[which(unique < 10 & unique > 1)] # no column has unique values less than 10 

# binarization
for (c in colnames(num_vars)) {
  num_vars[, paste(c, "_fine", sep = "")] <- cut(
    x = as.matrix(num_vars[c]),
    breaks = c(-Inf, unique(percentile[, c])),
    labels = c(paste("<=", unique(percentile[, c])))
  )
}

head(num_vars)

# Weight of Evidence calculation using smbinning

# re-defining GB flag 
# "satisfied" -> 0
# "neutral or dissatisfied" -> 1 (default)
data$def <- ifelse(data$satisfaction == "satisfied", 0, 1)
data$def_woe <- 1 - data$def

num_vars$def <- data$def 
num_vars$def_woe <- data$def_woe

head(data)

# a list to store WOE results
WOE <- list()

# a data frame to store Information Value
IV <- data.frame(VAR = character(), IV = integer())

num_vars.f <- num_vars[, grepl(pattern = "_fine" , x = names(num_vars))]

head(num_vars.f)

# create a pdf to store result
pdf(file = "WoE_numeric.pdf", paper = "a4")

names.n <- colnames(num_vars[, !names(num_vars) %in% c(colnames(num_vars.f), "def", "def_woe")])


#Set up progress bar

total_pb <- length(names.n)
pb <- txtProgressBar(min = 0, max = total_pb, style = 3)
# min, max - extreme values for progress bar
# style=3 fraction of task that are done

for (i in names.n) {
  # rows and columns at a chart
  par(mfrow = c(2, 2))
  
  # smbinning.custom(df, y, x, cuts)
  # df - data frame
  # y - GB flag
  # x - risk factors
  # cuts - cut-offs
  
  results <-
    smbinning.custom(
      df = num_vars,
      y = "def_woe",
      x = i,
      cuts = unique(percentile[, i])
    )
  
  # Relevant plots (2x2 Page)
  
  # BOXPLOT
  boxplot(
    num_vars[, i] ~ num_vars$def,
    horizontal = T,
    frame = F,
    col = "lightgray",
    main = "Distribution"
  )
  mtext(i, 3)
  # Frequency plot
  smbinning.plot(results, option = "dist", sub = i)
  # Bad rate fractions
  smbinning.plot(results, option = "badrate", sub = i)
  # WoE
  smbinning.plot(results, option = "WoE", sub = i)
  
  
  
  # IV row binding
  IV <-
    rbind(IV, as.data.frame(cbind(
      "VAR" = i, "IV" = results$ivtable[results$ivtable$Cutpoint == "Total", "IV"]
    )))
  
  # Saving data
  d <- results$ivtable[, c("Cutpoint", "WoE", "PctRec")]
  # Total row removal
  d <- d[d$Cutpoint != "Total", ]
  # Ordering wrt WoE - for gini etc. calculation
  d <- d[with(d, order(d$WoE)), ]
  # Id
  d$numer <- 11:(nrow(d) + 10)
  # Saving WoE in list
  WOE[[i]] <- d
  
  #Update progess bar
  setTxtProgressBar(pb,  min(grep(i, names.n)))
}
close(pb)
dev.off()



## COARSE CLASSING ----------------------------------------------------------------------------------------








