#
# Analytics Vidhya  "RampagingDataHulk" Mini Hackathon
#     2 April 2017
#     AtchiReddy Chavva (atchireddi@gmail.com)
#
#


#
# ***** Problem Statement ***** ----
#
#   Congratulations! you have been hired as a Chief Data Scientist by “QuickMoney”, 
#  a fast growing Hedge fund. They rely on automated systems to carry out trades 
#  in the stock market at inter-day frequencies. They wish to create a machine 
#  learning-based strategy for predicting the movement in stock prices.
#
#   They ask you to create a trading strategy to maximize their profits in the 
#  stock market. Stock markets are known to have high degree of unpredictability, 
#  but you have an opportunity to beat the odds and create a system which will 
#  outperform others.
#
#   The task for this challenge is to predict whether the price for a particular 
#  stock at the tomorrow’s market close will be higher(1) or lower(0) compared to 
#  the price at today’s market close.
#
#
# Important Points: 
#
#  Information derived from the use of information from future is not permitted. 
#  For example, If you're predicting for timestamp x, you must not use features 
#  from timestamp x+1 or any timestamp after that
#  Anyone found using such features will be disqualified from the hackathon
#

# ***** DATA UNDERSTANDING ***** ----


#  Variable	             Definition
#  ID	Unique             ID for each observation
#  Timestamp	           Unique value representing one day
#  Stock_ID	             Unique ID representing one stock
#  Volume	               Normalized values of volume traded of given stock ID on that timestamp
#  Three_Day_Moving_Average	      Normalized values of three days moving average of Closing price for given stock ID (Including Current day)
#  Five_Day_Moving_Average	      Normalized values of five days moving average of Closing price for given stock ID (Including Current day)
#  Ten_Day_Moving_Average	        Normalized values of ten days moving average of Closing price for given stock ID (Including Current day)
#  Twenty_Day_Moving_Average	    Normalized values of twenty days moving average of Closing price for given stock ID (Including Current day)
#  True_Range	                    Normalized values of true range for given stock ID
#  Average_True_Range	            Normalized values of average true range for given stock ID
#  Positive_Directional_Movement	Normalized values of positive directional movement for given stock ID
#  Negative_Directional_Movement	Normalized values of negative directional movement for given stock ID
#  Outcome	                      Binary outcome variable representing whether price for one particular stock at the tomorrow’s market close is higher(1) or lower(0) compared to the price at today’s market close     

#Note:
#  Evaluation Metric is log-loss
#You are expected to upload the solution in the format of "sample_submission.csv"
#Public and Private split is 30:70


# *************************************************************************----
#                PROCs ----
# *************************************************************************----

# Function to generate univariate plots for Numerical variables
# InPuts    :   DataFrame of Numerical Variables  
# OutPuts   :   Boxplot,HIstogram, Q-Q plots saves to ../output
#
# Flow   : 
#        1. re-format(collate) Numerical Variables
#        2.  -- Compute Skewness & Kurtosis
#            -- Compute slope & Intercept for Q-Q plot
#        3. Consolidated boxplots
#        4. Individual boxplots
#        5. Individual Histplots
#        6. Q-Q plots
#        7. Save plots
numerical_uni_plots  <- function(df,prefix) {
  nrows       <- nrow(df)
  df_num      <- data.frame()
  df_slp_int  <- data.frame()
  for (i in colnames(df)) {
    
    #   1. . . . Collate Variables ---
    df_num <- rbind(data.frame(var=rep(i,nrows),
                               val=df[[i]]
    ),
    df_num)
    
    #  2. . . . Caliculate slope & intercept ---
    # Find the slope and intercept of the line that passes through the 1st and 3rd
    # quartile of the normal q-q plot
    
    y     <- quantile(df[[i]], c(0.25, 0.75)) # Find the 1st and 3rd quartiles
    x     <- qnorm( c(0.25, 0.75))            # Find the matching normal values on the x-axis
    slope <- diff(y) / diff(x)                # Compute the line slope
    intr   <- y[1] - slope * x[1]             # Compute the line intercept
    mn     <- mean(df[[i]])                   # source for annotation co-ordinates
    sd     <- sd(df[[i]])                     # source for annotation co-ordinates
    df_slp_int <- rbind(data.frame(var=i,slp=slope,intcpt=intr,                   # slope & Intercept for qqline
                                   skw=sprintf("Sk=%.2f",skewness(df[[i]])),      # Skewness
                                   kurt=sprintf("Ku=%.2f",kurtosis(df[[i]])),     # Kurtosis
                                   sky=mn+sd,               # y-cord for skewness annotation
                                   kuy=mn-sd),              # y-cord for Kurtosis Annotation
                        df_slp_int)
  }
  
  df_num$var <- as.factor(df_num$var) # convert variables to factors
  
  # 3. . . . Combined Boxplot ---
  pcb <- ggplot(df_num,
                aes(var,val)
  )
  pcb <- pcb + geom_boxplot(colour="blue",
                            outlier.color = "red"
  )
  # pcb <- pcb + stat_summary(fun.data = give.n, geom = "text") 
  pcb <- pcb + ggtitle("BOXPLOTs (Numerical variables)")
  pcb <- pcb + theme(text=element_text(size = 10),          # title, labels
                     axis.text.x=element_text(angle = 45,   
                                              vjust = 0.5), # rotate & align x-axis labels
                     axis.text = element_text(size = 8)     # axis tick labels
  )
  pcb <- pcb + labs(x="",y="")
  
  
  # 4. . . . Individual Boxplots ---
  pib <- ggplot(df_num,
                aes(val,val)
  )
  pib <- pib + geom_boxplot(colour="blue",
                            outlier.colour = "red")
  pib <- pib + facet_wrap(~var,
                          scales = "free")
  pib <- pib + ggtitle(" BOXPLOTs (Numerical Variables) ") 
  pib <- pib + theme(axis.text.x=element_text(angle = 90), # x-axis labels rotated 90deg
                     axis.text=element_text(size = 8),     # axis tick labels
                     text=element_text(size = 10)          # title, labels
  )
  pib <- pib + labs(x="",y="")
  
  # 5. . . . Individual Histplots ---
  pih <- ggplot(df_num,
                aes(val)
  )
  pih <- pih + geom_histogram(fill="darkgreen",aes(y=..density..))
  pih <- pih + geom_density(colour="darkred")
  pih <- pih + facet_wrap(~var,
                          scales = "free")
  pih <- pih + ggtitle("DISTRIBUTION (Numerical Variables) ") 
  pih <- pih + theme(axis.text.x=element_text(angle = 90), # x-axis labels rotated 90deg
                     axis.text=element_text(size = 8),     # axis tick labels
                     text=element_text(size = 10)          # title, labels
  )
  pih <- pih + geom_text(data = df_slp_int, 
                         aes(label=skw), 
                         x=Inf,
                         y=Inf,
                         hjust = 1.2,
                         vjust = 2.2,
                         parse = T,
                         size=3)                          # skew annotation
  pih <- pih + geom_text(data = df_slp_int, 
                         aes(label=kurt),
                         x=Inf,
                         y=Inf,
                         hjust = 1.2,
                         vjust = 1.2,
                         parse = T,
                         size=3)                          # Kurt annotation
  
  pih <- pih + labs(x="",y="")
  
  
  # 6. . . . Combined Q-Q plots ---
  pqq <- ggplot(df_num) + stat_qq(aes(sample=val),
                                  color="red")            # qqplot 
  pqq <- pqq + facet_wrap(~var,
                          scales = "free")                # facet_wrap around Var
  pqq <- pqq + geom_abline(data = df_slp_int,
                           aes(intercept=intcpt,slope=slp),
                           color="blue")                  # qqline 
  pqq <- pqq + geom_text(data = df_slp_int, 
                         aes(label=skw), 
                         x=-Inf,
                         y=Inf,
                         hjust = -0.2,
                         vjust = 2.2,
                         parse = T,
                         size=3)                          # skew annotation
  pqq <- pqq + geom_text(data = df_slp_int, 
                         aes(label=kurt),
                         x=-Inf,
                         y=Inf,
                         hjust = -0.2,
                         vjust = 1.2,
                         parse = T,
                         size=3)                          # Kurt annotation
  pqq <- pqq + theme(text=element_text(size=10),          # title, labels
                     axis.text=element_text(size = 8)     # axis tick labels
  )
  pqq <- pqq + ggtitle("Q-Q Plots (Numerical variables)")
  pqq <- pqq + labs(x="",y="")
  
  
  # 7. . . . Save Plots ---
  # pdf("../output/univariate_numerical.pdf",paper = 'USr',width = 11)
  png(filename = paste("../output/",prefix,"_uni_num%02d.png",sep = ""), 
      width = 11.69, 
      height = 8.27, 
      units = "in",
      res = 288)
  # par(mfrow=c(3,1))  # Not Working
  print(plot_grid(pcb,pib))
  print(plot_grid(pib,pih))
  print(pqq)
  dev.off()
  
  # can't find a way to impose boxplot on histogram
}



# Plots MOdel performance, provided model and ref.labels
#  mdl -->  Model
#  ref -->  reference labels
#  clk -->  Line Colour
#  ad  -->  Should performance curve to be added to existing Plot
plot_model_perf <- function(mdl,ref,clr,ad) {
  pred_prob <- predict(mdl, type = "response")
  mdl_score <- prediction(pred_prob, ref)
  perf      <- performance(mdl_score, "tpr", "fpr")
  plot(perf,col=clr,add=ad)
}



# Greps Model R2 Values
getModelR2 <- function(mdl) {
  mdl_sumry <- capture.output(summary(mdl))
  print(mdl_sumry[grepl("R-squared",mdl_sumry)])
}



# Extracts Coefficients from Model Summary
mdlsmry2df <- function(smry) {
  # extract Coefficints sections
  # df_coeff <- data.frame()
  start_coeff <- grep("Intercept",smry)         # starting coeff
  end_coeff   <- grep("---",smry)               # end coeff
  coeff       <- smry[start_coeff:end_coeff-1]  # extract coeff
  coeff       <- gsub("< ","<",coeff[-1])       # clean-up
  #df_coeff    <- as.data.frame(strsplit(coeff,"\\s+"),ncol=6)  # create dataFrame
  # Create unqual list of list to DataFrame 
  coeff      <- strsplit(coeff,"\\s+")
  df_coeff   <- as.data.frame(do.call(rbind,lapply(coeff,'length<-',6)))  
  colnames(df_coeff) <- c("var","Estimate","Std.Error",
                          "t-value","Pr(>|t|)","Significance") # add proper colnames
  return(df_coeff)
}



# function to consolidate model summary and model vif,
# shows merged output as table
# Inputs   : linear model
# Outputs  :  model summary and vif returned as df
model_sumry_vif <- function(mdl) {
  
  # Model VIF
  print("....Generating Model VIF ....")
  mdl_vif   <- vif(mdl)
  df_vif    <-  data.frame(var=names(mdl_vif),
                           vif=as.numeric(mdl_vif))
  
  # Model Summary
  print("....Generating Model Summary ....")
  mdl_sumry <- capture.output(summary(mdl))
  df_sumry  <- mdlsmry2df(mdl_sumry)
  
  # Merger Summary and VIF  by variable
  print(".... Merging Summary and VIF ....")
  df <- merge(df_sumry, df_vif, by="var")
  return(df)
}



# Binning Model Year, 3 bins
#  2011-2015, 2006-2010, <2006
year_bin <- function(year) {
  year <- as.numeric(year)
  if(year>2010) {
    return("2011_2015")
  } else if (year>2005) {
    return("2006_2010")
  } else {
    return("<2006")
  }
}



# Convert 2-level Categorical Variable to numeric type
cat_lv2_2num <- function(df) {
  for (i in colnames(df)) {
    levels(df[[i]]) <- c(1,0)
    df[[i]] <- as.numeric(levels(df[[i]]))[df[[i]]]
  }
  return(df)
}



# COnvert categorical dependant variable to numeric type
# "Yes" <- 1
# "No"  <- 0
YesNo2num <- function(v) {
  v <- as.character(v)
  v[v=="Yes"] <- "1"
  v[v=="No"]  <- "0"
  v <- as.numeric(v)
  return(v)
}



# Dimensional Variable Univariate(barlot) Analysis plots
dim_uni_qplot <- function(data,dim) {
  v <- data[[ dim ]]
  p <- qplot(x=reorder(v,v,function(x)-length(x))) + 
    theme(axis.text.x=element_text(angle = 45,hjust = 1)) + 
    labs(x="",title=dim)
  p
  print(p)
}


dim_multi_vsResponse <- function(data, dim, resp) {
  # since reorder with aes_string not working, will create local df
  # and used hardcode colnames for plotting
  
  local_df <- data[c(dim,resp)]
  colnames(local_df) <- c("dim", "resp")
  
  
  # barplot
  b <- ggplot(local_df, aes(x=reorder(dim,dim,function(x)-length(x)))) + geom_bar(aes(y=(..count..))) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
    geom_text(aes(y=(..count..), 
                  label = scales::percent( (..count..)/sum(..count..))),
              stat = "count",
              vjust = -0.5
    ) +
    labs(x="", title=dim) 
  # percent barplot
  pb <- ggplot(local_df, aes(x=reorder(dim,dim,function(x)-length(x)),fill=resp)) + geom_bar(position = "fill") +
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
    labs(x="", title=dim)
  # gridplot
  plot_grid(b,pb)
}



# percent barplot of multi-variate categorical variables
dim_multi_percent_barplot <- function(data,dim,col) {
  ggplot(data, aes_string(dim)) +
    geom_bar(aes_string(fill = col), position = "fill") + 
    theme(axis.text.x=element_text(angle = 45,vjust = 0.5))
}



# Replace missing values with Mean
rep_mean <- function(v) {
  v[is.na(v)] <- mean(v,na.rm = T)
  return(v)
}



# Replace missing values with Median
rep_median <- function(v) {
  v[is.na(v)] <- median(v,na.rm = T)
  return(v)
}



# Replace missing values with Zero
rep_zero <- function(v) {
  v[is.na(v)] <- 0
  return(v)
}



# Mark missing values as "uncategorized"
rep_uncategorized <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- "uncategorized"
  v <- as.factor(v)
  return(v)
}



# Replace value of  Factor variable
rep_dim_factor <- function(v, oldval, newval) {
  v <- as.character(v)
  v[v==oldval] <- newval
  v <- as.factor(v)
  return(v)
}



# Replace missing Categorical variable with Mode
rep_dim_mode <- function(v) {
  mod <- names(sort(-table(v)))[1]
  v[is.na(v)] <- mod
  return(v)
}



# Cap/floor outliers @ 25th and 95th percentile
cap_outliers <- function(v) {
  # make sure NO missing values
  quantiles <- quantile( v, c(.05, .95 ) )
  v[ v < quantiles[1] ] <- quantiles[1]
  v[ v > quantiles[2] ] <- quantiles[2]
  return(v)
}



# Summarize NA's
n.a.summary <- function(df) {
  obs <- nrow(df)
  cs <- colSums(is.na(df))  # NA's per column
  cls <- sapply(df,class)
  df <- data.frame(Vars = names(cs), NAS = cs, class=cls, row.names = NULL)
  df <- df[order(df$NAS),]
  df$perNAS <- 100*(df$NAS/obs)
  df
}

plot_model_perf <- function(mdl,ref,clr,ad) {
  pred_prob <- predict(mdl, type = "response")
  mdl_score <- prediction(pred_prob, ref)
  perf      <- performance(mdl_score, "tpr", "fpr")
  plot(perf,col=clr,add=ad)
}



# ***** LOAD LIBRARY ***** ----
library(MASS)
library(h2o)        # Neural Netwroks
library(cowplot)    # Plot Grid
library(ggplot2)
library(car)    # VIF
library(Hmisc)
library(ROCR)
library(caret)
library(caTools) # sample.split


# ***** LOAD DATA ***** ----

train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)

test <- read.csv("../input/test.csv",stringsAsFactors = FALSE)


str(train)
summary(train)

str(test)
summary(train)



# ***** Data Cleaning ***** ----

# Replace missing values with zero
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# make response as Character factor
#train$Outcome <- as.character(train$Outcome)
#train$Outcome[train$Outcome == 1] <- "YES"
#train$Outcome[train$Outcome == 0] <- "NO"


# save train and test
write.csv(train, "../intrim/train.csv", row.names = FALSE)
write.csv(test, "../intrim/test.csv", row.names = FALSE)


# ***** Logistic Regression ***** ----
train_lm <- train[,-c(1,2,3)]
mdl1 <- glm(Outcome~., train_lm, family = "binomial")

str(mdl1)

summary(mdl1)



# ***** H2O ***** ----

# . . . . H2O Init ----
h2o.init()

# . . . . Read Data ----
train_data <- h2o.importFile("../intrim/train.csv")

h2o.describe(train_data)


# split training data into train and validation
split <- h2o.splitFrame(train_data, 
                        ratios = c(0.8),                   # For N-parts, list of N-1
                        destination_frames = c("train", "vald"),
                        seed = 100
)

train <- split[[1]]
valid <- split[[2]]

# Load test data
test_data <- h2o.importFile("../intrim/test.csv")


# . . . . Model ----
nnet <- h2o.deeplearning(names(train[,-c(1,2,3,13)]),                # Explanatory Variables
                         names(train[,13]),                          # Target Variable
                         training_frame = train,                     # Training Data
                         validation_frame = valid,                   # Validation Data
                         activation = c("RectifierWithDropout"),     # Activation Function
                         hidden = c(10),                            # #of Hidden Layers/Neurons
                         hidden_dropout_ratios = c(0.1),
                         distribution = "bernoulli",                 # Binomial distribution
                         nfolds = 5,
                         l1 = 1e-5,
                         variable_importances = TRUE,
                         seed = 1,
                         reproducible = TRUE,
                         epochs = 5
)

# ***** Validation Predictions ***** ----
nnetpred <- h2o.predict(nnet, valid[,-c(1,2,3,13)])
h2o.confusionMatrix(nnet, valid)
nnetPerf<-h2o.performance(nnet, valid)
h2o.auc(nnetPerf)
plot(nnetPerf)


# ***** Test Prediction ***** ----
nnetpred <- h2o.predict(nnet, test_data[,-c(1,2,3)])

pred <-  h2o.cbind(test_data['ID'],nnetpred['predict'])
pred_df <- as.data.frame(pred)
pred_df$predict <- as.character(pred_df$predict)
pred_df$predict_n <- rep(0,length(pred_df))
pred_df$predict_n[pred_df$predict=='YES'] <- 1
pred_df <- pred_df[,-c(2)]
names(pred_df) <- c("ID","Outcome")

write.csv(pred_df, "../output/submission_atchirc.csv")



#h2o.shutdown()


