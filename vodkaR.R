## install required packages (and some more)
# install.packages(c("caret", "class","ggplot2","ggmap", "viridis","ggthemes","plotly"))

## if some packages wont load, run :
#install.packages("devtools")
#install_github("name of missing package")

library("caret")
library("class")
library("devtools")
library("ggplot2")
library("ggmap")
library("viridis")
library("ggthemes")
library("plotly")

#load map as dataframe
load("municipality.rda")

#load election data as df_raw, and copy to a dataframe called df 
df_raw = read.csv("dkelec.csv", header = FALSE, sep =";")
df = df_raw
#rename column names
colnames(df) = c("party", "location", "res_2007","res_2011","res_2015")


## print unique political parties in the dataset
#unique(df$party)[1:4]


#set proper variable classes for the dataframe
df$party = as.character(df$party)
df$location = as.character(df$location)
df$location = tolower(df$location)
df$res_2007 = as.numeric(df$res_2007)
df$res_2011 = as.numeric(df$res_2011)
df$res_2015 = as.numeric(df$res_2015)

#replace danish letters to match those in the map dataset
df$location = gsub("æ", "ae", df$location)
df$location = gsub("ø", "oe", df$location)
df$location = gsub("å", "aa", df$location)
df$location = gsub("-", " ", df$location)

#generate new variable with first letter of the variable "party" - i.e. A,V,O and replace "G" with "total"
df$letter = substr(df$party,1,1)
df$letter = ifelse(df$letter == "G", "total", df$letter)

#Add total so separate column
df$total_07 = df$res_2007[df$letter == "total"]
df$total_11 = df$res_2011[df$letter == "total"]
df$total_15 = df$res_2015[df$letter == "total"]

#calculate vote share in each municipality
df$pct_07 = df$res_2007 / df$total_07 * 100
df$pct_11 = df$res_2011 / df$total_11 * 100
df$pct_15 = df$res_2015 / df$total_15 * 100

#############################################################################################
### PLOTTING RESULTS ########################################################################
#############################################################################################

### Major parties ############
ggplot(data = subset(df, letter == "A" | letter == "V" | letter == "O")) +
  geom_map(aes(map_id = location, fill = pct_15), map = municipality) +
  expand_limits(x = municipality$long, y = municipality$lat) +
  scale_fill_viridis(option = "plasma", name = "Pct. of votes 2015") +
  coord_quickmap() +
  facet_wrap(~ letter, ncol = 2) +
  theme(legend.position = c(0.7, 0.3), plot.background = element_rect(fill = NULL)) +
  labs(x = NULL, y = NULL, title = "Share of valid votes 2015 (Major parties)" )

### Plotly point plot ################################################################
p = ggplot(data = subset(df,letter != "total" & letter != "Å")) +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1.5, alpha = 0.3) +
  geom_point(aes(x = pct_07, y=pct_15, color = letter, text = location), alpha = 1) +
  labs(x = "Percentage of votes 2007", y = "Percentage of votes 2015",
       title = "Winners and loosers since 2007") +
  scale_color_brewer(name = "Party") 

ggplotly(p)

#############################################################################################
### PREP FOR KNN ############################################################################
#############################################################################################

#subset df to remove party variable total
df_nototal_r = subset(df, letter != "total")

#index 20% of the dataset as 1, and the remaining as 2
p = 0.2
index = sample(2, nrow(df_nototal_r), replace = TRUE, prob = c(p, 1-p))

#generate test and train data based on index from above
df_train = df_nototal_r[index == 1, c(10,12)]
df_test = df_nototal_r[index == 2, c(10,12)]

# test and train variables with their labels (that is the y variable in y=f(x))
df_train_withlabels = df_nototal_r[index == 1, c(6,10,12)]
df_test_withlabels = df_nototal_r[index == 2, c(6,10,12)]

#test and train labels
df_trainlabels = df_nototal_r[index == 1,6]
df_testlabels = df_nototal_r[index == 2,6]

#add index vector to the dataframe
df_nototal = cbind(df_nototal_r, index)


# expand dataset to cover all variations of x and y. Used for background on the plot below 
nbp <- 250;
predA <- seq(min(df_nototal$pct_07), max(df_nototal$pct_07), length = nbp)
predC <- seq(min(df_nototal$pct_15), max(df_nototal$pct_15), length = nbp)

Grid <- expand.grid(pct_07 = predA, pct_15 = predC)

#############################################################################################
### MODELLING KNN ###########################################################################
#############################################################################################
# set control mechanism to repeated cross validation
V = 10
T = 4
TrControl = trainControl(method = "repeatedcv", number = V, repeats = T)


# train a k-NN model on the training data, and predict it on the grid
model = train(x = df_train, y = df_trainlabels, method = "knn", trControl = TrControl)
pred = predict(model, newdata = Grid)

#bind predictions to grid
predictions = cbind(Grid, letter = pred)

# Plot results
ggplot(data = df_nototal, aes(x = pct_07, y = pct_15)) +
  geom_tile(data = predictions, aes(fill = letter), alpha = 0.3) +
  geom_point(aes(color = letter)) +
  geom_point(data = df_train_withlabels, aes(x = pct_07, y = pct_15),
             color = "black", shape = 1, size = 3.2) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Percentage of votes 2007", y = "Percentage of votes 2015", title = "k-NN decision regions" )