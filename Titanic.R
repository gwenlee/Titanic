#Data Science with Taitanic data



#Comment: Lets import the data
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)

ncol(train)
ncol(test)
#survived = 1 means they are survived
##comment: 'Survived' column is missing in train as it is a training dataset


#Dataframe is a type of list
#?Vector vs ?Matrix vs ?List vs ?Dataframe
#https://www.programcreek.com/2014/01/vector-array-list-and-data-frame-in-r/
#https://jamesmccaffrey.wordpress.com/2016/05/02/r-language-vectors-vs-arrays-vs-lists-vs-matrices-vs-data-frames/


#?rep vs ?seq
#?data.frame 
#?sample

##comment: I want to convert this into Dataframe 
test.survive <- data.frame(Survived=rep("None",nrow(test)),test[,])
ncol(test.survive)
##comment: Now the number of columns is the same


#combine data sets
#?rbind vs ?cbind
combine <- rbind(train, test.survive)

#?str vs ?class
str(combine)
##comment: this is a data frame type, 12 variables (columns) with more data rows

#?as.factor  (categorical) vs ?as.character
#?int vs ?num vs ?factor
##comment: we change the plcass from int into factor, because this is about the measurement of wealth (can be categorised)
combine$Pclass <- as.factor(combine$Pclass)

##comment: Rather than using as a string (Character) in 'Survived', it would be easy to assess data if we can classify them into categories
combine$Survived <- as.factor(combine$Survived)




###Variables 

#?table vs ?matrix
#https://www.cyclismo.org/tutorial/R/types.html
table(combine$Survived)
##comment: More people died overall. If Pclass was numerical variable, may have needed ?qqnorm to check skewness. The more skewed the data, the more machine learning be likely to pick the trending. 
##comment: None value is a large number, which would be determined by Machine Learning algorithm
table(combine$Pclass)
##comment: Most of people who died were 3rd class

##comment: Compare the survival rate by classes
table(combine$Pclass,combine$Survived)
##comment: What other variables might be able to affect the survival rate?

table(combine$Sex)
hist(combine$Age)




##Hypothesis## - we will explore the visualisation first, then we will test our hypothesis later.

library(ggplot2)
#?ggplot #fill= variable is optional
#?factor vs ?as.factor
str(train)
train$Survived <-as.factor(train$Survived)
ggplot(train, aes(x=Pclass, fill=Survived))+
  geom_bar(width=0.5)+ xlab("Pclass") + ylab("Count") + labs(fill="Survived")
##comment: Visualisation - it seems that our hypothesis is likely to be correct



###Name Variable - What's up with the title?

##comment: Getting rid of the duplicates (name)
#?unique
length(unique(combine$Name))
length(combine$Name)
#comment: there are two rows who are duplicates

#?duplicated --> Returns True or False (logical Vector)
#?which  --> Returns a the TRUE indices of a logical objects
##comment: Figuring out which are the duplicates
Titles <- combine$Name[which(duplicated(combine$Name))]
Titles

#%in%  --> checking if the Names are in the combine$Name columns
#     -->%in% operator in R, is used to identify if an element belongs to a vector.
combine[which(combine$Name %in% Titles),]
##comment: Executing the dupliactes in a row.
##        They do have the same name but their age is different. So they are all different individuals.
##        So there is no duplicates in this data.




###Name Variable -- Relationship in Mr. Mrs. Miss.
library(stringr)

class(Titles)


#?grep vs ?grepl vs ?gregexpr ? regexpr 
#?substring
#?paste
##comment: Creating a function that extracts titles. By looking at data, there are mainly 4 titles seem to exist.


ggplot(combine[1:891,], aes(x=SibSp,fill=Survived)) +
  facet_grid(~Pclass+Sex) +
  geom_histogram(stat="count")

extractTitle <- function(x){
    if(grepl("Mrs.", x)) {return("Mrs.")}
    else if(grepl("Miss.",x)) {return("Miss.")}
    else if(grepl("Mr.",x)) {return("Mr.")}
    else if(grepl("Master.",x)) {return("Master.")}
    else {return("Other")}
  }

##comment: We will add a new column 'Titles' according to each row
Titles <- NULL
for (i in 1:nrow(combine)){
  Titles <- c(Titles, extractTitle(combine[i,"Name"]))
}
##comment: Under the column "Name", we go through each row to extract the tile from their name and will implement its title inside the vector

combine$Title <- as.factor(Titles)
##comment: Add Titles into 'combine' data, 


###Visualise by Titles & Pclass

#?geom_bar vs geom_histogram
#?stat='identity'
#?facet_wrap - helps to categorise the chunk of data.
ggplot(combine[1:891,], aes(x=Title, y=Pclass, fill=Survived))+
  geom_bar(stat='identity')+
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill="Survived")
##comment:
#It seems that 3rd class Man are more likely to die based on the result of the training data.
#Other casualties were mostly from 3rd class as well.
#However, at the same time, lots of 3rd class Man were on the board. 
#So we are not sure yet whether actually 3rd class people were more likely to die.
#Although, it seems that males are more likely to die than females based on the graph.




###Visualise by Sex & Pclass
ggplot(combine[1:891,], aes(x=Sex, y=Pclass, fill=Survived))+
  geom_bar(stat='identity')+
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill="Survived")
#comment: It seems that Males are more likely to die. 




###Visualise by Age
summary(combine$Age)
##comment: Quite a few of NA's...
#How to deal with NA's...  https://www.r-bloggers.com/missing-value-treatment/
hist(combine$Age)
##comment: slightly right skewed? But in general, they seem to be normally distributed.(i.e. not so biased)

#?geom_histogram
ggplot(combine[1:891,], aes(x=Age, fill=Survived))+
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10)+
  ggtitle("Pclass with Gender") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")
##comment: Males are more likely to perish. 
##        The older, the more likely to perish, 
##        Relatively lower class women died more than higher classes but for males, you could not say big difference between classes.



#?which() vs ?filter()
###Experimenting - Does 'Master' means young males and 'Miss' means young females?
Master <- combine[which(combine$Title == "Master."),]
summary(Master$Age)
##comment: Max age is 14.5 ... It seems that our guess was right

Miss <- combine[which(combine$Title == "Miss."),]
summary(Miss$Age)
length(Miss$Age)
##comment: Max age is 63...50 is NA out of 258. At this stage, we will keep them as it is.

ggplot(Miss[Miss$Survived != "None",], aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass) + 
  geom_histogram(binwidth = 10)
##comment: People with Miss title are more likely to perish if they are in the 3rd class


###SibSp & Parch Variables 

summary(combine$SibSp)
ordered(unique(combine$SibSp))
##comment: Seems that most of them were by themselves or with their one company (sibiling/spouse), Maximum 8 of them
summary(combine$Parch)
ordered(unique(combine$Parch))
##comment: Seems that most of them were by themselves, rather than with their parents or children. The max was 9.

combine$SibSp <- as.factor(combine$SibSp)

#Visualising Sibsp & Parch
ggplot(combine[1:891,], aes(x=SibSp,fill=Survived)) +
  facet_grid(~Pclass+Sex) +
  geom_histogram(stat="count") +
  xlab("Siblings or Spouse") +
  ylab("Total Count")
##comment: It seems that you are more likely to perish, if you have more siblings or with parents in general.

ggplot(combine[1:891,], aes(x=Parch,fill=Survived)) +
  facet_grid(~Pclass+Sex) +
  geom_histogram(stat="count", binwidth = 10) +
  xlab("Parents or children") +
  ylab("Total Count")
##comment: It seems that if you were a male in the 2nd or 3rd class, it is more likely to perish. And the more children you may have, the less likely to survive.


###Total number of family size - The more you have family memebers with you on board, the less likely you will survive
combine$SibSp <- as.integer(combine$SibSp)
combine$Parch <- as.integer(combine$Parch)
combine$FamSize <- as.factor(combine$SibSp + combine$Parch + 1)


ggplot(combine[1:891,], aes(x=FamSize,fill=Survived)) +
  facet_grid(~Pclass+Sex) +
  geom_histogram(stat="count", binwidth = 10) +
  xlab("Family Size") +
  ylab("Total Count")
##comment: It seems that the visualisation confirms that what we assumed earlier. The more family you would have - the less likely you would survive. However, this trend might be less likely to 1st or 2nd class females.





###Ticket variable
combine$Ticket <- as.character(combine$Ticket)
##comment: change into string value as they all seem to be a unique value. We doubt if there is any relationship, but it would be worth if we can check

#?substring vs ?strsplit vs ?substr vs ?grep vs ?grepl vs ?gsub vs ?str_detect
FirstLetter <- ifelse(combine$Ticket == "", " ", substr(combine$Ticket, 1, 1))
unique(FirstLetter)

combine$FirstLetter <- FirstLetter

ggplot(combine[combine$Survived != "None",], aes(x=FirstLetter,fill=Survived )) +
  geom_bar() +
  ylab("Total Count")
##comment: Interesting, it seems that there were some significant values were shown, especially 1,2,3 and P & S. However, does 1,2,3 means Pclass? 

ggplot(combine[combine$Survived != "None",], aes(x=FirstLetter,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ylab("Total Count")
##comment: Maybe it is, it seems that the 1,2,3 numbers are more corresponding to the Plcass levels. 

ggplot(combine[combine$Survived != "None",], aes(x=FirstLetter,fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass+Title) +
  ylab("Total Count")
##comment: However, there are not many information regarding different ticket numbers, it only confirms what we had earlier with Pclass and Title







###Fare variable
summary(combine$Fare)
##comment: Median & Mean seem to be different, skewed data..The paid gap between the highest and the lowest seems to be extreme.

ggplot(combine[1:891,], aes(x=Fare)) +
  geom_histogram(bins = 200)

ggplot(combine[1:891,], aes(x=Fare,fill=Survived)) +
  geom_histogram(binwidth=5) +
  facet_wrap(~Pclass + Title)
##comment: Fare does not add much into our information...I would not replace Plcass with this as there are some skewed data which may impact to our conclusion.





###Cabin variable

str(combine$Cabin)
combine$Cabin[1:30]
##comment: Lots of empty factors... It seems to contain Alphabet + Numbers combination

CabinLetter <- substr(combine$Cabin,1,1)
##comment: Lets do what we have done for Ticket variable
combine$CabinLetter <- as.factor(CabinLetter)
levels(unique(combine$CabinLetter))
##comment: There are alphabets from A to T


ggplot(combine[1:891,], aes(x=CabinLetter, fill=Survived)) +
  geom_bar()

ggplot(combine[1:891,], aes(x=CabinLetter, fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass)
##comment: Most of alphabets were located to first class which makes sense as cabins would been allocated as a private room and those traits would be more affordable to first class crews. Probaly this is why 2nd or 3rd class have more empty values as they would not have cabins during shipping.

ggplot(combine[1:891,], aes(x=CabinLetter, fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass+Title)
##comment: Nothing interesting in here. 


##Crews with multiple cabins 

#?lapply vs ?sapply vs ?vapply vs ?rapply
#?gsub
CabinList <- str_split(combine$Cabin , " ")

IdentifyNA <- function(x){
  if (x == "") {return("0")}
  else {return(length(x))}
}
##comment: This function allows to identify any non-cabin as the number of zero.

MultipleCabin <- NULL
for (n in 1:length(CabinList)){
  MultipleCabin <- c(MultipleCabin, IdentifyNA(CabinList[[n]]))
}
##comment: counting how many cabins were for each passenger, the recursive function applied by each of object.

length(MultipleCabin)
combine$MultipleCabin <- as.factor(MultipleCabin)
##comment: MultipleCabin counts how many Cabins they have for each passenger. MultipleCabin was embedded as a new column

library(dplyr)

combine[1:891,] %>%
  ggplot(aes(x=MultipleCabin, fill=Survived)) +
  geom_bar()

combine[1:891,] %>%
  ggplot(aes(x=MultipleCabin, fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass)
##comment: Most of people who would have lots of cabins were more likely to be a higher class (i.e. 1st class)
##        You can see that the people who does not have cabin (i.e. MultipleCabin = 0) are rare in a higher class, whereas the number of MultipleCabin increases, is is more likely they are from a higher class

combine[1:891,] %>%
  filter(MultipleCabin != "1") %>%
  ggplot(aes(x=MultipleCabin, fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass+Title)
##comment: However, this variable does not seem to add any interesting information to predict the casualties.



