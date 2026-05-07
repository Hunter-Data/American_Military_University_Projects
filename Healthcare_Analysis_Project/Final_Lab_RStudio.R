
library(tidyverse)
library(readr)
library(corrplot)

Lab_Final_Data_Joined <- read_csv("~/AMU/Dats200/Week_8/Lab_Final_Data_Joined.csv")
View(Lab_Final_Data_Joined)

# Data Exploration
summary(Lab_Final_Data_Joined)
str(Lab_Final_Data_Joined)
glimpse(Lab_Final_Data_Joined)
unique(Lab_Final_Data_Joined$PatientID)
Lab_Final_Data_Joined %>%
  distinct(PatientName, Age, .keep_all = FALSE) %>% 
  print(n = 98)


# Sorting a table into a spread sheet. 
# This is tidyverse syntax
# Age descending
Lab_Final_Data_Joined %>%
  select(Age) %>%
  count(Age)%>%
  arrange(desc(Age)) %>% 
  View()

# Count descending
Lab_Final_Data_Joined %>%
  select(Age) %>%
  count(Age)%>%
  arrange(desc(n)) %>% 
  View()

# Count descending
Lab_Final_Data_Joined %>%
  select(Specialization) %>%
  count(Specialization)%>%
  arrange(desc(n)) %>% 
  View()

# Count descending
Lab_Final_Data_Joined %>%
  select(ClinicID) %>%
  count(ClinicID)%>%
  arrange(desc(n)) %>%
  View()

# Count descending
Lab_Final_Data_Joined %>%
  select(PatientName) %>%
  count(PatientName)%>%
  arrange(desc(n)) %>% 
  head()

# Count descending
Lab_Final_Data_Joined %>%
  select(PatientName) %>%
  count(PatientName)%>%
  arrange(desc(n)) %>% 
  tail()

# Data Manipulation
# Create a new column with for age groups
Lab_Final_Data_Joined["Age_group"] = cut(Lab_Final_Data_Joined$Age,
                                         c(0, 10, 20, 30, 40, 50, 60, Inf),
                                         c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "Greater than 60"),
                                         include.lowest=TRUE)

# create a new column based off info in another column
Lab_Final_Data_Joined$LocationCode <- vector(length=length(Lab_Final_Data_Joined$Location))

# for loop
#creates a list and assigned values based on strings in original column
for (i in 1:length(Lab_Final_Data_Joined$Location)) { 
  
  if (Lab_Final_Data_Joined$Location[i] == "North")  {Lab_Final_Data_Joined$LocationCode[i] <- '1'}
  
  else if  (Lab_Final_Data_Joined$Location[i] == "South")  {Lab_Final_Data_Joined$LocationCode[i] <- '2'}
  
  else if  (Lab_Final_Data_Joined$Location[i] == "East")  {Lab_Final_Data_Joined$LocationCode[i] <- '3'}
  
  else  {Lab_Final_Data_Joined$LocationCode[i] <- '4'}
  
}

# create a new column based off info in another column
Lab_Final_Data_Joined$GenderCode <- vector(length=length(Lab_Final_Data_Joined$Gender))

# for loop
#creates a list and assigned values based on strings in original column
for (i in 1:length(Lab_Final_Data_Joined$Gender)) { 
  
  if (Lab_Final_Data_Joined$Gender[i] == "Female")  {Lab_Final_Data_Joined$GenderCode[i] <- '1'}
  
  else  {Lab_Final_Data_Joined$GenderCode[i] <- '2'}
  
}

# Verifying column unique values
unique(Lab_Final_Data_Joined$ReasonForVisit)

# create a new column based off info in another column
Lab_Final_Data_Joined$VisitCode <- vector(length=length(Lab_Final_Data_Joined$ReasonForVisit))

# for loop
#creates a list and assigned values based on strings in original column
for (i in 1:length(Lab_Final_Data_Joined$ReasonForVisit)) { 
  
  if (Lab_Final_Data_Joined$ReasonForVisit[i] == "Checkup")  {Lab_Final_Data_Joined$VisitCode[i] <- '1'}
  
  else if  (Lab_Final_Data_Joined$ReasonForVisit[i] == "Follow-up")  {Lab_Final_Data_Joined$VisitCode[i] <- '2'}
  
  else if  (Lab_Final_Data_Joined$ReasonForVisit[i] == "Emergency")  {Lab_Final_Data_Joined$VisitCode[i] <- '3'}
  
  else  {Lab_Final_Data_Joined$VisitCode[i] <- '4'}
}

# Verifying column unique values
unique(Lab_Final_Data_Joined$Specialization)

# create a new column based off info in another column
Lab_Final_Data_Joined$SpecializationCode <- vector(length=length(Lab_Final_Data_Joined$Specialization))

# for loop
#creates a list and assigned values based on strings in original column
for (i in 1:length(Lab_Final_Data_Joined$Specialization)) { 
  
  if (Lab_Final_Data_Joined$Specialization[i] == "Neurology")  {Lab_Final_Data_Joined$SpecializationCode[i] <- '1'}
  
  else if  (Lab_Final_Data_Joined$Specialization[i] == "General")  {Lab_Final_Data_Joined$SpecializationCode[i] <- '2'}
  
  else if  (Lab_Final_Data_Joined$Specialization[i] == "Cardiology")  {Lab_Final_Data_Joined$SpecializationCode[i] <- '3'}
  
  else if  (Lab_Final_Data_Joined$Specialization[i] == "Pediatrics")  {Lab_Final_Data_Joined$SpecializationCode[i] <- '4'}
  
  else  {Lab_Final_Data_Joined$SpecializationCode[i] <- '5'}
}

colnames(Lab_Final_Data_Joined)

# Mean, median, min, max
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, min)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, mean)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, median)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, max)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, sd)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Age_group, var)
tapply(Lab_Final_Data_Joined$Gender, Lab_Final_Data_Joined$Age_group, median)

tapply(Cor_test$SpecializationCode, Lab_Final_Data_Joined$Age_group, median)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Specialization, min)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Specialization, mean)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$Specialization, max)


tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$ReasonForVisit, min)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$ReasonForVisit, mean)
tapply(Lab_Final_Data_Joined$Duration, Lab_Final_Data_Joined$ReasonForVisit, max)

Cor_test <- Lab_Final_Data_Joined[,c(1, 4, 8, 9, 12, 13, 14, 15)]

summary(Cor_test)

# Count descending
Cor_test %>%
  select(SpecializationCode) %>%
  count(SpecializationCode)%>%
  arrange(desc(n)) %>% 
  View()

# Count descending
Cor_test %>%
  select(SpecializationCode) %>%
  count(SpecializationCode)%>%
  arrange(desc(n)) %>% 
  View()

# Count descending
Cor_test %>%
  select(SpecializationCode) %>%
  count(SpecializationCode)%>%
  arrange(desc(n)) %>% 
  View()


Cor_test$LocationCode <- as.numeric(as.character(Cor_test$LocationCode))
Cor_test$GenderCode <- as.numeric(as.character(Cor_test$GenderCode))
Cor_test$VisitCode <- as.numeric(as.character(Cor_test$VisitCode))
Cor_test$SpecializationCode <- as.numeric(as.character(Cor_test$SpecializationCode))
sapply(Cor_test, class)

#Visualizations
hist(Lab_Final_Data_Joined$PatientID, xlab = 'PatientID', ylab = 'Number of Visits', breaks = 22, main = 'Histogram of Patient visits by ID number', col = 'blue')

hist(unique(Lab_Final_Data_Joined$PatientID), xlab = 'PatientID', ylab = 'Number of Patients', breaks = 22, main = 'Histogram of Patient visits by ID number', col = 'blue')

hist(Lab_Final_Data_Joined$Age, xlab = 'Age', ylab = 'Number of Visits', breaks = 22, main = 'Histogram of Distribution by Age', col = 'blue')

hist(unique(Lab_Final_Data_Joined$Age), xlab = 'Age', ylab = 'Number of Patients', breaks = 22, main = 'Histogram of Distribution by Age', col = 'blue')


# ggplot(Lab_Final_Data_Joined, aes(Age, Duration, colour = Location))+
#   geom_point()+
#   facet_wrap(Lab_Final_Data_Joined$Age_group)
# 
# ggplot(Lab_Final_Data_Joined, aes(PatientID, Duration, colour = ReasonForVisit))+
#   geom_point()+
#   facet_wrap(Lab_Final_Data_Joined$Age_group)

ggplot(Lab_Final_Data_Joined, aes(Age_group, Duration, colour = Location))+
  geom_boxplot()+
  stat_summary(fun.y="mean",position = position_dodge2(width = 0.75))

# ggplot(Lab_Final_Data_Joined, aes(Location, Duration, colour = Location))+
#   geom_boxplot()

# ggplot(Lab_Final_Data_Joined, aes(Age, fill=ReasonForVisit)) +
#   geom_histogram(bins=30, color='black') +
#   labs(title = "Reason For Visit By Age",
#        x = "Age",
#        y = "Number of Visits")+
#   scale_fill_manual(values = c('Checkup' = 'red',
#                                'Follow-up' = 'blue',
#                                'Emergency' = 'purple',
#                                'Illness' = 'green'))

ggplot(Lab_Final_Data_Joined, aes(Age, colour = Gender))+
  geom_density()+
  geom_vline(aes(xintercept=mean(Age)),
            linetype="dashed", size=1)

# ggplot(Lab_Final_Data_Joined, aes(Age, fill=ReasonForVisit)) +
#   geom_histogram(bins=46, color='black') +
#   labs(title = "Reason For Visit by Age Group",
#        x = "Age",
#        y = "Number of Visits")+
#   facet_wrap(Lab_Final_Data_Joined$Age_group)+
#   scale_fill_manual(values = c('Checkup' = 'red',
#                                'Follow-up' = 'blue',
#                                'Emergency' = 'purple',
#                                'Illness' = 'green'))

# ggplot(Lab_Final_Data_Joined, aes(PatientID, fill=ReasonForVisit)) +
#   geom_histogram(bins=46, color='black') +
#   labs(title = "Reason for Visit Sorted by Age Group and Patient ID",
#        x = "Patient ID",
#        y = "Number of Visits")+
#   facet_wrap(Lab_Final_Data_Joined$Age_group)+
#   scale_fill_manual(values = c('Checkup' = 'red',
#                                'Follow-up' = 'blue',
#                                'Emergency' = 'purple',
#                                'Illness' = 'green'))

ggplot(Lab_Final_Data_Joined, aes(Age, fill=Specialization)) +
  geom_histogram(bins=98, color='black', binwidth = 0.15) +
  labs(title = "Clinic Specialty Visited by Age Group",
       x = "Age",
       y = "Number of Visits")+
  facet_wrap(Lab_Final_Data_Joined$Age_group, scales = "free_x")+
  scale_fill_manual(values = c('Neurology' = 'red',
                               'General' = 'blue',
                               'Cardiology' = 'purple',
                               'Pediatrics' = 'green',
                               'Orthopedics' = 'pink'))

ggplot(Lab_Final_Data_Joined, aes(VisitDate, fill=Specialization)) +
  geom_histogram(bins = 12, color='black') +
  labs(title = "Visits Over Time",
       x = "Date",
       y = "Number of Visits")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "months", date_labels = "%m-%Y")+
  scale_fill_manual(values = c('Neurology' = 'red',
                               'General' = 'blue',
                               'Cardiology' = 'purple',
                               'Pediatrics' = 'green',
                               'Orthopedics' = 'pink'))

corrplot(cor(Cor_test),
         method = "number",
         type = 'lower',
         order = 'FPC', 
         diag = FALSE,
         tl.col = "black",
         bg = "grey",
         tl.srt = 30)

# Hypothesis testing
# Duration by location
Lab_Final_Data_Joined %>% 
  filter(Location == "North") %>% 
  select(Duration) %>% 
  t.test(mu = 93.58)

Lab_Final_Data_Joined %>% 
  filter(Location == "South") %>% 
  select(Duration) %>% 
  t.test(mu = 93.58)

Lab_Final_Data_Joined %>% 
  filter(Location == "East") %>% 
  select(Duration) %>% 
  t.test(mu = 93.58)

Lab_Final_Data_Joined %>% 
  filter(Location == "West") %>% 
  select(Duration) %>% 
  t.test(mu = 93.58)
# no statistical difference with the above tests


# Age by location
Lab_Final_Data_Joined %>% 
  filter(Location == "North") %>% 
  select(Age) %>% 
  t.test(mu = 43.16)

Lab_Final_Data_Joined %>% 
  filter(Location == "South") %>% 
  select(Age) %>% 
  t.test(mu = 43.16)

Lab_Final_Data_Joined %>% 
  filter(Location == "East") %>% 
  select(Age) %>% 
  t.test(mu = 43.16)

Lab_Final_Data_Joined %>% 
  filter(Location == "West") %>% 
  select(Age) %>% 
  t.test(mu = 43.16)
# no statistical difference with the above tests


# Two sided testing
Lab_Final_Data_Joined %>% 
  filter(Location %in% c("North", "South")) %>% 
           t.test(Duration ~ Location, data = .,
                  alternative = "two.sided")

Lab_Final_Data_Joined %>% 
  filter(Location %in% c("North", "East")) %>% 
  t.test(Duration ~ Location, data = .,
         alternative = "two.sided")

Lab_Final_Data_Joined %>% 
  filter(Location %in% c("North", "West")) %>% 
  t.test(Duration ~ Location, data = .,
         alternative = "two.sided")

Lab_Final_Data_Joined %>% 
  filter(Location %in% c("South", "West")) %>% 
  t.test(Duration ~ Location, data = .,
         alternative = "two.sided")

Lab_Final_Data_Joined %>% 
  filter(Location %in% c("South", "East")) %>% 
  t.test(Duration ~ Location, data = .,
         alternative = "two.sided")

Lab_Final_Data_Joined %>% 
  filter(Location %in% c("West", "East")) %>% 
  t.test(Duration ~ Location, data = .,
         alternative = "two.sided")
# no statistical difference with the above two sided tests

Lab_Final_Data_Joined %>% 
  filter(Gender %in% c("Female", "Male")) %>% 
  t.test(Duration ~ Gender, data = .,
         alternative = "two.sided")
# no statistical difference with the above two sided test

# One-Sided test
Lab_Final_Data_Joined %>% 
  filter(Gender %in% c("Female", "Male")) %>% 
  t.test(Duration ~ Gender, data = .,
         alternative = "less",
         conf.level = 0.95)

Lab_Final_Data_Joined %>% 
  filter(Gender %in% c("Female", "Male")) %>% 
  t.test(Age ~ Gender, data = .,
         alternative = "less",
         conf.level = 0.95)
# no statistical difference with above one-sided tests



write.csv(Lab_Final_Data_Joined,file="C:/Users/hunte/OneDrive/Documents/AMU/Dats200/Week_8/Final_Data_R_Manipulated.csv")
?write.csv
