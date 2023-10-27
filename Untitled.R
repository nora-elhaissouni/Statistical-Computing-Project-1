install.packages("verification")
install.packages("starter")
library(starter)
library(verification)

## Question 4 part c
x <- c(0,0,3,9,16,18)
y<- c(2,1,4,5,3,1)
data<- data.frame(x,y)
sensy <- c(0,0,0,0,3/16,1/16,0,0)
one_min_speci<- c(0,0,0,0,16/46, 18/46, 0, 0)
data_2 <- data.frame(sensy, one_min_speci)
sensy_x<- data %>% mutate(x[1:4] = x/16)
plot(min_specy, Sensy)

PPV
PPV<- c(1,1,4/7,5/14)
prevalence<-

Sensy<- c(y[1:4]/(16), 0, 0)
min_specy<- c(x[1:4]/(16+18+12), 0, 0)
data_2 <- data.frame(Sensy, min_specy)
data_2 %>% ggplot(aes(
  x = min_specy,
  y = Sensy
)) + geom_point()+geom_smooth()

plot(min_specy, Sensy)


ggplot(aes(x = min_specy, y = specy))+geom_smooth()


## Textbook

# calculate sensitive and specificity for each cut off and then manually plot the curve
# Find TP, TN, FP, FN and use these to calculate each specicity

# Page Number
# 215-217


## Key Question
# 2 for loops
# smaller is used to simulate each drawing of the keys
# if key = val assigned to the door
# use break to stop the loop
# if it is not then the part without replacement, then remove that value from the keys vector (this is a smaller loop that is placed in a bigger loop that simulates the total trials)
### each loop represents a success so we eventually we record the amount in the small loop and the amount in the little loop and calculate the expected value


# Or you can use the while loop

## Question 5 part c
N = 10000
set.seed(seed = 5)
x = sample(1:1000, replace = FALSE)
i<-1
key_prob =
while (i == length(x)){
  key_prob = 1/x
  sum_key_prob = sum(keu_prob[i])
}
key_prob
sum_key_prob



#Set the seed for fully reproducible simulation results
set.seed(234901)
#Set the number of independent random samples to 100000
nsim=100000
150CHAPTER 5. RANDOM VECTORS, INDEPENDENCE, COVARIANCE, AND SAMPLE MEAN
#Simulate independently two random variables from a N(0,1)
x1=rnorm(nsi










###ROC Curve

install.packages("pROC")
library(pROC)
# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
# Load the required library
library(ggplot2)

# Create the data
x <- c(0, 0, 3, 9, 16, 18)
y <- c(2, 1, 4, 5, 3, 1)
combined <- data.frame(x = x, y = y)

# Initialize vectors to store sensitivity and specificity values
sensitivity <- numeric(nrow(combined))
one_minus_specificity <- numeric(nrow(combined))

# Calculate sensitivity and 1-specificity for each row as a cutoff
for (i in 1:nrow(combined)) {
  threshold_row <- combined[i, ]
  threshold <- i  # Use the row number as the cutoff index

  # Calculate True Positives (TP), False Negatives (FN), True Negatives (TN), and False Positives (FP)
  TP <- sum(y[threshold:nrow(combined)])
  FN <- sum(y[1:(threshold - 1)])
  TN <- sum(x[1:(threshold - 1)])
  FP <- sum(x[threshold:nrow(combined)])

  # Calculate sensitivity and 1-specificity, handling division by zero
  sensitivity[i] <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  one_minus_specificity[i] <- ifelse((TN + FP) == 0, 0, 1 - (TN / (TN + FP)))
}

# Create a data frame for the ROC-like curve
roc_data <- data.frame(Sensitivity = sensitivity, OneMinusSpecificity = one_minus_specificity, Threshold = 1:nrow(combined))

# Create a smoothed ROC-like plot using ggplot2
ggplot(roc_data, aes(x = OneMinusSpecificity, y = Sensitivity)) +
  geom_line(color = 'blue', size = 2) +
  geom_text(aes(label = Threshold), hjust = 0, vjust = 0, nudge_x = 0.02) +
  labs(
    title = "Smoothed Sensitivity vs. 1 - Specificity for Different Cutoffs",
    x = "1 - Specificity",
    y = "Sensitivity"
  )
