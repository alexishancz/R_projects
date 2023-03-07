## R_Challenge_vector_sum_and_more_sums.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
##
## COURSE NAME: Data Science for International Studies (DSIS)
## University of Michigan, Winter 2023, Winter 2022, Winter 2021, Winter 2020
##
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022, 2021, 2020
##
## Date: 2023-01-20
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Instructions:
##
## These challenges are meant to be just that, challenging. They should also be fun. I encourage you to think creatively and collaboratively. Getting stuck or not finishing all the steps is expected and encouraged. This is how learning works.
##
## Always start with step (1) and then continue to each step as time permits.
## Don't worry about completing each step. Document your code for each step.
## You may wish to come back to some of the harder steps as you progress through the course.
## Note that some of the steps may ask you to use skills we have not yet covered in the course.
## Don't worry about these steps now but definitely think through the programming logic if you are stuck and make plans to come back to try them once you feel ready.
##
##########################################################################
##
## Steps for the Challenge
##
## (1a) Create a numeric vector (let's call the object we create x or vec) of a length greater than 2 and less than or equal to 5 (i.e., 2 < n <= 5).
  
  #I decided to use the sample() function and pass the argument size=5. 

## (1b) Fill the vector with random numeric variables that are integers (not floating-point numbers so no decimal points).

  #I decided to use the sample() function and pass the argument 1:10. 
  #I know that the colon assignment operator always goes by 1 unit, so it will give me intergers.

## (1c) Select the integers arbitrarily (programmer's choice) or use the sample() function to randomly select numbers (probabilistic choice or nature's choice).

  #(1a) through (1c): I created a numeric vector "vec" of length 5 and filled it with random integers between 1 and 10 using the sample() function. 
  #I know that replace = TRUE means that in the sampling, intergers could be selected more than once. 

  vec <- sample(1:10, size=5, replace = TRUE)
  vec

## (1d) Are any of the numeric values in the vector equal to the coordinate position at which they reside in the vector? If so how many?

  #Yes. From just looking, the value 1 is equal to its position 1 in the vector. 
  #I also created a logical test below that returns TRUE when the value is equal to its position in the vector.
  vec == 1:length(vec)


## (2) Calculate the sum of all the numbers in the vector using the sum() function.

  #I calculated the sum and saved it as an object s
  sum_vec <- sum(vec)
  sum_vec

## (3a) Calculate the sum of all the numbers in the vector except for the value at the first coordinate position.

  #I used indexing ([]) to exclude the first element of the vector and saved it as an object
  sum_vec_1 <- sum(vec[-1])
  sum_vec_1

## (3b) Calculate the sum of all the numbers in the vector except for the value at the second coordinate position.

  #I used indexing ([]) to exclude the second element of the vector and saved it as an object
  sum_vec_2 <- sum(vec[-2])
  sum_vec_2

## (3c) Continue to calculate the sum of all the numbers in the vector except for value at the i_th coordinate position.

  #sum excluding third element 
  sum_vec_3 <- sum(vec[-3])
  sum_vec_3


  #sum excluding fourth element 
  sum_vec_4 <- sum(vec[-4])
  sum_vec_4


  #sum excluding fifth element 
  sum_vec_5 <- sum(vec[-5])
  sum_vec_5

  #Alternatively, I could assign an object "i" a value and plug in the object instead 
    i <- 1
    sum(vec[-i])

## (3d) Continue to calculate the sum of all the numbers in the vector except for the value at the i_th position (the i_th position is any coordinate position between 1 and n) until you reach the n_th coordinate position (the n_th coordinate position is the last coordinate position in the vector).

  #I interpreted these instructions as: Do the same thing as in 3c, but write a for-loop that iterates it. 
  #I created a for-loop that calculates the sum of each element in vec, excluding the ith value.
  #I set n to the length of vec and assigned the output to a vector called result.
  #In the for loop, the current element is excluded by indexing [-i] and the result is stored in the corres. position
  vec 
  n <- 5
result <- numeric(n)

for (i in 1:5) {
  result[i] <- sum(vec[-i])
}

result

  #Print the result as a vector
result

## (4) Write a function to generalize all of the steps (1) - (3) so that it can take a numeric vector of integers of any length and return a vector with the sums of all the sub-vectors that exclude the i_th value.

  #I interpreted these instructions as: Do the same thing as in 3d, but write a function using my previous for-loop. 
  #I did the same thing as in 3d but encapsulated the for-loop into a function:  
vec                                     #calling the original vector 
sum_excluding_ith <- function(vec) {    #function "sum_excluding_ith" takes a vector as an argument.
  n <- length(vec)                      #n is set to the length of the input vector.
  result <- numeric(n)                  #readies an empty vector "result" to eventually hold the output
  
  for (i in 1:5) {                      #this loop calculates the length of each sub-vector excluding ith and stores it in result.
    result[i] <- sum(vec[-i])              #iterates over all possible subvectors excluding i. 
  }
  
  return(result)                        #returns the result vector after the iterations are complete
}

result <- sum_excluding_ith(vec)        #stores the whole operation in result vector 

  
result                                  #print to see the new vector 

## (5) Pick an integer (arbitrarily or randomly) check to see if any of the sums that you calculated in step (3) or using the function in step (4) are equal to the integer you selected in step (5). Note that we will refer to the number selected as the "target" number.
  
  #Using sample function to pick the target number
target_number <- sample(1:100, size = 1)
target_number 

  #Using the any() function with a logical argument to check the target number against any of the sums I calculated in steps 3 and 4
any(target_number == result)

## (6) If the answer to the question in step (5) is FALSE, are there any smaller subsets of the vector (remove 2 or more numbers) that sum to the same number as the target number you selected in step (5)?

  #I wrote a function that checks if there is a subset of my "result" vector, excluding any two elements, that sums up to my target number and returns TRUE if there is and FALSE if there is not: 
check_sum <- function(result) {              #function "check_sum" takes a vector as an argument.
  n <- length(result)                        #n is set to the length of the input vector.
  result <- numeric(n)                       #readies an empty vector "result" to eventually hold the output
  for (i in 1:n) {                           #creates 2 nested loops                                          
    for (j in 1:n) {                             #iterates over all possible subvectors excluding i and j
      if (i != j) {                              #checks if i and j are not equal so we do not include the same number twice 
        subset_sum <- sum(result[-c(i,j)])   #stores the sum of the subvectors with our exclusions in an object subset_sum
        if (subset_sum == target_number) {   #checks if the sum of the subvector is equal to the target number 
          return(TRUE)                           #returns true 
        }
      }
    }
  }
  return(FALSE)                                 #or returns false 
}

result                                          #prints the new vector of sub-vectors 

## (7) If no subset of the vector sums to the target number as the one selected in step (5), calculate the number that is closest to (smallest distance) to the target number selected in step (5).

find_closest <- function(result, target_number) {  #telling a function find_closet to take two arguments 
  n <- length(result)                              #sets n to the length of the result vector
  closest <- result[1]                             #initializes "closest" object to the first coordinate position of result vector 
  for (i in 2:n) {                                 #iterates through the rest of the coordinate positions of the result vector 
    if (abs(result[i] - target_number < abs(closest - target_number)) {    #updates "closest" object if: abs diff between the current element and target_number < abs diff between the current "closest" and target_number.
      closest <- result[i]
    }
  }
  return(closest)                                   #returns closest value
}
closest 
result 
target_number

## (8) Write a function to generalize steps (5) - (7).

result
random.target <- sample(1:100, size = 1) #step 5
random.target
if (check_sum(result)) {                        #step 6: "if" statement that calls the check_sum function to check if any subset of the result vector adds up to the random.target number
  # subset found
  # do something
} else {                                        #step 7: if not, "else" statement calls the find_closest function and assigns the result to the closest variable.
  # find closest number
  closest <- find_closest(result, random.target)
  
}

closest

## (9a) Using the mean() function, calculate/estimate (later in the course we will make a distinction between calculate and estimate) the number that is closest to the average value in the original vector created in step (1) or the function in step (4).

vec 
mean_vec <- mean(vec) #calculate the mean of the vector and store the result in mean_vec
diff_vec <- abs(vec - mean_vec) #calculate the absolute difference between each element of vec and the mean value and store in diff_vec
min_diff <- min(diff_vec) #find the minimum value in 'diff_vec' and store it in 'min_diff'
closest_nums <- vec[diff_vec == min_diff] #select the elements that have an absolute difference from the mean equal to 'min_diff', and store the result in 'closest_nums'

closest_nums

## (9b) Using the mean() function, calculate/estimate the number that is closest to the average of the sub-vectors generated in step (3) or the function in step (4). What is this number? How close is it to each of the averages of the sub-vectors and the average of the entire vector?

average <- mean(result) #calculates mean of subvectors in "result" vector. number is 24
average
index <- which.min(abs(result - average)) #finds the element in result vector that is closest to the abs diff between result vector and avg
index
closest <- result[index]                  #saves calculation in an object "closest" 
closest # closest is 23

# Calculate the distances between the closest number and each of the sub-vector averages.
subvector_averages <- numeric(length(result))  #creates an empty vector to store the averages 
for (i in 1:length(result)) {                  #for loop that iterates through the length of result vector.
  subvector_averages[i] <- mean(result[-i])    #calculates the mean of each sub-vector excluding i; stores the calculations in the subvector_averages vector.
}
distances <- abs(subvector_averages - closest) #gives the distances between the closest number and each of the sub-vector averages. stores in distances vector. 

distances

## (10) Summarize and visualize the numbers calculated/estimated in step (9a) and (9b). Use the mean() and sd() functions to summarize (sd is short for standard deviation). Use the truehist() function from the MASS library to visualize. Congratulations, you have programmed the Jackknife estimator for a mean/average. Jackknife is an old analogy and a resampling technique used in statistics and data science. We will talk about resampling tools and what they are used for later in the course.


  #9a: 
mean_closest <- mean(closest_nums)  #mean of the number that is closest to the average value in the original vector, stored in object 
mean_closest
sd_closest <- sd(closest_nums) #SD of the number that is closest to the average value in the original vector, stored in object
sd_closest

  #9b: 
mean_distances <- mean(distances) # mean of the distances between the closest number and each of the sub-vector averages, stored in an object  
mean_distances
sd_distances <- sd(distances)  # SD of the distances between the closest number and each of the sub-vector averages, stored in an object 
sd_distances 

install.packages("MASS") #install package for histogram visualizations 
library(MASS)

Histogram.Closest <- truehist(closest_nums, main = "Histogram of Closest Numbers") #"Histogram of Closest Numbers"
Histogram.Closest
Histogram.Distances <- truehist(distances, main = "Histogram of Distances")  #"Histogram of Distances"
Histogram.Distances
##########################################################################




