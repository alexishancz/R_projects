## R_Challenge_vector_matrix_array_coordinate_distances.R
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
## (1) Generate a 1-D vector x of arbitrary length.
#I interpret "arbitrary" as: generate it however you wish. 

AlexisVec <- c(4, 9, 6, 7, 3, 3) #used c() function to generate a vector, filled it with values of my choice
AlexisVec
## (2) Choose a position in the vector (coordinate).

AlexisVec <- c(4, 9, 6, 7, 3, 3)
position_2 <- AlexisVec[2] #used indexing to select the second coordinate position in the vecto. saved it in an object "position_2"
position_2

## (3) Generate a new vector with the distances from the chosen position in the vector to every other element in the vector. The distance formula we will use is sqrt((position_vector - coordinate)^2).

distance_vector <- sqrt((AlexisVec - position_2)^2) #applied distance formula to my vector, stored the calculations in distance_vector object 
distance_vector


## (4) Repeat steps 1-3 for a 2-D matrix xy with dimensions x and y. The distance between two cells in a matrix with coordinates (x1, y1) and (x2, y2) is the square root of the squared distances (hint use the sqrt() function): sqrt((x1 - x2)^2 + (y1 - y2)^2).

#(1)
AlexisMat <- matrix(c(4, 9, 6, 7, 3, 3), nrow = 2, ncol = 3, byrow = TRUE) #used matrix function to generate a matrix, filled it with values of my choice, specified columns and rows 
AlexisMat

#(2)
Alexiscoord <- AlexisMat[1,3] #selected the coordinate position 1,3 to store in a new object Alexiscoord
Alexiscoord

#(3) #generated a new matrix with the distances from the chosen position in the matrix to every other element in the matrix.
chosen_x <- 1 #assigns the coordinate 1 to the variable chosen_x
chosen_y <- 3 #assigns the coordinate 3 to the variable chosen_y
distance_matrix <- matrix(0, nrow = nrow(AlexisMat), ncol = ncol(AlexisMat)) #initializes distance matrix to store the results of the following for loops 
for(x in 1:nrow(AlexisMat)){ #first for loop iterates through each row in AlexisMat using the variable x to keep track of the current row
  for(y in 1:ncol(AlexisMat)){ #second for loop iterates through each column in AlexisMat using the variable y to keep track of the current row
    distance_matrix[x,y] <- sqrt((x - chosen_x)^2 + (y - chosen_y)^2) #calculates the distance between the point (x, y) and the chosen point (chosen_x, chosen_y). The result is then assigned to the corresponding entry in distance_matrix.
  }
}
distance_matrix

## (5) Generalize (4) for an array of 3 dimensions. The distance between two cells in a 3-D array with coordinates (x1, y1, z1) and (x2, y2, z2) is the square root of the squared distances: sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2).

#(1)
AlexisArr <- array(c(4, 9, 6, 7, 3, 3, 1, 2, 3, 4, 5, 6), dim = c(2, 3, 2)) 
#created an array using aray() function. Using c() function, fills it with a vector of values. dimensions specififed using dim() argument 
AlexisArr

#(2)
Alexiscoord <- AlexisArr[1,3,1] #value at index [1, 3, 1] is assigned to Alexiscoord
print(Alexiscoord)

#(3) 
chosen_x <- 1 #assigned value of 1
chosen_y <- 3 #assigned value of 3
chosen_z <- 1 #assigned value of 1
distance_array <- array(0, dim = dim(AlexisArr)) #initializes distance array to store the results of the following for loops 
for(x in 1:dim(AlexisArr)[1]){ #iterates through first dimenstion
  for(y in 1:dim(AlexisArr)[2]){ #iterates through second dimenstion
    for(z in 1:dim(AlexisArr)[3]){ #iterates through third dimenstion
      distance_array[x,y,z] <- sqrt((x - chosen_x)^2 + (y - chosen_y)^2 + (z - chosen_z)^2) #performing the distance calculation on each element 
    }
  }
}
distance_array #print result

## (6) Write a function for (1), (4), and (5).
#I interpreted this as: write three separate functions for step 1, step 4, and step 5. 

#for step 1: 
generate_vector <- function(length) {
  random_vector <- sample(1:length, length, replace = TRUE) #generate vector of a length to be specified, with randomly sampled elements
  return(random_vector)  
}

#call the function and argue for a randomly generated vector of length 6. save the output in a vector called random_vector 
random_vector <- generate_vector(6)

#print output 
cat(random_vector)

#for step 4: 
calculate_distance_matrix <- function(matrix, chosen_x, chosen_y){ #creates a function calculate_distance_matrix that takes three arguments 
  distance_matrix <- matrix(0, nrow = nrow(matrix), ncol = ncol(matrix)) #initialize distance matrix
  for(x in 1:nrow(matrix)){ #iterates over the rows
    for(y in 1:ncol(matrix)){ #iterates over the columns
      distance_matrix[x,y] <- sqrt((x - chosen_x)^2 + (y - chosen_y)^2) #calculate distance
    }
  }
  return(distance_matrix) #return distance matrix
}

#call and apply the function: 
AlexisMat <- matrix(c(4, 9, 6, 7, 3, 3), nrow = 2, ncol = 3, byrow = TRUE)
calculate_distance_matrix(AlexisMat, 1, 3)

#for step 5: 

calculate_distances <- function(data, chosen_x, chosen_y, chosen_z) { #define a function called "calculate_distances" that takes four arguments: "data", "chosen_x", "chosen_y", and "chosen_z"
  if (is.vector(data)) { #check if input is a vector
    distances <- sqrt((data - data[chosen_x])^2) #if it is a vector, calculate the distances between each element in the vector and the chosen point
    return(distances)
  } else if (is.matrix(data)) { #check if input is a matrix 
    distances <- array(0, dim = dim(data)) #initialize a new matrix called "distances" and fill it with zeros
    for(x in 1:nrow(data)) { #iterate over each row in the matrix 
      for(y in 1:ncol(data)) { #iterate over each column in the matrix 
        distances[x,y] <- sqrt((x - chosen_x)^2 + (y - chosen_y)^2) #perform distance calculation
      }
    }
    return(distances)
  } else if (is.array(data)) { #check if input is an array
    distances <- array(0, dim = dim(data)) #initialize a new array called "distances" and fill it with zeros
    for(x in 1:dim(data)[1]) { #iterate over first dimension
      for(y in 1:dim(data)[2]) { #iterate over second dimension
        for(z in 1:dim(data)[3]) { #iterate over third dimension
          distances[x,y,z] <- sqrt((x - chosen_x)^2 + (y - chosen_y)^2 + (z - chosen_z)^2) #perform distance calculation
        }
      }
    }
    return(distances)
  } else {
    stop("error") #if input is not an array
  }
}
#call the function: 
a <- array(1:27, dim = c(3,3,3)) #create a 3-dimensional array called "a" with dimensions 3x3x3, filled with values 1 through 27
distances <- calculate_distances(a, 1, 2, 3) #call the function on the aarray "a" 
a
## (7) Write a function to generalize this process for an array of any dimensions.
distance_from_position <- function(array, chosen_coordinates) { #defines the function distance_from_position, takes two arguments: array and chosen_coordinates
  dims <- dim(array) #calculates the dimensions of the array and stores them in the variable dims
  distance_array <- array(dim = dims) #initializes an array to 0 stores it in the variable distance_array
  for(i in 1:dims[1]) { #162 and 163 iterate through every position in the array. i = row index and j = column index.
    for(j in 1:dims[2]) {
      if(length(dims)>2){ #164 and 165: if array has 3 dimensions, iterate through every position in the array. k = 3rd dimension
        for(k in 1:dims[3]){ 
          distance_array[i,j,k] <- sqrt(sum((c(i,j,k) - chosen_coordinates)^2)) #performs distance calculation on an array of 3 dimensions 
        }
      }
      else{
        distance_array[i,j] <- sqrt(sum((c(i,j) - chosen_coordinates)^2)) #if array has 2 dimensions, performs distance calculation
      }
    }
  }
  return(distance_array)
}

#applying the function:
#3D array with dimensions 2x3x4
my_array <- array(data = rnorm(24), dim = c(2, 3, 4))

#choose coordinates
my_coords <- c(1, 2, 3)

#call the function and assign output to object "my_distances"
my_distances <- distance_from_position(my_array, my_coords)

#print output 
print(my_distances)

##########################################################################

