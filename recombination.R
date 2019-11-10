#' crossOverAt
#' 
#' Helper function for the crossover operator.
#' It performs crossover recursively with a given robot and an infill child at a given position (ind)
#'
#' @param robot Parent Robot
#' @param infill The part to be inserted into the parent robot. (infill is type robot)
#' @param ind integer, id where to insert the infill part
#' @param count counter for recursive search of the robot id
crossOverAt <- function(robot, infill, ind, count = 0){
    count <- count + 1
    if(count == ind){
        return(list("robot" = infill,"count" = count))
    }else{
        if(length(robot$children)>0){
            for(i in 1:length(robot$children)){
                res <- crossOverAt(robot$children[[i]], infill, ind, count)
                robot$children[[i]] <- res$robot
                count <- res$count
            }
        }
    }
    return(list("robot" = robot,"count" = count))
}

#' crossOver
#'
#' 1 point tree crossover with 2 parents.
#' Samples a random position in both parents. The part of parent2 at the given id will 
#' be cut and implanted into parent1
#'
#' @param parent1 A Robot
#' @param parent2 A Robot
crossOver <- function(parent1, parent2){
    ## Count robot parts
    partCount1 <- getPartCount(parent1)
    partCount2 <- getPartCount(parent2)
    
    ## If the robot is too small, then crossover cant be done
    if(partCount1 < 2 || partCount2 < 2){
        return(parent1)
    }
    
    ## Sample random positions in both robots.
    ## Crossover will be applied at these positions in the robot tree
    chosenInd1 <- sample(2:partCount1,1)
    chosenInd2 <- sample(2:partCount2,1)
    
    infill <- getRobotFromID(parent2,chosenInd2)$robot
    
    ## Create the new robot and check for its validity
    newRobot <- crossOverAt(parent1,infill, chosenInd1)$robot
    if(!isValid(newRobot)){
        print("Crossover generated invalid Robot")
        return(NULL)
    }
    return(newRobot)
}