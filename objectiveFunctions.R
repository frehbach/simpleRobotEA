objectiveLongRobot <- function(robot){
    df <- robotToDF(robot)
    
    if(nrow(df) == 1){
        return(0)
    }
    
    ## Create a long slim robot
    robotLen <- max(dist(df[,1:2]))
    robotPieces <- nrow(df)
    
    return(-(robotLen-(0.1*robotPieces)))
}

objectiveMaximumPiecesIntoLength10 <- function(robot){
    df <- robotToDF(robot)
    
    if(nrow(df) == 1){
        return(0)
    }
    robotLen <- max(dist(df[,1:2]))
    robotPieces <- nrow(df)
    
    if(robotLen > 10){
        return(999)
    }else{
        return(-robotPieces)
    }
}