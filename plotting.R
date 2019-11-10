print.RobotPart <- function(x,tabIndex = 0,runningID = 0,...){
    runningID <- runningID + 1
    print(paste0(paste(rep("   ",tabIndex),collapse=""),"ID: ",runningID))
    print(paste0(paste(rep("   ",tabIndex),collapse=""),"Type: ",x$type))
    print(paste0(paste(rep("   ",tabIndex),collapse=""),"Orientation: ",x$orientation))
    print(paste0(paste(rep("   ",tabIndex),collapse=""),"Dock: ",x$dockPoint))
    if(length(x$children)>0){
        print(paste0(paste(rep("   ",tabIndex),collapse=""),"Children: "))
        tabIndex <- tabIndex + 1
    }
    for(child in x$children){
        runningID <- print(child, tabIndex = tabIndex, runningID)
    }
    return(runningID)
}

plot.RobotPart <- function(robot, fitness = NULL, withDocks = T){
    longData <- robotToDF(robot)
    
    ## Add coordinates to plot dock points
    ## 
    if(withDocks){
        getDockPoint <- function(df){
            x <- df[["x"]]
            y <- df[["y"]]
            if(df[["dockPoint"]] == 1) y <- y - 0.5
            if(df[["dockPoint"]] == 2) x <- x + 0.5
            if(df[["dockPoint"]] == 3) y <- y + 0.5
            if(df[["dockPoint"]] == 4) x <- x - 0.5
            data.frame(dockX = x, dockY = y)
        }
        longData <- cbind(longData, dplyr::bind_rows(apply(longData,1,getDockPoint)))
    }
    
    xRange <- c(min(longData$x)-1, max(longData$x)+1)
    yRange <- c(min(longData$y)-1, max(longData$y)+1)
    if((xRange[2]-xRange[1])>(yRange[2]-yRange[1])){
        yRange[2] <- yRange[2] + (xRange[2]-xRange[1])-(yRange[2]-yRange[1])
    }else{
        xRange[2] <- xRange[2] + (yRange[2]-yRange[1])-(xRange[2]-xRange[1])
    }
    
    p <- ggplot(longData, aes(x = x, y = y, label = type, fill = type)) + 
        geom_tile() +
        geom_text(col = "black") + 
        scale_fill_gradient(low="grey90", high="red") +
        labs(x="", y="", title="Robot Plot") +
        xlim(xRange) + ylim(yRange) + 
        theme_bw() + 
        theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
              axis.text.y=element_text(size=9),
              plot.title=element_text(size=11)) 
    if(withDocks){
        p <- p + geom_point(aes(x = dockX, y = dockY))
    }
    if(!is.null(fitness)){
        return(p + ggtitle(fitness))
    }else{
        return(p)
    }
}

plotMultiRobot <- function(robotList){
    plotList <- list()
    for(r in robotList){
        plotList[[length(plotList) + 1]] <- plot(r)
    }
    do.call("grid.arrange", c(plotList, nrow = 1))
}