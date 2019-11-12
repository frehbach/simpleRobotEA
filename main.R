source("ea.R")

POSSIBLE_TYPES <- 2:3
POSSIBLE_DOCK_POINTS <- 1:4
POSSIBLE_ORIENTATIONS <- c(-90,-45,0,45,90)

r <- createRandomRobot()
print(r)
plot(r)

createAndPlotRandom(15)

## Objective: Create a long slim robot
robot <- robotEA(objectiveLongRobot,100,10,3)

## Objective most pieces into limited space
robot <- robotEA(objectiveMaximumPiecesIntoLengthN(10),1000,100,3)

## Same with more budget and without Stops:
robot <- robotEA(objectiveLongRobot,100,0,3)
robot <- robotEA(objectiveMaximumPiecesIntoLengthN(10),2000,0,3)
robot <- robotEA(objectiveMaximumPiecesIntoLengthN(20),3000,0,3)
robot <- robotEA(objectiveMaximumPiecesIntoLengthN(20),5000,0,3)
