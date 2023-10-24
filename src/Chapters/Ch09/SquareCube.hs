module SquareCube where


mySqr = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

combi = [(x,y) | x<-mySqr, y<-myCube]

combiTwo = [(x,y) | x<-mySqr, y<-myCube, x<50, y<50]

combiThree = sum [1 | x<-mySqr, y<-myCube, x<50, y<50]