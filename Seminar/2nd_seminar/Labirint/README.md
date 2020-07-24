# Tasks
The purpose of the seminar paper is the implementation and use of investigative algorithms in the domain of search
paths in the maze. Each input maze contains one initial field and one or more target fields. We want to find
the cheapest possible route from the initial to one of the target fields, with no movement through the wall allowed
(fields marked -1). Positive values in the field represent the price of the passage through this field. Transitions
through the initial (code -2) and target field (code -3) have a price of 0.
Your task is to implement various investigative algorithms and test them on the path finding domain in
maze. You can implement the algorithms in any programming language. The program's 
input data is the labyrinth, which will be given in the form of a matrix, and as a result it should display:
- found route to the destination (in the form of ordered pairs of numbers representing the coordinates of the route),
- the price of the found route,
- number of movements on the found route,
- processing statistics (for example, when searching in depth, you can present the number of processed nodes
in the graph, the maximum depth examined; in the case of a genetic algorithm, however, you can present
number of treated generations, movement of average quality of specimens by individual generations and
similar).  
The implemented algorithms should be general, which means that they must work on all labyrinths (but not
necessarily to find the theoretical optimum at all inputs). 
