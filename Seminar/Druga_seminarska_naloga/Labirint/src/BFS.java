import java.util.*;

public class BFS extends Solver {
    /*
     * Constructor
     * m: Maze to solve
     */
    public BFS(Maze m) {
        this.maze = m;
        this.workingPath = new LinkedList<Node<Maze>>();
        this.closedSquares = new LinkedList<Square>();
    }

    public boolean solve() {
        boolean endfound = false;
        this.nodesCounter = 0;

        this.workingPath.add(new Node<Maze>(this.maze)); //Add initial state

        //Measure run time
        long startTime = System.currentTimeMillis();


        //Search
        while (!endfound) {
            if (this.workingPath.isEmpty()) //Check if the frontier is empty
                break;

            else {
                Node<Maze> currentNode = ((LinkedList<Node<Maze>>) this.workingPath).removeFirst(); //Get first node from the frontier
                this.maze = (Maze) currentNode.getContent(); //Get maze from the node
                Square currentSquare = this.maze.getCurrentSquare(); //Get currentNode state from the maze

//				if(currentSquare.getLine() == this.maze.getEndSquares().getLine() && currentSquare.getCol() == this.maze.getEndSquares().getCol())
//				if(this.maze.getEndSquares().contains(currentSquare))
                if (currentSquare.checkEnd(this.maze.getEndSquares())) {
                    Node<Maze> temp = new Node<Maze>(this.maze);
                    temp.setFather(currentNode); //Set currentNode as father for all next states
                    this.workingPath.add(temp);
                    endfound = true;
                } else {
                    LinkedList<Node<Maze>> nextSquares = this.getNextSquares(); //Get next possible states
                    if (!this.closedSquares.contains(currentSquare)) {
                        this.closedSquares.add(currentSquare);
                    }

                    Iterator<Node<Maze>> iterator = nextSquares.iterator();

                    //Set fathers
                    while (iterator.hasNext()) {
                        Node<Maze> temp = iterator.next();
                        temp.setFather(currentNode); //Set currentNode as father for all next states
                        this.workingPath.add(temp);
                        this.nodesCounter++;
                    }
                }

            }
        }

        long endTime = System.currentTimeMillis();

        long time = endTime - startTime;


        if (endfound) {
            Node<Maze> revertedTree = ((LinkedList<Node<Maze>>) this.workingPath).removeLast();

            revertedTree = revertedTree.getFather();
            this.pathLength++;

            while (revertedTree.hasFather()) {
                Maze temp = revertedTree.getContent();
                Square state = temp.getCurrentSquare();

                if (!state.equals(this.maze.getEndSquares())) {
                    this.maze.path.add(state);
                    this.pathLength++;


                }
                revertedTree = revertedTree.getFather();
            }
            this.maze.print(nodesCounter, time);


        } else {
            System.out.println("Failed : Unable to go further and/or end is unreachable.");
        }

        return endfound;

    }

    public LinkedList<Node<Maze>> getNextSquares() {
        LinkedList<Node<Maze>> result = new LinkedList<Node<Maze>>();

        //Get 4 next squares
        LinkedList<Maze> nextSquares = this.maze.getCurrentSquare().getNexts();

        for (int i = 0; i < nextSquares.size(); i++) {
            Square tempSq = nextSquares.get(i).getCurrentSquare();
            if (!this.closedSquares.contains(tempSq)) {

                Node<Maze> tempNode = new Node<Maze>(nextSquares.get(i));
                result.add(tempNode); //Add the state
            }
        }

        return result;
    }


    public AbstractCollection<Node<Maze>> getWorkingPath() {
        return this.workingPath;
    }


}
