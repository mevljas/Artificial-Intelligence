import java.lang.Math;
import java.util.ArrayList;
import java.util.LinkedList;

public class Square {
    private Maze maze;
    private int row;
    private int column;
    private boolean wall;

    private int g;
    private double h;
    private double f;
    private int cost;


    /*
     * Constructor for a wall square in the maze
     * row: Line poisition
     * column: Column position
     * w: Wall state
     * 		-> Set as true to define the square as a wall
     */
    public Square(int row, int column, boolean wall, int cost) {
        this.row = row;
        this.column = column;
        this.wall = wall;
        this.g = 0;
        this.h = 0;
        this.f = 0;
        this.cost = cost;
    }

    public void assignMaze(Maze m) {
        this.maze = m;
    }

    /*
     * Returns the line position of the square in the maze
     */
    public int getLine() {
        return row;
    }


    /*
     * Returns the column position of the square in the maze
     */
    public int getCol() {
        return column;
    }


    /*
     *
     * c: The origin Square from where to get the next squares
     */
    public LinkedList<Maze> getNexts() {
        LinkedList<Maze> nexts = new LinkedList<Maze>();

        for (int i = 0; i < 4; i++) {
            Maze tempMaze = this.maze.clone();

            if (this.maze.order[i] == 'N') {
                if (this.getNorth() != null && !this.getNorth().isWall()) {
                    tempMaze.setNextSquare(this.getNorth());
                    nexts.push(tempMaze);
                }
            } else if (this.maze.order[i] == 'E') {
                if (this.getEast() != null && !this.getEast().isWall()) {
                    tempMaze.setNextSquare(this.getEast());
                    nexts.push(tempMaze);
                }
            } else if (this.maze.order[i] == 'S') {
                if (this.getSouth() != null && !this.getSouth().isWall()) {
                    tempMaze.setNextSquare(this.getSouth());
                    nexts.push(tempMaze);
                }
            } else if (this.maze.order[i] == 'W') {
                if (this.getWest() != null && !this.getWest().isWall()) {
                    tempMaze.setNextSquare(this.getWest());
                    nexts.push(tempMaze);
                }
            }
        }
        return nexts;
    }

    /*
     * Returns the Square at North from the given Square
     * c: The origin Square from where to get the North Square
     */
    public Square getNorth() {
        if (this.row - 1 < 0)
            return null;
        else
            return this.maze.getGrid()[this.row - 1][this.column];
    }

    /*
     * Returns the Square at West from the given Square
     * c: The origin Square from where to get the West Square
     */
    public Square getWest() {
        if (this.column - 1 < 0)
            return null;
        else
            return this.maze.getGrid()[this.row][this.column - 1];
    }

    /*
     * Returns the Square at South from the given Square
     * c: The origin Square from where to get the South Square
     */
    public Square getSouth() {
        if (this.row + 1 == this.maze.nRows)
            return null;
        else
            return this.maze.getGrid()[this.row + 1][this.column];
    }

    /*
     * Returns the Square at East from the given Square
     * c: The origin Square from where to get the East Square
     */
    public Square getEast() {
        if (this.column + 1 == this.maze.nColumns)
            return null;
        else
            return this.maze.getGrid()[this.row][this.column + 1];
    }


    /*
     * Sets the square attribute
     * If the attribute given is not correct, it changes nothing
     */


    /*
     * Returns wall attribute
     */
    public boolean isWall() {
        return this.wall;
    }


    //----------------------
    // H Value
    //----------------------

    /*
     * Returns H value
     */
    public double getH() {
        return this.h;
    }

    /*
     * Computes the H value with the Manhattan distance
     * end: The ending Square in the maze
     */
    public void calcManhattanH() {
        double min = Integer.MAX_VALUE;
        double temp = min;
        for (Square engSquare : this.maze.getEndSquares()) {
            temp = Math.abs(this.getLine() - engSquare.getLine())
                    + Math.abs(this.getCol() - engSquare.getCol());
            min = Math.min(min, temp);
        }
        this.h = min;
    }

    /*
     * Computes the H value with the Euclidean distance
     * end: The ending Square in the maze
     */
    public void calcEuclidH() {

        double min = Integer.MAX_VALUE;
        double temp = min;
        for (Square engSquare : this.maze.getEndSquares()) {
            temp = Math.sqrt(
                    Math.pow(this.getLine() - engSquare.getLine(), 2)
                            + Math.pow(this.getCol() - engSquare.getCol(), 2)
            );
            min = Math.min(min, temp);
        }
        this.h = min;
    }

    //----------------------
    // G Value
    //----------------------

    /*
     * Returns G value
     */
    public int getG() {
        return g;
    }

    /*
     * Increases the G value
     */
    public void incG(int prev) {
        this.g = 1 + prev;
    }

    //----------------------
    // F Value
    //----------------------

    /*
     * Computes F value by addition of H and G
     */
    public double calcF() {
        this.f = this.g + this.h + this.getCost();
        return this.f;
    }

    /*
     * Returns F value
     */
    public double getF() {
        return this.f;
    }

    /*
     * Returns the Square in a string with the format "[LINE, COLUMN](F)"
     */
    public String toString() {
        return "[" + this.row + ", " + this.column + "](" + this.f + ")";
    }

    public int getCost() {
        return cost;
    }

    public boolean checkEnd(ArrayList<Square> list) {
        for (Square square : list) {
            if (square.column == this.column && square.row == this.row) {
                /**/
                return true;
            }
        }
        return false;
    }


}
