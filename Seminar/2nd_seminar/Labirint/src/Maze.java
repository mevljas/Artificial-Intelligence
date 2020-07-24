import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Maze {
    private Square[][] grid;
    private Square startSquare;
    private ArrayList<Square> endSquares = new ArrayList<>();
    private Square currentSquare;
    public int nRows;
    public int nColumns;
    public char[] order = {'N', 'E', 'S', 'W'}; //Shift order in the grid during solving in cardinals
    static double squareSize;
    static int canvasSize = 800;
    public static ArrayList<Square> path = new ArrayList();


    /*
     * Constructor with file
     * file: .txt file from where to import the maze
     */
    public Maze(String filepath) {

        List<List<Integer>> rows = new ArrayList();

        try (Scanner in = new Scanner(new File(filepath))) {
            List<Integer> row = new ArrayList<>();
            String[] readRow;
            while (in.hasNextLine()) {
                nRows++;
                nColumns = 0;
                readRow = in.nextLine().split(",");
                for (String item : readRow) {
                    row.add(Integer.parseInt(item.replace(" ", "")));
                    nColumns++;
                }
                rows.add(row);
                row = new ArrayList<>();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }


        //Init grid
        this.grid = new Square[this.nRows][this.nColumns];

        for (int i = 0; i < this.nRows; i++) {
            for (int j = 0; j < this.nColumns; j++) {
                switch (rows.get(i).get(j)) {
                    case -3:
                        this.getEndSquares().add(new Square(i, j, false, rows.get(i).get(j)));
                        this.setEndSquares(this.getEndSquares());
                        break;

                    case -2:
                        this.setStartSquare(new Square(i, j, false, rows.get(i).get(j)));
                        break;

                    case -1:
                        this.grid[i][j] = new Square(i, j, true, rows.get(i).get(j));
                        break;

                    default:
                        this.grid[i][j] = new Square(i, j, false, rows.get(i).get(j));
                        break;
                }
            }
        }


        this.assignMazeToGridSquares();

        this.currentSquare = this.getStartSquare();

        this.initDrawing();
        this.draw();

    }

    public Maze(Square[][] grid, Square startSquare, ArrayList<Square> endSquares, Square currentSquare, int nRows, int columns) {
        this.grid = grid;
        this.startSquare = startSquare;
        this.endSquares = endSquares;
        this.currentSquare = currentSquare;
        this.nRows = nRows;
        this.nColumns = columns;
    }

    /*
     * Retuns the starting Square
     */
    public Square getStartSquare() {
        return startSquare;
    }

    /*
     * Sets the starting Square for the attribute and the grid
     * start: The square to set as starting square
     */
    public void setStartSquare(Square startSquare) {
        this.startSquare = startSquare;
        this.grid[startSquare.getLine()][startSquare.getCol()] = startSquare;
    }

    /*
     * Returns the ending Square
     */
    public ArrayList<Square> getEndSquares() {
        return endSquares;
    }

    /*
     * Sets the ending Square for the attribute and the grid
     * end: The square to set as ending square
     */
    public void setEndSquares(ArrayList<Square> endSquares) {
        this.endSquares = endSquares;
        for (Square endSquare : endSquares) {
            this.grid[endSquare.getLine()][endSquare.getCol()] = endSquare;
        }

    }


    public Square getCurrentSquare() {
        return this.currentSquare;
    }

    public void setNextSquare(Square rename) {
        this.currentSquare = rename;
    }

    public void assignMazeToGridSquares() {
        for (int i = 0; i < this.nRows; i++) {
            for (int j = 0; j < this.nColumns; j++) {
                this.grid[i][j].assignMaze(this);
            }
        }
    }


    /*
     * Returns the maze grid
     */
    public Square[][] getGrid() {
        return grid;
    }

    public Maze clone() {
        return new Maze(this.grid, this.startSquare, this.endSquares, this.currentSquare, this.nRows, this.nColumns);
    }


    public String toString() {
        return this.currentSquare.toString();
    }

    void draw() {

        for (int i = 0; i < this.grid.length; i++) {
            for (int j = 0; j < this.grid[i].length; j++) {
                switch (this.grid[i][j].getCost()) {
                    case -1:
                        StdDraw.setPenColor(0, 0, 0);
                        break;
                    case -2:
                        StdDraw.setPenColor(255, 0, 0);
                        break;
                    case -3:
                        StdDraw.setPenColor(255, 255, 0);
                        break;
                    case 1:
                        StdDraw.setPenColor((int) (0.3 * 255), (int) (0.3 * 255), (int) (0.3 * 255));
                        break;
                    case 2:
                        StdDraw.setPenColor((int) (0.4 * 255), (int) (0.4 * 255), (int) (0.4 * 255));
                        break;
                    case 3:
                        StdDraw.setPenColor((int) (0.6 * 255), (int) (0.6 * 255), (int) (0.6 * 255));
                        break;
                    case 4:
                        StdDraw.setPenColor((int) (0.8 * 255), (int) (0.8 * 255), (int) (0.8 * 255));
                        break;
                    default:
                        StdDraw.setPenColor(255, 255, 255);

                }

                drawText(i, j);

            }
        }

    }

    private void drawText(int i, int j) {
        StdDraw.filledRectangle(j * squareSize + squareSize / 2, (this.grid.length - i) * squareSize - squareSize / 2, squareSize / 2, squareSize / 2);
        StdDraw.setPenColor(0, 0, 0);
        StdDraw.text(j * squareSize + squareSize / 2, (this.grid.length - i) * squareSize - squareSize / 2, this.grid[i][j].getCost() + "");
    }

    public void initDrawing() {
        squareSize = (double) canvasSize / (this.grid.length * this.grid[0].length);
        StdDraw.setCanvasSize(canvasSize, canvasSize);
        StdDraw.setXscale(0, squareSize * this.grid[0].length);
        StdDraw.setYscale(0, squareSize * this.grid.length);
        Font font = new Font("Arial", Font.PLAIN, (int) (squareSize * 10));
        StdDraw.setFont(font);

    }


    public void drawNode(int i, int j) {
        StdDraw.setPenColor(0, 0, 255);
        drawText(i, j);
    }

    public void print( int nodesCounter, long time) {
        int cost = 0;
        path.add(startSquare);
        System.out.print("Path: ");
        for (int j = path.size() - 1; j >= 0; j--) {
            Square square = path.get(j);
            cost += square.getCost();
            System.out.print("[" + square.getLine() + ", " + square.getCol() + "], ");
            drawNode(square.getLine(), square.getCol());
        }
        System.out.println();
        System.out.println("Cost: " + cost);
        System.out.println("Moves: " + (path.size() - 1));
        System.out.println("Number of nodes: " + nodesCounter);
        System.out.println("Execution time: " + time / 1000d + " seconds\n");
    }

}
