import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        int lab = 1;
        int algoritm = 1;

        try (Scanner in = new Scanner(System.in)) {
            System.out.println("Vpisi stevilko labirinta [1-15]: ");
            lab = in.nextInt();

            System.out.println("1 = BFS");
            System.out.println("2 = DFS");
            System.out.println("3 = IDDFS");
            System.out.println("4 = A* Manhatan");
            System.out.println("5 = A* Euclid");
            System.out.println("Vpisi Stevilko algoritma: ");
            algoritm = in.nextInt();

        } catch (Exception e) {
            e.printStackTrace();
        }


        Maze maze = new Maze("labyrinths/labyrinth_" + lab + ".txt");


        BFS bfs = new BFS(maze);
        DFS dfs = new DFS(maze);
        IDDFS iddfs = new IDDFS(maze, 0);
        AStar AStarM = new AStar(maze, true);
        AStar AStarE = new AStar(maze, false);

        switch (algoritm) {
            case 1:
                bfs.solve();
                break;
            case 2:
                dfs.solve();
                break;
            case 3:
                iddfs.solve();
                break;
            case 4:
                AStarM.solve();
                break;
            case 5:
                AStarE.solve();
        }


    }
}
