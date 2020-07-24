import java.util.AbstractCollection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Stack;

public class DFS extends Solver
{
	/*
	 * Constructor
	 * m: The maze to solve
	 */
	private int depth = Integer.MAX_VALUE;

	public DFS(Maze m)
	{
		this.maze = m;
		this.workingPath = new Stack<Node<Maze>>();
		this.closedSquares = new Stack<Square>();
	}

	public DFS(Maze m, int depth)
	{
		this.maze = m;
		this.workingPath = new Stack<Node<Maze>>();
		this.closedSquares = new Stack<Square>();
		this.depth = depth;
	}
	
	public boolean solve()
	{
		Boolean endfound = false;
		this.nodesCounter = 0;
		

		((Stack<Node<Maze>>) this.workingPath).push(new Node<Maze>(this.maze)); //Add first state
		
		//Measure run time
		long startTime = System.currentTimeMillis();
		
		//Search
		while(!endfound)
		{
			if(this.workingPath.isEmpty())
				break;
			
			else
			{
				Node<Maze> currentNode = ((Stack<Node<Maze>>) this.workingPath).pop(); //Get first node from the frontier
				this.maze = (Maze) currentNode.getContent();
				Square currentSquare = this.maze.getCurrentSquare();
				
//				if(currentSquare.getLine() == this.maze.getEndSquares().getLine() && currentSquare.getCol() == this.maze.getEndSquares().getCol())
				if(this.maze.getEndSquares().contains(currentSquare))
				{
					Node<Maze> temp = new Node<Maze>(this.maze);
					temp.setFather(currentNode);
					((Stack<Node<Maze>>) this.workingPath).push(temp);
					endfound = true;
					Maze.path.add(currentNode.getContent().getCurrentSquare());
				}	
				
				else
				{
					LinkedList<Node<Maze>> nextSquares = this.getNextSquares(); //Get next possible states
					if(!this.closedSquares.contains(currentSquare))
					{
						((Stack<Square>) this.closedSquares).push(currentSquare);
					}
					
					Iterator<Node<Maze>> iterator = nextSquares.descendingIterator();
					
					while(iterator.hasNext())
					{
						Node<Maze> temp = iterator.next();
						temp.setFather(currentNode);
						temp.setDepth(currentNode.getDepth() + 1);
						if (temp.getDepth() <= this.depth){
							((Stack<Node<Maze>>) this.workingPath).push(temp);
							this.nodesCounter++;
						}

					}
				}
			}
		}

		long endTime = System.currentTimeMillis();

		long time = endTime - startTime;

		

		if(endfound)
		{
			Node<Maze> revertedTree = ((Stack<Node<Maze>>) this.workingPath).pop();

			revertedTree = revertedTree.getFather().getFather();
			this.pathLength++;

			while(revertedTree.hasFather())
			{
				Maze temp = revertedTree.getContent();
				Square state = temp.getCurrentSquare();

				if(!state.equals(this.maze.getEndSquares()))
				{
					Maze.path.add(state);
					this.pathLength++;
				}
				revertedTree = revertedTree.getFather();
			}
			this.maze.print( nodesCounter, time);

		}
		else
		{
			if (this.depth == Integer.MAX_VALUE){
				System.out.println("Failed : Unable to go further and/or end is unreachable.");
			}

		}

		return endfound;

	}
	
	public LinkedList<Node<Maze>> getNextSquares()
	{
		LinkedList<Node<Maze>> result = new LinkedList<Node<Maze>>();
		
		//Get 4 next squares
		LinkedList<Maze> nextSquares = this.maze.getCurrentSquare().getNexts();
		
		for(int i = 0; i < nextSquares.size(); i++)
		{
			Square tempSq = nextSquares.get(i).getCurrentSquare();
			if(!this.closedSquares.contains(tempSq))
			{
				Node<Maze> tempNode = new Node<Maze>(nextSquares.get(i));
				result.add(tempNode);
			}
		}
		
		return result;
	}


	public AbstractCollection<Node<Maze>> getWorkingPath()
	{
		return  this.workingPath;
	}
}
