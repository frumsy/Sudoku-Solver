import  hw.sudoku._

case class Unsolvable(s:String)  extends Exception(s)

object  Solution  extends  SudokuLike {

type T = Board
val fullCell = 1::2::3::4::5::6::7::8::9::Nil

def strtoBoardX(num: Int): Int = 
{
	(num)/9
}
def strtoBoardY(num: Int): Int = 
{
	(num)%9
}

def removeNum(cell: List[Int], num: Int): List[Int] =
{
	cell match {
		case Nil => Nil
		case head :: tail =>{
			 if(head == num)
			 {
			 	tail
			 }
			 else
			 {
			 	head :: removeNum(tail,num)
			 }
		} 
	}
}

def peersOf(x: Int, y: Int, m: Map[(Int , Int), List[Int]]): Map[(Int , Int), List[Int]] =
{
 	val prs = peers(x,y)
 	m.filter(p => prs.contains(p._1))
}

def placeVals(aBoard: Map[(Int , Int), List[Int]], toPlace: List[Int]): Map[(Int , Int), List[Int]] = 
{
	toPlace match {
		case Nil => aBoard
		case head :: tail =>
		{
			if(head == 0)//go to next cell
			{
				placeVals(aBoard,tail)
			}
			else//place value
			{
			val pos = (strtoBoardX(81 - toPlace.size), strtoBoardY(81 - toPlace.size))
			val nVal = (pos,head::Nil)
			val nBoard = aBoard + nVal
			val parsedBoard = parseDelete(nBoard,pos,head)
			placeVals(parsedBoard,tail)
			}
		} 
	}
}

def deletePeers(brd: Map[(Int , Int), List[Int]], peers: Map[(Int , Int), List[Int]], value: Int): Map[(Int , Int), List[Int]] = //depth first 
{
	peers.isEmpty match {
		case true => brd
		case false =>
		{
			val head = peers.head
			//println(head)
			if(head._2.size > 1)
			{
				val nHead = (head._1,removeNum(head._2, value))
				val nBoard = brd + nHead
				if(nHead._2.size == 1)
				{
					val loc = nHead._1	
					parseDelete(nBoard ++ deletePeers(nBoard,peers.tail,value), loc, nHead._2.head)
				}
				else
				{
					deletePeers(nBoard,peers.tail,value)
				}
			}
			else
			{
				val nHead = (head._1, removeNum(head._2, value))
				val newBoard = brd + nHead
				deletePeers(newBoard,peers.tail,value)
			}
		} 
	}
}

def parseDelete(brd: Map[(Int , Int), List[Int]], pos: (Int,Int), value: Int): Map[(Int , Int), List[Int]] =
{
	val prs = peersOf(pos._1, pos._2, brd)
	deletePeers(brd, prs, value)
}

def getEmptyBoard(): Map[(Int , Int), List[Int]] =
{
	val coords = 0.to(8).map(x => 0.to(8).map(y => (x, y))).flatten.toList
	val emptyBoard = coords.map(x => (x,fullCell)).toMap
	emptyBoard
}

def to0List(str: String): List[Int] =
{
	str.toList.map(x=> {
		if(x == '.')
		{0}
		else
		{x.asDigit}
	})
}

def parse(str: String ):  Board = 
{
	val emptyBoard = getEmptyBoard()
	val ls = to0List(str)
	val nBoard = placeVals(emptyBoard, ls)
	new Board(nBoard)
}

def buildBlock(block: Int): List[(Int, Int)] =
{
	val firstx = (block % 3)*3
	val firsty = (block / 3)*3
	firstx.to(firstx+2).map(x => firsty.to(firsty+2).map(y => (x, y))).flatten.toList
}

def peers(row: Int , col: Int): List[(Int , Int)] = 
{
	rowPeers(row,col) ::: colPeers(row,col) ::: blockPeers(row,col)
}

def getBlock(x: Int, y: Int): Int  = 
{
	x/3 + ((y/3)*3)
}

//produces the coords of the peers in the block without the coords of the row and col peers in the block
def blockPeers(row: Int , col: Int): List[(Int , Int)] = 
{

	buildBlock(getBlock(row,col)).filter((pos) => pos._1 != row &&
									  pos._2 != col)
}

def rowPeers(row: Int , col: Int): List[(Int , Int)] = 
{
	row match {
		case 8 => 0.to(7).map(x => (x, col)).toList
		case 0 => 1.to(8).map(x => (x, col)).toList
		case _ =>
		{
			val left = 0.to(row-1).map(x => (x, col)).toList
			val right = (1+row).to(8).map(x => (x, col)).toList
			left ::: right
		} 
	}
}

def colPeers(row: Int , col: Int): List[(Int , Int)] = 
{
	col match {
		case 8 => 0.to(7).map(y => (row, y)).toList
		case 0 => 1.to(8).map(y => (row, y)).toList
		case _ =>
		{
			val left = 0.to(col-1).map(y => (row, y)).toList
			val right = (1+col).to(8).map(y => (row, y)).toList
			left ::: right
		} 
	}
}

}

//Top left corner is (0,0). Bottom right corner is (8 ,8).
class  Board(val available: Map[(Int , Int), List[Int ]])  extends  BoardLike[Board] {

def  availableValuesAt(row: Int , col: Int): List[Int] = {
//  Assumes  that a missing  value  means  all  values  are  available.
available.getOrElse ((row , col), 1.to(9). toList)
}
def  valueAt(row: Int , col: Int): Option[Int] = 
{
	val ls = available.get(row,col)
	ls.get.size match {
		case 0 => throw new Unsolvable("This board is not solvable!")
		case 1 => Some(ls.get.head)
		case _ =>  None
	}	
}

def  isSolved ():  Boolean = 
{
	!(available.values.exists(x=> x.size != 1))
}

def  isUnsolvable ():  Boolean = 
{
	available.values.exists(x=> x.size == 0)
}

def  place(row: Int , col: Int , value: Int):  Board = {
require(availableValuesAt(row , col).contains(value))
val pos = (row,col)
val nVal = (pos,value::Nil)
val nBoard = available + nVal
val parsedBoard = Solution.parseDelete(nBoard,pos,value)
new Board(parsedBoard)
}

def  nextStates ():  List[Board] = {
if(isUnsolvable()) {
	List()
}
else {
	val placeable = available.filter(x => x._2.size > 1)
	val rPos = util.Random.shuffle(placeable)
	val toPlace = placeable.map(x => x._2.map(y => (x._1,y::Nil)) ).flatten.toList
	val boards = toPlace.map(x => this.place(x._1._1,x._1._2,x._2.head))
	boards
	}
}

def solveNext(ls: List[Board]): Option[Board] =
{
	val winner = ls.find(x => x.isSolved)
	winner match {
		case None =>
		{
			val boards = ls.filter(x => !x.isUnsolvable())
			if(boards.size == 0)
			{
				None
			}
			else
			{
				val nextBoards = boards.map(x => x.nextStates).flatten
				solveNext(nextBoards)
			}
		} 
		case Some(x) => winner
	}
}

def solve (): Option[Board] = 
{
	if(this.isSolved)
	{
		Some(this)
	}
	else if(this.isUnsolvable)
	{
		None
	}
	else
	{
		solveNext(this.nextStates())
	}
}

}