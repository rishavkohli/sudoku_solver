sudoku_board<<-matrix(,9,9)
number<-c()
rowno<-c()
columnno<-c()
value<-c()
input<-function()
{ 
  number<<-readline(prompt = "enter no of values u want to enter in sudoku")
  number<<-as.vector(number)
  for(i in 1:number)
  {  print("enter row no , column no and data(0-9)")
    rowno[i]<<-readline(prompt = "row no ")
    columnno[i]<<-readline(prompt = "column no ")
    value[i]<<-readline(prompt = "value ")
  }
}


initiate<-function()
{rowno<<-as.numeric(rowno)
columnno<<-as.numeric(columnno)
value<<-as.numeric(value)

for(i in 1:number)
{   
  sudoku_board[rowno[i],columnno[i]]<<-value[i]
}
}
input()
24

2
2
2

4
2
6

6
2
9

9
2
7

3
3
4

6
3
8

9
3
9

7
4
2

8
4
7

3
5
8

4
5
2

6
6
1

1
7
8

4
7
1

8
7
3

9
7
2

1
8
9

2
8
5

6
8
3

8
8
6

5
5
5

6
5
6

2
6
9

3
6
2

initiate()
print(sudoku_board)

constraint<-function(c,row,column)
{   
  for(i in 1:9)
  { if(is.na(sudoku_board[i,column])==FALSE && sudoku_board[i,column]==c)
    return(FALSE)
  }
  
  for(i in 1:9)
  { if(is.na(sudoku_board[row,i])==FALSE && sudoku_board[row,i]==c)
    return(FALSE)
  }
  
  if(row<=3)
    rowno<-1
  else if(row<=6 && row>=4)
    rowno<-4
  else if(row<=9 && row>=7)
    rowno<-7
  
  
  if(column<=3)
    columnno<-1
  else if(column<=6 && column>=4)
    columnno<-4
  else if(column<=9 && column>=7)
    columnno<-7
  
  
  for(i in rowno:(rowno+2))
  {for(j in columnno:(columnno+2))
  { if(is.na(sudoku_board[i,j])==FALSE && sudoku_board[i,j]==c)
    return(FALSE)
    
  }
  }
  
  return(TRUE) 
}


sudoku_solver<-function()
{for(i in 1:9 )
{ for(j in 1:9)
{ if(is.na(sudoku_board[i,j])==TRUE)
{ for(k in 1:9)
{  if(constraint(k,i,j))
  {sudoku_board[i,j]<<-k
if(sudoku_solver())
  return(TRUE)
else 
  sudoku_board[i,j]<<-NA
}
}
  return(FALSE)
}
  
}
}
  
  return(TRUE)
  
}
ikkkk<-sudoku_solver()
print(ikkkk)
print(sudoku_board)
length(sudoku_board)


warnings()
