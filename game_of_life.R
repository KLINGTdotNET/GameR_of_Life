living_neighbours <- function(board, row, col) {
  # return is implicit in R, if it's omitted, then the value of the last statement in the block will be returned
  return(sum(board[max(row-1, 1):min(row+1, nrow(board)), max(col-1, 1):min(col+1, ncol(board))]) - board[row,col])
}

update<-function(board, new_board) {
  for(row in 1:nrow(board)) {
    for(col in 1:ncol(board)) {
      lives <- living_neighbours(board, row, col)
      if (lives == 2) {
        # new state is the current state
        new_board[row,col] = board[row,col]
      } else if (lives == 3) {
        # the cell will be "born" in the next state
        new_board[row,col] = 1;
      } else {
        # the cell dies in the next state
        new_board[row,col] = 0;
      }      
    } 
  }
  return(new_board)
}

go <- function(iterations=10, rows=10, cols=10, start_population=0.6) {  
  data <- runif(rows*cols)<0.6     # generate a uniformly distributed vector and check against a boundary -> results in vector of type boolean
  board <- matrix(data,rows,cols)  # generate a matrix from these values
  new_board <- matrix(0,rows,cols) # allocate matrix where the new states are saved
  times <- rep(0, iterations)
  for(i in 1:iterations) {    
    t <- system.time(board <- update(board, new_board))
    times[i] <- t[1]
    # print(attributes(t)) # shows fields of object
    image(board, col=c("#cccccc", "#333333"))  # view board
  }  
  print(paste("Execution time (in mean):", mean(times), "ms")) # paste concatenates strings
}

enter_values <- function() {
  usage <- "USAGE\n\tdimensions: x y\n\tf.e. dimensions: 10 15"
  print("dimensions: ")
  dim<-scan()
  if (length(dim) != 2) {
    print("Too many dimensions!")
    cat(usage)  # cat() parses escape characters
    return(NULL)
  } 
  return(dim)
}

# Main
dim <- enter_values()
if(!is.null(dim)) {
  go(100, dim[1], dim[2])
}