#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's Make a
#'   Deal" where there are three doors for a contestant to choose
#'   from, one of which has a car behind it and two have goats. The
#'   contestant selects a door, then the host opens a door to reveal
#'   a goat, and then the contestant is given an opportunity to stay
#'   with their original selection or switch to the other unopened
#'   door. There was a famous debate about whether it was optimal to
#'   stay or switch when given the option to switch, so this
#'   simulation was created to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample(x=c("goat", "goat", "car"), size = 3, replace = F)
  return(a.game)
}



#' @title
#'   Contestant selects a door.
#'
#' @description
#'   `select_door()` randomly selects one of the three doors as the
#'   contestant's initial pick.
#'
#' @details
#'   Since the contestant will not know the position of the car when
#'   they select a door, no information about the game set-up is
#'   shared before the selection is made. The door is drawn at random
#'   from the three positions.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 1 numeric vector, a number
#'   between 1 and 3 indicating the door the contestant selected.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function()
{
  doors <- c(1,2,3)
  a.pick <- sample(x = doors, size = 1)
  return(a.pick)
}



#' @title
#'   Host opens a goat door.
#'
#' @description
#'   `open_goat_door()` returns the number of a door with a goat
#'   behind it that is not the contestant's pick.
#'
#' @details
#'   The host cannot open the contestant's door and can only open a
#'   door with a goat behind it. That leaves two cases. If the
#'   contestant selected the car, both remaining doors are goats, so
#'   one of the two is opened at random. If the contestant selected a
#'   goat, only one goat door is left that is not the contestant's,
#'   so the host has no choice.
#'
#' @param game a length 3 character vector of "goat" and "car"
#'   returned by `create_game()`.
#' @param a.pick a length 1 numeric vector between 1 and 3, the door
#'   returned by `select_door()`.
#'
#' @return The function returns a length 1 numeric vector, a number
#'   between 1 and 3 indicating the door the host opened.
#'
#' @examples
#'   this.game <- create_game()
#'   my.initial.pick <- select_door()
#'   open_goat_door( this.game, my.initial.pick )
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1,2,3)
  
  # if contestant selected car, randomly select one of two goats
  if(game[a.pick] == "car")
  {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(x=goat.doors, size=1)
  }
  
  # if contestant selected a goat, open the only other goat door
  if(game[a.pick] == "goat")
  {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  return(opened.door)
}



#' @title
#'   Contestant stays or switches.
#'
#' @description
#'   `change_door()` returns the contestant's final door choice after
#'   they either keep their original pick or switch to the remaining
#'   unopened door.
#'
#' @details
#'   The strategy is passed in through the `stay` argument. If the
#'   contestant stays, the final pick is the original pick and there
#'   is nothing to compute. If the contestant switches, the opened
#'   door and the original pick are removed and the one door left is
#'   the final pick. With only three doors, removing those two always
#'   leaves exactly one.
#'
#' @param stay a length 1 logical vector. TRUE keeps the original
#'   pick and FALSE switches to the remaining door. Defaults to TRUE.
#' @param opened.door a length 1 numeric vector between 1 and 3, the
#'   door returned by `open_goat_door()`.
#' @param a.pick a length 1 numeric vector between 1 and 3, the door
#'   returned by `select_door()`.
#'
#' @return The function returns a length 1 numeric vector, a number
#'   between 1 and 3 indicating the contestant's final door.
#'
#' @examples
#'   this.game <- create_game()
#'   my.initial.pick <- select_door()
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'
#'   change_door( stay=T, opened.door, my.initial.pick )
#'   change_door( stay=F, opened.door, my.initial.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick)
{
  doors <- c(1,2,3)
  if( stay == T)
  {
    final.pick <- a.pick
  }
  if( stay == F)
  {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }
  return(final.pick) # number between 1 and 3
}



#' @title
#'   Determine if the contestant has won.
#'
#' @description
#'   `determine_winner()` reports whether the contestant's final door
#'   held the car or a goat.
#'
#' @details
#'   The final pick is used as a position in the game vector to find
#'   out what is behind that door. A car returns "WIN" and a goat
#'   returns "LOSE". Every door holds one or the other, so one of the
#'   two returns always fires.
#'
#' @param final.pick a length 1 numeric vector between 1 and 3, the
#'   door returned by `change_door()`.
#' @param game a length 3 character vector of "goat" and "car"
#'   returned by `create_game()`.
#'
#' @return The function returns a length 1 character vector, either
#'   "WIN" or "LOSE".
#'
#' @examples
#'   this.game <- create_game()
#'   my.initial.pick <- select_door()
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'   my.final.pick <- change_door( stay=F, opened.door, my.initial.pick )
#'
#'   determine_winner( my.final.pick, this.game )
#'
#' @export
determine_winner <- function( final.pick, game )
{
  
  if( game[final.pick] == "car")
  {
    return( "WIN" )
  }
  if( game[final.pick] == "goat")
  {
    return( "LOSE" )
  }
  
}



#' @title
#'   Play one complete game.
#'
#' @description
#'   `play_game()` plays a single game from start to finish and
#'   returns the outcome under both the stay and switch strategies.
#'
#' @details
#'   The functions above each handle a single move. This function
#'   chains them together so that one call plays one complete game.
#'   The game vector, the first pick, and the opened door are held
#'   constant, and both strategies are then evaluated against that
#'   same set-up. The only thing that differs between the two results
#'   is the strategy itself, which is what makes the comparison fair.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a data frame with two rows and two
#'   columns. The strategy column holds "stay" and "switch", and the
#'   outcome column holds "WIN" or "LOSE" for each.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#'   Simulate a series of games.
#'
#' @description
#'   `play_n_games()` plays n games and returns the results of every
#'   game in a single data frame.
#'
#' @details
#'   One game tells us nothing, because the outcome is random. The
#'   true win rate of each strategy only appears once enough games
#'   have been played for the proportions to settle. The loop has
#'   three parts. The collector, `results.df`, is an empty object
#'   declared before the loop so that the first `rbind()` has
#'   something to bind to. The iterator repeats the body n times. The
#'   binding step stacks the two row result of each game onto
#'   everything collected so far.
#'
#'   The default is 10,000 games, which is the rule of thumb for
#'   inference and returns a data frame of 20,000 rows, two per game.
#'   The printed table reports row proportions, so each row reports
#'   the share of wins and losses within that strategy rather than
#'   out of all games played.
#'
#' @param n a length 1 numeric vector, the number of games to play.
#'   Defaults to 10000.
#'
#' @return The function returns a data frame with 2n rows and columns
#'   for strategy and outcome. The table of row proportions is
#'   printed as a side effect.
#'
#' @examples
#'   play_n_games( n=100 )
#'
#' @export
play_n_games <- function( n=10000 )
{
  results.df <- NULL   # collector
  
  for( i in 1:n )      # iterator
  {
    game.outcome <- play_game()
    results.df <- rbind( results.df, game.outcome )   # binding step
  }
  
  print( round( prop.table( table( results.df ), margin=1 ), 3 ) )
  
  return( results.df )
}
