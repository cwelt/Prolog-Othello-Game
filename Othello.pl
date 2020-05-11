/******************************************************************************
*__________________________Mamam17 - Final Project____________________________*
*					 20596 - Prolog & Artificial Intelligence				  *				
*------------|----------------------------------------------------------------*														
* Programmer | Chanan Welt									  * 
*------------|----------------------------------------------------------------*
* File Name  | Othello.pl													  * 
*------------|----------------------------------------------------------------*
* Description| Othello nxn board game application against the computer, 	  *
*			 | implemented with the alphabeta search algorithm.				  *
*------------|----------------------------------------------------------------*
* Synopsis   | Start the game with the predicate run/0. 			 		  *
* 		     | The game will start & guide you with further instructions.  	  *
* 		     | You can play interactive against computer or watch an automatic*
* 		     | game of a computer playing itself.							  *
* 		     | The rules of the game are the classical othello rules.   	  *
*			 | For further details on how to play, see the following link:	  *
*			 | https://www.wikihow.com/Play-Othello							  *
*------------|--------------------------------------------------------------*/%.
/* dynamic utllity predicates & data structures  for interal representation   */
:- dynamic dimension/1.	 		% N - dimension of the quadratic board 		
:- dynamic coordinate/1. 		% coordinate(I,J), indices of a specific slot on board 
:- dynamic slot/3.				% slot(GridIdentifier,coordinate(I,J),ValueOnThisSlot)
:- dynamic next_idle_grid_id/1. % numerator for managing grid id's. 
:- dynamic user_exited_game/0.  % flag indicating if user quit game.
:- dynamic end_of_game/1.	 	% flag indicating end of game & last final grid
:- dynamic no_legal_move/0. 	% flag indicating no legal moves for both players.
:- dynamic player_stuck/1.   	% flag indicating this player has no legal moves 
:- dynamic pos_evaluation/4. 	% (HashKey,Depth,Pos,Val) - precalculated position entries 

/* cleanup - clear memory from all dynamic predicates */ 
cleanup:-
	retractall(dimension(_)),
	retractall(coordinate(_)),
	retractall(slot(_,_,_)),
	retractall(next_idle_grid_id(_)),
	retractall(user_exited_game),
	retractall(end_of_game(_)),
	retractall(no_legal_move),
	retractall(player_stuck(_)),
	retractall(pos_evaluation(_,_,_,_)).


/******************************************************************************
*________________Internal representation logic & utillities module____________*
*****************************************************************************/%.
% list of all possible directions in grid for supporting non-determinism 
direction_list([north,northEast,east,southEast,south,southWest,west,northWest]).


/* initialize_board(+N) - N is a natural number for setting dimension of grid */
initialize_board(N):-
	assert(dimension(N)),				 % set dimension in memory
	initialize_starting_pos_grid(1,1,N), % build starting position with GridId=0
	assert(next_idle_grid_id(1)).		 % update numerator 


/* initialize_starting_pos_grid(+I,+J,+Dimension) - build starting position	  
   I & J are row & column indices (respectivly). Starting Grid Id is 0.		  
   values of slots on grid are as follows - 0=nil, 1=x("max"), 2=O("min")*/
% Case 1: last slot on grid (right bottom corner), just set it and return. 
initialize_starting_pos_grid(N,N,N):-		
	assert(slot(0,coordinate(N,N),0)),!.

% Case 2: set current slot appropriately and continue recursively 		       
initialize_starting_pos_grid(I,J,N):-
	% case current coordinate is in the center of the board 
	((I =:= N/2, J =:= N/2, !, Val = 1)			% 1 = x
	;
	(I =:= N/2, J =:= (N/2)+1, !, Val = 2)		% 2 = o
	;
	(I =:= (N/2)+1, J =:= N/2, !, Val = 2)		% 2 = o
	;
	(I =:= (N/2)+1, J =:= (N/2)+1, !, Val = 1) 	% 1 = x
	;
	% case not on center of board
	(Val = 0)),									% 0 = empty square			
	
	assert(slot(0,coordinate(I,J),Val)),			% save slot value in memory 		
	get_next_sequential_index(I, J, NewI, NewJ,N),	% get next slot coordinates 
	initialize_starting_pos_grid(NewI,NewJ,N).		% continue recursively 


/* get_next_sequential_index(+I,+J,-NewI,-NewJ,+N) 						  
   calcualte the following coordinate index in the grid in linear order  */
get_next_sequential_index(I, J, NewI, NewJ,N):-
	(I =< N, J < N, !, NewI is I, NewJ is J+1)
	;
	(I < N, J =:= N, !, NewI is I+1, NewJ is 1).


/* get_next_directional_index(+I,+J,-NewI,-NewJ,+Direction) 					
   calcualte following coordinate in the grid in given a specific direction -	
   north,northEast,east,southEast,south,southWest,west,northWest.              */
get_next_directional_index(I,J,NewI,NewJ,Direction):-
	Up is I-1, 
	Down is I+1,
	Right is J+1,
	Left is J-1,
	((Direction = north,!, NewI is Up, NewJ is J)
	;
	(Direction = northEast,!, NewI is Up, NewJ is Right)
	;
	(Direction = east,!, NewI is I, NewJ is Right)
	;
	(Direction = southEast,!, NewI is Down, NewJ is Right)
	;
	(Direction = south,!, NewI is Down, NewJ is J)
	;
	(Direction = southWest,!, NewI is Down, NewJ is Left)
	;
	(Direction = west,!, NewI is I, NewJ is Left)
	;
	(Direction = northWest,!, NewI is Up, NewJ is Left)).


/* print_grid - print given grid to current output stream */
% main printout routine: print_grid(+GridIdentifier) 
print_grid(GridID):-
	dimension(N),				% get dimension 
	nl, tab(6),
	print_grid_header(1,N),		% print grid column headers  
	print_grid(GridID,1,1,N).	% print actual grid 

% internal recursive printout routine: print_grid(+GridId,+I,+J,+N) 
print_grid(GridID,I,J,N):-
	slot(GridID,coordinate(I,J),Val),
	WidthSpace = 1,
	((Val = 0, write('[_]'), tab(WidthSpace))
	;
	 (Val = 1, 	 write('[x]'), tab(WidthSpace))
	;
	 (Val = 2, 	 write('[o]'), tab(WidthSpace))),
	
	(I =:= N, J =:= N, !, nl)	% either last index or continue recursion 
	;
	(get_next_sequential_index(I, J, NewI, NewJ,N),
	((NewI > I,!, nl, write(NewI), 
	  ((NewI < 10,!, tab(4)) ; tab(2)))
	;
	(NewI =:= I)),
	print_grid(GridID,NewI,NewJ,N)).% recursive call 

% header printout routine - print_grid_header(+ColumnIndex,+Dimension)
print_grid_header(J,N):-
	((J =< 10,!, write('['),write(J),write(']'))
	;
	(write('|'),write(J),write('|'))),
	((J =:= 4,!,tab(2)) ; (J >= 10,J =\= 12, !,tab(0)) ; tab(1)),
	((J =:= N, !, nl, write(1), tab(4))			% either last column
	;
	(NewJ is J+1, print_grid_header(NewJ,N))).  % or call recrusively 


/* duplicate_grid(+GridId,-NewGridId) - duplicate given grid with id GridId, 
   and return an id NewGridId of a new separate duplicated grid             */
duplicate_grid(GridID, NewGridID):-
	dimension(N),
	retract(next_idle_grid_id(NewGridID)),	 % retrieve current numerater value 
	NextIdleID is NewGridID + 1,			 % update numerator
	assert(next_idle_grid_id(NextIdleID)),	 % set updated numerator in memory 
	duplicate_grid(GridID, NewGridID,1,1,N). % call internal duplication routine from index (1,1) 

% internal duplication routine: duplicate_grid(+GridID, +NewGridID,+I,+J,+Dimension)
duplicate_grid(GridID, NewGridID,I,J,N):-
	slot(GridID,coordinate(I,J),Val),				% get current index slot value
	assert(slot(NewGridID,coordinate(I,J),Val)),	% copy it to new grid 
	(I =:= N, J =:= N,!)							% either last index 
	;
	(get_next_sequential_index(I, J, NewI, NewJ,N), % or continue recursively  
	 duplicate_grid(GridID, NewGridID,NewI,NewJ,N)).
	 
	 
/* flip_pieces(+GridId,+coordinate(I,J),+Val) - 
   update grid after placing a piece Val on coordinate(I,J) */ 
flip_pieces(GridId,coordinate(I,J),Val):-
	direction_list(DirectionList),		% get possible directions 
	(member(Direction, DirectionList),	% selet one undeterminsticly  
	get_next_directional_index(I,J,NewI,NewJ,Direction), % check this direction further 
	getListOfCoordinatesToFlip(GridId,coordinate(NewI,NewJ),Val,Direction,CoordinatesList),
	flip_list(GridId,Val,CoordinatesList), % actual value update routine 
	fail)	% fail inorder to explore next direction 	
	;
	true.	%return after exploring all directions 


/* getListOfCoordinatesToFlip(+GridId,+coordinate(I,J),+Val,+Direction,-coordinatesList) 
   explore direction and return list of coordinates that need to be flanked (flipped)   */
getListOfCoordinatesToFlip(GridId,coordinate(I,J),Val,Direction,[Head|Tail]):-
	slot(GridId,coordinate(I,J),CurrentVal), 
	CurrentVal =\= 0,!,
	((CurrentVal =:= Val,!, Head = [], Tail = [])
	;
	(Head = coordinate(I,J),
	get_next_directional_index(I,J,NewI,NewJ,Direction),
	getListOfCoordinatesToFlip(GridId,coordinate(NewI,NewJ),Val,Direction,Tail))).
	

/* flip_list(+GridId,+Val,+ListOfCoordinatesNeededToBeFlipped)
   flip current list coordinate in grid to value Val.        */
flip_list(_,_,[]):-!.						% Case1: empty list, nothing to flip
flip_list(GridId,Val,[Coordinate|Tail]):-	% Case2: non-empty list 
	retract(slot(GridId,Coordinate,_)),		% erase old value from memory 
	assert(slot(GridId,Coordinate,Val)),	% save new value to memory
	flip_list(GridId,Val,Tail).				% continue recursively 
	 

/* makeLegalMove(+coordinate(I,J), +pos(Player1,Grid1Id), -pos(Player2,Grid2Id)) 
   check if Player1 can place a piece on coordinate(I,J).                        
   if so, return new position with Grid2Id and Player2 to move 					*/
makeLegalMove(coordinate(I,J), pos(Grid1Id,Player1,_), pos(Grid2Id,Player2,coordinate(I,J))):-
	% intialize coordinates incase they are not intialized yet 
	((not(var(I)), not(var(J)),!)
	;
	slot(Grid1Id,coordinate(I,J),0)),
	(slot(Grid1Id,coordinate(I,J),0), 	% validate requested slot is idle 
	validate_coordinate(Grid1Id,coordinate(I,J),Player1), % check if move is legal 
	duplicate_grid(Grid1Id,Grid2Id),	% create grid for new position 
	retract(slot(Grid2Id,coordinate(I,J),_)),
	assert(slot(Grid2Id,coordinate(I,J),Player1)),
	flip_pieces(Grid2Id,coordinate(I,J),Player1),
	((Player1 =:= 1, Player2 is 2) ; (Player1 =:= 2, Player2 is 1))). % alternate turn 
	

/* validate_coordinate(+Grid1Id,+coordinate(I,J),+RequestedVal)       
   check if move is legal of placing RequestedVal on coordinate(I,J) */
validate_coordinate(GridId,coordinate(I,J),RequestedVal):-	
	% initialize all possible slot coordinates in all directons 
	direction_list(DirectionList),		% get all possible directions 
	member(Direction, DirectionList),	% selet one undeterminsticly 
	get_next_directional_index(I,J,NewI,NewJ,Direction), % explore direction 
	slot(GridId,coordinate(NewI,NewJ),Val),	% get current value of this slot 
	Val =\= 0,						%  slot is not idle 
	abs(Val-RequestedVal) =:= 1, 	%  slot is opposite color 
	validate_direction_recursively(GridId,coordinate(NewI,NewJ),RequestedVal,Direction).
	
% validate_direction_recursively(+GridId,coordinate(I,J),+RequestedVal,+Direction) 
% internal helper validation routine for recrusion usage 
validate_direction_recursively(GridId,coordinate(I,J),RequestedVal,Direction):-
	get_next_directional_index(I,J,NewI,NewJ,Direction),
	slot(GridId,coordinate(NewI,NewJ),Val),
	Val =\= 0,	% assure slot is not idle
	((Val =:= RequestedVal,!)	
	;	
	(validate_direction_recursively(GridId,coordinate(NewI,NewJ),RequestedVal,Direction))).

% collector routine for all possible legal continuation coordinates 
% get_legal_coordinates(+GridId,+Player,-LegalCoordinatesList) 
get_legal_coordinates(Id,Player,Coordinates):-
	setof((I,J),(slot(Id,coordinate(I,J),0),validate_coordinate(Id,coordinate(I,J),Player)),Coordinates).
	

/****************************************************************************
*______________________________ AlphaBeta module	___________________ _____
*****************************************************************************/%.

/* max_to_move(+POS), min_to_move(+POS) 						   */
max_to_move(pos(_,1,_)). % true iff it's MAX turn to move  (x player) 
min_to_move(pos(_,2,_)). % true iff it's MIN turn to move  (o player) 


/***********************************************************************
* moves(+Pos,-PosList,+DepthLevel,+MaxDepth)                   		   *
* PosList is a list of all possible legal successor posistion of  Pos. *
* DepthLevel is current depth level, MaxDepth is max depth level       *
**********************************************************************/%.
moves(Pos,PosList,DepthLevel,MaxDepth):-	%just collect results from makeLegalMove predicate
	DepthLevel =< MaxDepth,
	% just collect all possible legal moves - 
	setof(pos(Id,Player,coordinate(I,J)),makeLegalMove(coordinate(I,J),Pos,pos(Id,Player,coordinate(I,J))),PosList). 

/**************************************************************************
* alphabeta(+Pos,+Alpha,+Beta,-GoodPos,-Val,+DepthLevel,+MaxDepth,+Level)         			          *
* This code is based on figure 24.5 implementation alphabeta algorithm*
* Pos is a pos, Val is it's minimax value, best move from Pos leads  *
* to GoodPos, Alpha is the minimal value that MAX is guaranteed to achieve*
* Beta is the maximal value that MAX can hope to achieve. 				  *
* First call is with alpha = -infinity & Beta with +infinity.             *
*************************************************************************/%.
alphabeta(CurrentPos,Alpha,Beta,BestSuccessorPos,Val,DepthLevel,MaxDepth,Level):-
	% check if results already exists on database 
	ActualDepth is MaxDepth - DepthLevel,
	CurrentPos = pos(Grid,_,_),
	get_hash_key(Grid,HashKey),
	((pos_evaluation(HashKey,ActualDepth,BestSuccessorPos,Val),!)
	;
	% if not, calculate them from scratch 
	(((nonvar(Alpha),nonvar(Beta)) ; (Alpha is -999999, Beta is 999999)), % init +-infinity 
	 moves(CurrentPos,PosList,DepthLevel,MaxDepth),!,
	 NewDepthLevel is DepthLevel+1,	% update depth level 
	 boundedbest(PosList,Alpha,Beta,BestSuccessorPos,Val,NewDepthLevel,MaxDepth,Level),
	 % save result to database for saving calculaation runtime in the future
	 assert(pos_evaluation(HashKey,ActualDepth,BestSuccessorPos,Val)))  
	;
	staticval(CurrentPos,Val,Level)). % terminal seatchtree node - evaluate directly  

/* find a 'good enough' pos GoodPos in the list PosList */%.
/* so that the backed-up value Val of GoodPos is a good enough approximation */%.
/* with respect to Alpha and Beta */
boundedbest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal,DepthLevel,MaxDepth,Level):-
	alphabeta(Pos,Alpha,Beta,_,Val,DepthLevel,MaxDepth,Level),
	goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel,MaxDepth,Level).

goodenough([],_,_,Pos,Val,Pos,Val,_,_,_):- !.	% No other candidate

goodenough(_,Alpha,Beta,Pos,Val,Pos,Val,_,_,_):-
	min_to_move(Pos),Val > Beta, !          % Maximizer attained upper bound
	;
	max_to_move(Pos),Val < Alpha, !.        % Minimizer attained lower bound

goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,DepthLevel,MaxDepth,Level):-
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta),   % Refine bounds  
	boundedbest(PosList,NewAlpha,NewBeta,Pos1,Val1,DepthLevel,MaxDepth,Level),
	betterof(Pos,Val,Pos1,Val1,GoodPos,GoodVal).

/* define new interval by 2 last arguments that is narrower or equal to old interval */ 
newbounds(Alpha,Beta,Pos,Val,Val,Beta):-
	min_to_move(Pos),Val > Alpha, !.       % Maximizer increased lower bound 

newbounds(Alpha,Beta,Pos,Val,Alpha,Val):-
   max_to_move(Pos),Val < Beta, !.         % Minimizer decreased upper bound 

newbounds(Alpha,Beta,_,_,Alpha,Beta).      % Otherwise bounds unchanged 

/* find the better pos continuation and value from within the 2 alternatives */ 
% betterof(Pos,Val,Pos1,Val1,Pos,Val)     % Pos better than Pos1 
betterof(Pos,Val,_,Val1,Pos,Val):-      % Pos better than Pos1 
	min_to_move(Pos),Val > Val1, !
	;
	max_to_move(Pos), Val < Val1, !.

betterof(_,_,Pos1,Val1,Pos1,Val1).         % Otherwise Pos1 better


/**************************************************************
* staticVal(+Pos,-Val,+Level)	                              *
* Val is the static value of a terminal (leaf) posistion Pos  *
* Level is game level: 1=begginer, 2=intermediate, 3=advanced *
***************************************************************/
staticval(pos(GridId,_,_),Val,Level):-
	% begginer level, only count pieces 
	(Level =:= 1,!,							
	 pieces_count_evaluation(GridId,Val,_,_))
	;
	% intemediate level, count pieces & approximate mobility 
	(Level =:= 2,!,
	 pieces_count_evaluation(GridId,CountVal,_,_),
	 mobility_evaluation(GridId,MobilityVal),
	 Val is (0.4 * CountVal) + (0.6 * MobilityVal))
	;
	% advanced level, count pieces & approximate mobility & check corners 
	(Level =:= 3,!,
  	 pieces_count_evaluation(GridId,CountVal,_,_),
	 mobility_evaluation(GridId,MobilityVal),
	 corners_evaluation(GridId,CornersVal),
	 Val is (0.25 * CountVal) + (0.35 * MobilityVal) + (0.4 * CornersVal)).

/* Heurstic evaluation function #1
   pieces_count_evaluation(+GridId,-Val,+MaxCount,+MinCount) 
   compare amount of pieces each player has on board 		*/
pieces_count_evaluation(GridId,Val,MaxCount,MinCount):-
	pieces_count(GridId,MaxCount,0,MinCount,0,1,1),
	TotalCount is MaxCount + MinCount,
	Val is (MaxCount - MinCount) / TotalCount.

% internal recursive accumulative helper routine 
pieces_count(Id,MaxTotal,MaxTemp,MinTotal,MinTemp,I,J):-
	slot(Id,coordinate(I,J),CurrentVal), 
	((CurrentVal =:= 0,!, NewMaxTemp is MaxTemp, NewMinTemp is MinTemp)
	;
	(CurrentVal =:= 1,!, NewMaxTemp is MaxTemp+1, NewMinTemp is MinTemp)
	;
	(CurrentVal =:= 2,!, NewMinTemp is MinTemp+1, NewMaxTemp is MaxTemp)),
	dimension(N),
	(((I =:= N, J =:= N,!, MaxTotal is NewMaxTemp, MinTotal is NewMinTemp))
	;
	(get_next_sequential_index(I,J,NewI,NewJ,N),
	pieces_count(Id,MaxTotal,NewMaxTemp,MinTotal,NewMinTemp,NewI,NewJ))).

/* Heurstic evaluation function #2
   mobility_evaluation(+Grid,-Val) 
   compare the number of possible moves each player has 		  */ 
mobility_evaluation(Grid,Val):-
	% get max player possible moves
	((((get_legal_coordinates(Grid,1,MaxMoves),!, length(MaxMoves,MaxCount)) 
	 ;
	 MaxCount is 0),
	 
	 % get min player possible moves 
	((get_legal_coordinates(Grid,2,MinMoves),!, length(MinMoves,MinCount))
	 ;
	 MinCount is 0)),

	% compare 
	(Delta is MaxCount - MinCount,
	TotalCount is MaxCount + MinCount,
	((TotalCount =:= 0,!, Val is 0) ; (Val is Delta / TotalCount)))).

/* Heurstic evaluation function #3
   corners_evaluation(+Grid,-Val) 
   compare the number of captured cornes by each player	 */ 
corners_evaluation(GridId,Val):-
	% get corner values  
	dimension(N),
	slot(GridId,coordinate(1,1),C1),
	slot(GridId,coordinate(1,N),C2),
	slot(GridId,coordinate(N,1),C3),
	slot(GridId,coordinate(N,N),C4),
	
	% assign each corner a grade 1/4 according to it's value (empty\max\min)
	((C1 =:= 0,!,C1Val is 0) ; (C1 =:= 1,!,C1Val is 0.25) ; (C1Val is -0.25)),
	((C2 =:= 0,!,C2Val is 0) ; (C2 =:= 1,!,C2Val is 0.25) ; (C2Val is -0.25)),
	((C3 =:= 0,!,C3Val is 0) ; (C3 =:= 1,!,C3Val is 0.25) ; (C3Val is -0.25)),
	((C4 =:= 0,!,C4Val is 0) ; (C4 =:= 1,!,C4Val is 0.25) ; (C4Val is -0.25)),
	
	% sum result of all corners 
	Val is C1Val+C2Val+C3Val+C4Val.	
	
/* get_hash_key(+Grid,-HashKey) 		                                 
   return a hash key for a given grid id to manage it's state in memory */
get_hash_key(Grid,HashKey):-
	dimension(N),
	get_hash_key(Grid,GridFlatList,1,1,N),	% call internal recursive routine 
	name(HashKey,GridFlatList).				% convert list into a key 
			
get_hash_key(Grid,[Val],N,N,N):-			% case 1 of internal routine: last index 
	slot(Grid,coordinate(N,N),Val),!.
	
get_hash_key(Grid,[Head|Tail],I,J,N):-		% case 2 of internal routine: keep recursing  	
	slot(Grid,coordinate(I,J),Head),!,
	get_next_sequential_index(I,J,NewI,NewJ,N),
	get_hash_key(Grid,Tail,NewI,NewJ,N).
	

/****************************************************************************
*__________________Game application & I\O interaction module________________*
***************************************************************************/%.
% main program to initiate entire application: run/0
run:-
	print_welcome_message,
	get_user_name(PlayerName),
	print_greeting_message(PlayerName),
	get_board_dimension(N),
	get_game_mode(Mode),	
	get_game_level(Level),
	initialize_board(N),
	print_starting_pos,
	
	% play against computer 
	((Mode =< 2, play_interactive_game(Mode,Level,pos(0,1,_))	
	 ;
	 % watch automatic game computer vs computer 
	 Mode =:= 3, play_automatic_game(Level,pos(0,1,_)))
	;
	% user quit game explictly 
	(user_exited_game,!, print_goodbye_message(PlayerName), cleanup)
	;
	% game finished normally 
	(end_of_game(FinalGrid),
	 print_game_results(Mode,FinalGrid,PlayerName),
	 print_goodbye_message(PlayerName),
	 cleanup)).	
				
	
/*************************************************************************
* Automatic game AI vs AI                                                 
* play_automatic_game(+Level, +pos(Grid1,Computer1,_))                    
*************************************************************************/%.
play_automatic_game(Level,pos(Grid1,Computer1,_)):-
	% incase current player has a legal move 
	get_legal_coordinates(Grid1,Computer1,_),!,
	get_max_depth(Level,MaxDepth),
	alphabeta(pos(Grid1,Computer1,_),_,_,Pos2,_,0,MaxDepth,Level), % get best move 
	Pos2 = pos(Grid2,Computer2,coordinate(I2,J2)),
	nl,write('Computer'),write(Computer1),write(' plays ('),
	write(I2),write(','),write(J2),write(').'),
	nl, write('Current game position after placing a piece on this slot -'),
	print_grid(Grid2),sleep(1.5),
	play_automatic_game(Level, pos(Grid2,Computer2,_))	% alternate turn
	;
	% incase current player has no legal move, alternate turn 
	get_other_player(Computer1,Computer2),
	get_legal_coordinates(Grid1,Computer2,_),!,
	nl,write('Computer'),write(Computer1),write(' has no legal moves.'),
	play_automatic_game(Level, pos(Grid1,Computer2,_))
	;
	% both player have no legal move - end game 
	assert(end_of_game(Grid1)),!,nl,write('End of Game'), fail.


/*************************************************************************
* Interactive game Human vs AI                                            
*************************************************************************/%.
play_interactive_game(Mode,Level,pos(GridId,Player1,_)):-	
	% assure current player has a legal move  
	(get_legal_coordinates(GridId,Player1,ValidCoordinates),
	 retractall(player_stuck(_)),
	 
	((%user to move 
	 Mode =:= Player1,!,	
	 get_coordinate_from_user(Player1,ValidCoordinates,UserCoordinate),
	 makeLegalMove(UserCoordinate,pos(GridId,Player1,_),pos(NewId,Player2,_)))
	;		
	(%computer to move
	 get_max_depth(Level,MaxDepth),
	 !,alphabeta(pos(GridId,Player1,_),_,_,pos(NewId,Player2,coordinate(I,J)),_,0,MaxDepth,Level),
	 print_computer_move(I,J))),
	
	 % print updated position and shift controll to next player  
	 (not(end_of_game(_)),not(user_exited_game),nl,
	  write('current position after placing a piece on this slot -'),
	  print_grid(NewId), sleep(1),
	  ((Mode =:= Player1,!, print_a_compliment,!, sleep(1)) ; (sleep(0.75))),
	  play_interactive_game(Mode,Level,pos(NewId,Player2,_))))  % continue to next round 
	;
	
	% incase current player is stuck skip him or end game 
	(assert(player_stuck(Player1)),
		get_other_player(Player1,Player2),
		not(end_of_game(_)),not(user_exited_game),
		
		% if both players are stuck, end game 
		(((player_stuck(Player2),!,print_no_moves_message,assert(end_of_game(GridId)),fail))
		;
		
		% else, if other player has a move, shift the play to him  
		(print_skip_turn_message(Mode,Player1),
		 play_interactive_game(Mode,Level,pos(GridId,Player2,_))))).


/*************************************************************************
* Game Utillity Routines                                                 *
*************************************************************************/%.

/* get_other_player(+Player1,-Player2) */ 
get_other_player(Player1,Player2):-
	(Player1 =:= 1,!, Player2 is 2)
	;
	(Player1 =:= 2, Player2 is 1).
	
/* get_max_depth(+Level,-MaxDepth) : depth for alphabeta according to game level*/
get_max_depth(Level,MaxDepth):-
	((Level =:= 1,!, MaxDepth = 1)	% beginner 
	;
	(Level =:= 2,!, MaxDepth = 3)	% intemediate 
	;
	(Level =:= 3,!, MaxDepth = 5)).	% advanced 

/* user_exit(+X) - check if user requested to quit. if so, turn on appropriate flag */
user_exit(X):-
	(nonvar(X), (X = 'EXIT' ; X = exit), assert(user_exited_game)).
	
	
/*************************************************************************
* I\O Routines                                                           *
*************************************************************************/%.

/* get_user_input(-InputString) : main input routine                           
   get next line from input stream, return first token, drop rest of the line  */
get_user_input(InputString):-
	get(C),						% get first readable character (ignore blanks etc) 
	parse_rest_of_line(Rest),   % get rest of line 
	append([C],Rest,InputCharacterList), 	
	name(InputString,InputCharacterList). 
	
% recursive helper parser routine: parse_rest_of_line(-Result) 
parse_rest_of_line(Result):-
	get0(C), 
	% stop if we reached end of line 
	((C = 10,!, Result = [])		% 10 = '\n'		
	;	
	% if we reached end token (first whitespace), drop rest of line
	(C = 32,!,Result = [], drop_rest_of_line)	% 32 = space  
	;
	% else, just collect current chacter and continue parsing 
	(Result = [C|Rest], parse_rest_of_line(Rest))).
	 
% util helper to drop rest of line past first token - drop_rest_of_line/0
drop_rest_of_line:-
	get0(C), ((C = 10,!) ; (drop_rest_of_line)). % just read until '\n' character 


/* get_user_name(-PlayerName) */
get_user_name(PlayerName):-
	write('What is your name? '),
	get_user_input(PlayerName),nl.


/* get_board_dimension(+N) */ 
get_board_dimension(N):-
	nl, write('Let''s set the game''s board dimension - '),nl, 
	repeat, 
	write('Please enter an even number greater or equal to four: '),
	get_user_input(N),
	((integer(N),N >= 4,(0 =:= N mod 2),!)	% validate input 
	 ;
	 (user_exit(N),!, fail)					% user wishes to quit 
	 ;
	 print_invalid_input_message(N), fail). % invalid input  


/* get_game_level(+L) L = Level of game: 1=begginer,2=intermediate,3=advanced 
   according to level of game, computer will decide how deep to search and 
   which heurstics estimates to do, the higher level, the harder it is to beat the PC */
get_game_level(L):-
	nl, write('Okay. Let''s set the game''s level - '),nl,
	repeat, 
	write('Please enter a number between 1 to 3 as follows: '),nl,
	write('1 = Beginner'),nl,	
	write('2 = Intermediate'),nl,
	write('3 = Advanced'),nl, 
	get_user_input(L), 
	((integer(L), L >= 1,L =< 3,!)	% validate input	
	 ;
	 (user_exit(L),!, fail)			% user wishes to quit
	 ;
	 (print_invalid_input_message(L), fail)). % invalid input  
	
	
/* get_game_mode(+M) M = mode of game: 1=human starts, 2=PC starts, 3=PC vs PC 
   the player who starts is the Max player, he playes with x's.                
   the second player is the Minimum player, he playes with o's.               */
get_game_mode(M):-
	nl, write('Okay. Let''s set the game''s mode - '),nl,
	repeat, 
	write('Please enter a number between 1 to 3 as follows: '),nl,
	write('1 = Interactive game against me, YOU go first, you play with "x" pieces.'),nl, 
	write('2 = Interactive game against me, but on this mode I get to go first, you play with "o" pieces.'),nl,
	write('3 = Autmatic game between me and my self (AI vs AI)'),nl, 
	get_user_input(M),
	((integer(M), M >= 1, M =< 3, !)	% validate input	
	 ;
	 (user_exit(M),!, fail)				% user wishes to quit
	 ;
	 (print_invalid_input_message(M), fail)).	% invalid input  


/* get_coordinate_from_user(+Player, +ValidCoordinates,-Coordinate) 
   get from user the requested coordinate to place a piece on and validate it */
get_coordinate_from_user(Player, ValidCoordinates,Coordinate):-
	nl, write("It's your turn to move."),nl,
	((Player =:= 1,!, write('You need to place an "x" on the board'))
	;
	(write('You need to place an "o" on the board'))),nl,
	repeat, 
	prompt_user_to_move(ValidCoordinates),
	get_user_input(UserInput),
	
	% validate input with ascii: 44=Comma, 40&41=left &right closures i.e.,()
	((name(UserInput,[40,X,44,Y,41|_]),  
	  name(I,[X]),name(J,[Y]),member((I,J),ValidCoordinates),!,Coordinate=coordinate(I,J))
	 ;
	  % first coordinate has two digits 
	 (name(UserInput,[40,X,Y,44,Z,41|_]),  
	  name(I,[X,Y]),name(J,[Z]),member((I,J),ValidCoordinates),!,Coordinate=coordinate(I,J))
	 ;
	  % second coordinate has two digits 
	  (name(UserInput,[40,X,44,Y,Z,41|_]), 
	   name(I,[X]),name(J,[Y,Z]),member((I,J),ValidCoordinates),!,Coordinate=coordinate(I,J))
	 ;
	  % both coordinates have two digits 
	  (name(UserInput,[40,X,Y,44,Z,W,41|_]), 
	   name(I,[X,Y]),name(J,[Z,W]),member((I,J),ValidCoordinates),!,Coordinate=coordinate(I,J))
	 ;
	 (user_exit(UserInput),!, fail)	% user wishes to quit
	 ;
	 print_invalid_input_message(UserInput), fail).	% invalid input  


/*************************************************************************
* Printing Routines                                                      *
*************************************************************************/%.

/* print_welcome_message */ 
print_welcome_message:-
	write('Welcome to this super cool prolog othello game application!'),nl.
	
	
/* print_greeting_message(+PlayerName) */
print_greeting_message(PlayerName):-
	write('Hello '), write(PlayerName), write('!'),nl,sleep(1),
	write('This application enables a few possible different game settings.'),nl,sleep(1),
	write('Inorder to gain best game experience, '),nl,sleep(1),
	write('select your preferences as instructed below.'),nl,sleep(1),
	write('Please note: If by any stage you wish to quit, just type in ''EXIT'' or ''exit''.'),nl, sleep(1),
	nl,write('Okay, first things first... '), nl, sleep(1).


/* print_starting_pos */ 
print_starting_pos:-
	nl, write('Starting position:'), 
	print_grid(0).	% id 0 is id of initial grid.


/* print_goodbye_message(+PlayerName) */ 
print_goodbye_message(PlayerName):-
	 write(PlayerName),write(', It has been a pleasure, bye for now.'),
	 nl, write('May the force be with you!').


/* print_invalid_input_message(+InvalidInput) */
print_invalid_input_message(Input):-
	nl, write('Sorry, the input "'), write(Input), 
	write('" is invalid. Let''s try again - '),nl,nl.


/* print_computer_move(+I,+J) - inform user what the computer played */
print_computer_move(I,J):-
	nl, write('My artificial intelligence guides me to play ('),
	write(I),write(','),write(J),write(').').


/* print_skip_turn_message(+Mode,+PlayerNum) - inform user turn is being skipped */
print_skip_turn_message(Mode,PlayerNum):-
	Mode =:= PlayerNum, !, 
	 nl, write('I am sorry, you have no legal moves, therefore your turn is skipped. ')
	;
	(nl, write('Oh shoot!, I''m stuck without any legal moves, my turn is skipped.'),nl).
	
	
/* print_no_moves_message */ 
print_no_moves_message:-
	nl,write('Apparently, we both have no legal moves left,'),nl,
	write('therefore this game has come to it''s end.').
	
	
/* prompt_user_to_move(+ValidCoordinates) */
prompt_user_to_move(ValidCoordinates):-
	write("Enter the requested slot in the format (I,J),"),nl,
	write("where I & J stand for the row & column coordinate indices, respectively."),nl,
	write("The possible options are: "),
	write(ValidCoordinates),nl,
	write('If you feel like cutting early, just type "exit" or "EXIT"'),nl.


/* print_a_compliment - print a random compliment to the user after he makes a move */
print_a_compliment:-
	build_compliment_list(List),	
	%	permutation(List,MixedList),% mix order of list to achieve randomness 
	%	member(Temp,MixedList),!,	% select some compliment un-deterministiclly 
	random_member(Compliment,List),!, 
	nl,write(Compliment),nl.		

% build compliment list 
build_compliment_list(List):-
	A = 'Hmmm, not bad, not bad at all.',
	B = 'Wow, you are pretty good',
	C = 'God! didn''t see that coming!',
	D = 'sharp move! are you using an alphabeta algrorithm your self to calculate moves?',
	E = 'Gosh, you have skills!',
	F = 'I like your way of thinking. How long have you been practicing this game?',
	G = 'Oh my, I am afraid you going rip me apart',
	H = 'Jesus, Don''t be that hard on me, alright?',
	I = 'Eh, let me see think this through...',
	J = 'Wait a second, are you trying to set me a trap?',
	K = 'You are killing me with your talented moves, didn''t you say you are a rookie?',
	List = [A,B,C,D,E,F,G,H,I,J,K].
	
	
/* print_game_results(+Mode,+FinalGrid,+PlayerName)      */
print_game_results(Mode,FinalGrid,PlayerName):-
	% get pices statistics 
	pieces_count_evaluation(FinalGrid,_,MaxCount,MinCount),
	Sum is MaxCount + MinCount,
	nl,nl, write('Total pieces on board: '),write(Sum),
	nl,write('Total X''s on board: '), write(MaxCount),
	nl,write('Total O''s on board: '), write(MinCount),nl,nl,
	
	% draw 
	(((MaxCount =:= MinCount),!, write('It''s a Draw! Wow, great game!'),nl)
	;
	
	% computer vs computer 
	(Mode =:= 3,!,  
	 ((MaxCount > MinCount,!, write('Computer1 won. Wow, great game!'),nl)
	 ;	
	 (write('Computer2 won. Wow, great game!'),nl)))
	;
	
	% human va computer 
	(((Mode =:= 1, MaxCount > MinCount) 	% human win 
	 ;
	 (Mode =:= 2, MaxCount < MinCount)),!,
	 write('You won '), write(PlayerName), 
	 write('! Wow, great game!'),nl)
	;										% computer win 
	(write('Sorry '),write(PlayerName),write('. I won this time,'),
	 nl,write('but i''ll tell you the truth it was not easy.'),
	 nl,write('Maybe next time you''ll get a chance to strike back.'),nl,nl)).
	 