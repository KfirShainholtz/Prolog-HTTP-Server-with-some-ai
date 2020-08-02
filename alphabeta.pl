:- module(alphabeta, [alphabeta/7]).
:- use_module(game).
:- use_module(library(http/http_session)).

alphabeta(Board, _, _, _, _, Val, 0) :-
	staticval(Board, Val), !.

% alphabeta pruning from the book, with additional caching
% of a good board if existed before, or visited in the alphabeta alogrithm
alphabeta(Board, Player, Alpha, Beta, GoodBoard, Val, Depth) :-
	moves(Board, Player, Boards),
	(
		length(Boards, 0), staticval(Board, Val);
		other_player(Player, Enemy),
		!,
		boundedbest(Boards, Enemy, Alpha, Beta, GoodBoard, Val, Depth)
	).

% bounded best from the book
boundedbest([Board | Boards], Player, Alpha, Beta, GoodBoard, GoodVal, Depth) :-
	DepthP is Depth - 1,
	alphabeta(Board, Player, Alpha, Beta, _, Val, DepthP),
	goodenough(Boards, Player, Alpha, Beta, Board, Val, GoodBoard, GoodVal, Depth).

goodenough([], _, _, _, Board, Val, Board, Val, _) :-
	!.
goodenough(_, 1, _, Beta, Board, Val, Board, Val, _) :-
	Val < Beta, !.
goodenough(_, 2, Alpha, _, Board, Val, Board, Val, _) :-
	Val > Alpha, !.
goodenough(Boards, Player, Alpha, Beta, Board, Val, GoodBoard, GoodVal, Depth) :-
	newbounds(Alpha, Player, Beta, Board, Val, NewAlpha, NewBeta),
	boundedbest(Boards, Player, NewAlpha, NewBeta, Board1, Val1, Depth),
	betterof(Board, Player, Val, Board1, Val1, GoodBoard, GoodVal).

newbounds(Alpha, 1, Beta, _, Val, Val, Beta) :-
	Val > Alpha, !.
newbounds(Alpha, 2, Beta, _, Val, Alpha, Val) :-
	Val < Beta, !.
newbounds(Alpha, _, Beta, _, _, Alpha, Beta).

betterof(Board, 1, Val, _, Val1, Board, Val) :-
	Val > Val1, !.
betterof(Board, 2, Val, _, Val1, Board, Val) :-
	Val < Val1, !.
betterof(_, _, _, Board, Val1, Board, Val1).

% get moves from cache
% or find moves - when eat available - eat,
% else, find all regular moves
moves(Board, Player, NewBoards) :-
	(
			setof(NewBoard, possible_eat_move(Player, Board, NewBoard), NewBoards), length(NewBoards, L), L > 0
			;
			setof(NewBoard, possible_move(Player, Board, NewBoard), NewBoards)
	).

% heuristics function for the given board;
% get static val from cache, or compute:
% 1. Count each player regular pawns
% 2. Count each player king pawns
% 3. Count each player adjacent pawns (those can't be eaten and make the game harder for the opponent)
% 4. Calculate each player's (regular pawns * 3) + (king pawns * 4) + (player adjacents * 1.5)
% 5. Deduct player's result from the computer's result.
staticval(Board, Res) :-
	%(retract(s(Board, Res)),
	%asserta(s(Board, Res))
	%;
	Computer = 2,
	Human = 1,
	count_player_checkers(Board, Computer, PiecesCC),
	count_player_checkers(Board, Human, PiecesHC),
	count_player_kings(Board, Computer, KingsCC),
	count_player_kings(Board, Human, KingsHC),	
	count_adjacents(Board, Computer, AdjacentsCC),
	count_adjacents(Board, Human, AdjacentsHC),
	Res is (PiecesCC * 1 + (KingsCC * 2) + (AdjacentsCC * 0.5)) - (PiecesHC * 1 + (KingsHC * 2) + (AdjacentsHC * 0.5)).
	%asserta(s(Board, Res))).