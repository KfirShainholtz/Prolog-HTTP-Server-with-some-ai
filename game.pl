:- module(game, [possible_eat_move/3, possible_move / 3, other_player / 2, count_player_checkers / 3, count_player_kings / 3, count_adjacents / 3]).
:- use_module(library(lists)).

% get possible eat moves for a player (multi-eat is one move that contains multiple removals)
possible_eat_move(Player, Board, NewBoard) :-
    member(checker(Player, IsKing, Row, Col, false), Board),
    get_eat_moves(checker(Player, IsKing, Row, Col, false), false, Board, NewBoard).

% get regular possible move for a player
possible_move(Player, Board, NewBoard) :-
    member(checker(Player, IsKing, Row, Col, false), Board),
    get_checker_move(checker(Player, IsKing, Row, Col, false), Board, NewBoard).

% regular move for a king, or the human player, get adjacents and move top-right if the location is empty
get_checker_move(checker(Player, IsKing, Row, Col, IsRemoved), Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true
        ),
    get_adjacents(Row, Col, TopRow, RightCol, _, _),
    get_single_moves(checker(Player, IsKing, Row, Col, IsRemoved),Board, TopRow, RightCol, NewBoard).

% regular move for a king, get adjacents and move top-left if the location is empty
get_checker_move(checker(Player, IsKing, Row, Col, IsRemoved), Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true
        ),
    get_adjacents(Row, Col, TopRow, _, _, LeftCol),
    get_single_moves(checker(Player, IsKing, Row, Col, IsRemoved),Board, TopRow, LeftCol, NewBoard).

% regular move for a king, or the computer player, get bottom-left adjacents and move if the location is empty
get_checker_move(checker(Player, IsKing, Row, Col, IsRemoved), Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true
        ),
    get_adjacents(Row, Col, _, _, BottomRow, LeftCol),
    get_single_moves(checker(Player, IsKing, Row, Col, IsRemoved),Board, BottomRow, LeftCol, NewBoard).

% regular move for a king, or the computer player, get adjacents and move bottom-right if the location is empty
get_checker_move(checker(Player, IsKing, Row, Col, IsRemoved), Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true
        ),
    get_adjacents(Row, Col, _, RightCol, BottomRow, _),
    get_single_moves(checker(Player, IsKing, Row, Col, IsRemoved),Board, BottomRow, RightCol, NewBoard).

% get single moves for a player
% check if the adjacent is valid (0-8 col/row)
% move piece over the available adjacent
get_single_moves(checker(Player, IsKing, Row, Col, IsRemoved),Board, NewRow, NewCol, NewBoard) :-
    can_do_adjacent(Board, NewRow, NewCol),
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, NewRow, NewCol, IsRemoved), NewBoard).

% get eat moves for the human player, or a king, or a multi eat
% get adjacents
% figure if enemy is in radius (any radius is valid if multi or king)
% move piece
% eat the enemy piece
% get eat moves (try eat more, backtrack if not available)
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved), IsMulti, Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, TopRow, RightCol, _, _),
    is_enemy_in_radius(Player, TopRow, RightCol, Board),
    get_adjacents(TopRow, RightCol, TopperRow, RighterCol, _, _),
    can_do_adjacent(Board, TopperRow, RighterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, TopperRow, RighterCol, IsRemoved), Temp),
    eat_piece(Temp, TopRow, RightCol, Temp2),
    get_eat_moves(checker(Player, IsKing, TopperRow, RighterCol, IsRemoved), true, Temp2, NewBoard).

% eat moves for human player, king, or multi eat, to top-right adjacent
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved), IsMulti, Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, TopRow, RightCol, _, _),
    is_enemy_in_radius(Player, TopRow, RightCol, Board),
    get_adjacents(TopRow, RightCol, TopperRow, RighterCol, _, _),

    can_do_adjacent(Board, TopperRow, RighterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, TopperRow, RighterCol, IsRemoved), Temp),
    eat_piece(Temp, TopRow, RightCol, NewBoard).

% eat moves for human player, king, or multi eat, to top-left adjacent - multi eat if available
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved), IsMulti, Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, TopRow, _, _, LeftCol),
    is_enemy_in_radius(Player, TopRow, LeftCol, Board),
    get_adjacents(TopRow, LeftCol, TopperRow, _, _, LefterCol),
    can_do_adjacent(Board, TopperRow, LefterCol),  % if can continue eating, call it. maybe 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, TopperRow, LefterCol, IsRemoved), Temp),
    eat_piece(Temp, TopRow, LeftCol, Temp2),
    get_eat_moves(checker(Player, IsKing, TopperRow, LefterCol, IsRemoved), true, Temp2, NewBoard).

% eat moves for human player, king, or multi eat, to top-left adjacent
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved),IsMulti, Board, NewBoard) :-
    (
            Player =:= 1;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, TopRow, _, _, LeftCol),
    is_enemy_in_radius(Player, TopRow, LeftCol, Board),
    get_adjacents(TopRow, LeftCol, TopperRow, _, _, LefterCol),
    can_do_adjacent(Board, TopperRow, LefterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, TopperRow, LefterCol, IsRemoved), Temp),
    eat_piece(Temp, TopRow, LeftCol, NewBoard).

% eat moves for computer player, king, or multi eat, to top-right adjacent, try eat multiple
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved), IsMulti, Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true;
            IsMulti == true
    ),
    get_adjacents(Row, Col, _, RightCol, BottomRow, _),
    is_enemy_in_radius(Player, BottomRow, RightCol, Board),
    get_adjacents(BottomRow, RightCol, _, RighterCol, LowerRow, _),
    can_do_adjacent(Board, LowerRow, RighterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, LowerRow, RighterCol, IsRemoved), Temp),
    eat_piece(Temp, BottomRow, RightCol, Temp2),
    get_eat_moves(checker(Player, IsKing, LowerRow, RighterCol, IsRemoved), true, Temp2, NewBoard).

% get eat move for the computer player, or a player that is multi eating - and eat once.
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved),IsMulti, Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, _, RightCol, BottomRow, _),
    is_enemy_in_radius(Player, BottomRow, RightCol, Board),
    get_adjacents(BottomRow, RightCol, _, RighterCol, LowerRow, _),
    can_do_adjacent(Board, LowerRow, RighterCol),  % if can continue eating, call it. maybe 
    % eat_piece
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, LowerRow, RighterCol, IsRemoved), Temp),
    eat_piece(Temp, BottomRow, RightCol, NewBoard).
    
% get eat move for the computer player, or a player that is multi eating - and eat multiple.
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved), IsMulti, Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, _, _, BottomRow, LeftCol),
    is_enemy_in_radius(Player, BottomRow, LeftCol, Board),
    get_adjacents(BottomRow, LeftCol, _, _, LowerRow, LefterCol),
    can_do_adjacent(Board, LowerRow, LefterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, LowerRow, LefterCol, IsRemoved), Temp),
    eat_piece(Temp, BottomRow, LeftCol, Temp2),
    get_eat_moves(checker(Player, IsKing, LowerRow, LefterCol, IsRemoved), true, Temp2, NewBoard).

% get eat move for the computer player, or a player that is multi eating - and eat once.
get_eat_moves(checker(Player, IsKing, Row, Col, IsRemoved),IsMulti, Board, NewBoard) :-
    (
            Player =:= 2;
            IsKing == true;
            IsMulti == true
        ),
    get_adjacents(Row, Col, _, _, BottomRow, LeftCol),
    is_enemy_in_radius(Player, BottomRow, LeftCol, Board),
    get_adjacents(BottomRow, LeftCol, _, _, LowerRow, LefterCol),
    can_do_adjacent(Board, LowerRow, LefterCol), 
    move_piece(Board, checker(Player, IsKing, Row, Col, IsRemoved), checker(Player, IsKing, LowerRow, LefterCol, IsRemoved), Temp),
    eat_piece(Temp, BottomRow, LeftCol, NewBoard).

% eat a piece by removing it from the board
eat_piece(Board, Row, Col, NewBoard) :-
    member(checker(Player, IsKing, Row, Col, false), Board),
    select(checker(Player, IsKing, Row, Col, false), Board, NewBoard).

% check if an enemy is placed in a valid radius
is_enemy_in_radius(AttackingPlayer, AttackRow, AttackCol, Board) :-
    other_player(AttackingPlayer, OtherPlayer),
    member(checker(OtherPlayer, _, AttackRow, AttackCol, false), Board).

% check if the coordinates are valid, and the place is empty
can_do_adjacent(Board, Row, Col) :-
    Row < 8,
    Row >= 0,
    Col < 8,
    Col >= 0,
    not(member(checker(_, _, Row, Col, false), Board)).

% get pawn adjacents
get_adjacents(Row, Col, TopRow, RightCol, BottomRow, LeftCol) :-
    TopRow is Row - 1,
    RightCol is Col + 1,
    BottomRow is Row + 1,
    LeftCol is Col - 1.

% king piece
move_piece(Board, OldPiece, checker(1, false, 0, Col, false), NewBoard) :- 
    select(OldPiece, Board, checker(1, true, 0, Col, false), NewBoard), !.
    
% king piece
move_piece(Board, OldPiece, checker(2, false, 7, Col, false), NewBoard) :- 
    select(OldPiece, Board, checker(2, true, 7, Col, false), NewBoard), !.

% move general pawn
move_piece(Board, OldPiece, NewPiece, NewBoard) :-
    select(OldPiece, Board, NewPiece, NewBoard).

% count player active pawns
count_player_checkers(Board, Player, PlayerCount) :-
    findall(Board, member(checker(Player, false, _, _, false), Board), Checkers),
    length(Checkers, PlayerCount).

% count player king pawns
count_player_kings(Board, Player, KingsCount) :-
    findall(checker(Player, true, _, _, false), member(checker(Player, true, _, _, false), Board), Checkers),
    length(Checkers, KingsCount).

% count player pawns that are located in one of the corner row/cols
% such player is protected from being eaten
count_adjacents(Board, Player, AdjacentsCount) :-
    findall(checker(Player, _, _, 0, false), member(checker(Player, _, _, 0, false), Board), ColZAdj),
    findall(checker(Player, _, _, 7, false), member(checker(Player, _, _, 7, false), Board), ColSAdj),
    findall(checker(Player, _, 0, _, false), member(checker(Player, _, 0, _, false), Board), RowZAdj),
    findall(checker(Player, _, 7, _, false), member(checker(Player, _, 7, _, false), Board), RowSAdj),
    length(ColSAdj, L1), 
    length(ColZAdj, L2), 
    length(RowSAdj, L3), 
    length(RowZAdj, L4), 
    AdjacentsCount is L1 + L2 + L3 + L4.

% other player when computer = 2, human = 1.
other_player(1, 2).
other_player(2, 1).
