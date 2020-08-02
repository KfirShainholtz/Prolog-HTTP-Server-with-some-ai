:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json_convert)).
:- use_module(library(settings)).
:- use_module(game).
:- use_module(alphabeta).
:- use_module(library(http/http_session)).
:- set_setting(http:cors, ['http://localhost:3000']).

% declare json object structure
:- json_object
checker(player:integer, isKing:boolean, row:integer, col:integer, removed:boolean).

% initialize server with port
server(Port) :-
   http_handler(root(.),handle,[]),
   http_server(http_dispatch,[port(Port)]).

% handle general request with CORS: access control allow origin *
handle(Req) :-
    option(method(options), Req), !,
    cors_enable(Req, [ methods([post]) ]),
    format('~n').

% route post requests to get_best_move
% enable cors (once again)
% get best move
% reply json with the best move
handle(PostRequest) :-
    member(method(post), PostRequest), !,
    http_read_json(PostRequest, DictIn,[json_object(term)]),
    get_best_move(DictIn, BestMove),
    prolog_to_json(BestMove, JSONObject),
    cors_enable,
    reply_json(JSONObject).

% convert json to prolog json objects
get_best_move(JsonDict, BestMove) :-
    json_to_prolog(JsonDict, Checkers), 
    % retract staticvals (seems like the performance go up when single instance server)
    format(user_output, "Board is ~p~n", [Checkers]),
    % get alphabeta score for the computer player, with alphabeta depth of 4
    alphabeta(Checkers, 2, -1000, 1000, BestMove, Val, 6),
    format(user_output, "val is ~p~n", [Val]).
    
    