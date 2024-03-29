%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(adder_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
%-compile(export_all).

-export([add/2,cast/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================
cast(App,{M,F,A})->
%    io:format("App,{M,F,A} = ~p~n",[{?MODULE,?LINE,App,{M,F,A}}]),
    case rpc:call(node(),app_discovery,query,[App]) of
	{badrpc,Err}->
	    Reply={error,[Err]};
	[AppNode|_] ->
	    Reply=rpc:cast(AppNode,M,F,A);
	[] ->
	    Reply={error,[{?MODULE,?LINE,'no avaible application',App}]}
    end,
    Reply.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add(A,B)->
    A+B.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
