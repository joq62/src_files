%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mon_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-compile(export_all).

%-export([load_start_node/3,stop_unload_node/3
%	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
print_event(Prev)->
    io:format("~p~n",[Prev]),
    case rpc:call('controller@joqhome.dynamic-dns.net',
		     log,read_events,[1]) of
	[LogInfo]->
		io:format("~p~n",[{?MODULE,?LINE,LogInfo}]),
	    if 
		Prev==LogInfo->
		    io:format("~p~n",[{?MODULE,?LINE,Prev}]),
		    NewPrev=Prev;
		true ->
		    io:format("~p~n",[LogInfo]),
		    NewPrev=LogInfo
	    end;
	{badrpc,Err}->
	    io:format("~p~n",[{badrpc,Err}]),
	    NewPrev=[]
    end,
    NewPrev.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
