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
-define(MANY_EVENTS,50).
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
print_events(many)->
    Reply= case mon_lib:call(controller,{app_discovery,query,[log]}) of
	       {error,_Err}->
		   no_print;
	       [AppNode|_] ->
		   case  mon_lib:call(AppNode,{log,read_events,[?MANY_EVENTS]}) of
		       {error,_Err}->
			   no_print;
		       Events ->
			   [format_event(Event)||Event<-Events]
		   end
	   end,
    Reply;
print_events(Num) ->
    Reply=case mon_lib:call(controller,{app_discovery,query,[log]}) of
	      {error,_Err}->
		  no_print;
	      [AppNode|_] ->
		  case  mon_lib:call(AppNode,{log,read_events,[Num]}) of
		      {error,_Err}->
			  no_print;
		      []->
			  no_print; 
		      Events ->
			  [format_event(Event)||Event<-Events]
		  end
	  end,
    Reply.

format_event(Event)->
    {date,D}= lists:keyfind(date,1,Event),
    {time,T}= lists:keyfind(time,1,Event), 
    {node,Node}= lists:keyfind(node,1,Event),
    {event_level,Level}= lists:keyfind(event_level,1,Event),
    {event_info,Info}= lists:keyfind(event_info,1,Event),
    {D,T,Node,Level,Info}.

print_format_event(Event)->
    {date,D}= lists:keyfind(date,1,Event),
    {time,T}= lists:keyfind(time,1,Event), 
    {node,Node}= lists:keyfind(node,1,Event),
    {event_level,Level}= lists:keyfind(event_level,1,Event),
    {event_info,Info}= lists:keyfind(event_info,1,Event),
    io:format(" ~w ~n",[{D,T,Node,Level,Info}]).
    
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
cast(App,{M,F,A})->
 %   io:format("App,{M,F,A} = ~p~n",[{?MODULE,?LINE,App,{M,F,A}}]),
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
call(App,{M,F,A})->
 %   io:format("App,{M,F,A} = ~p~n",[{?MODULE,?LINE,App,{M,F,A}}]),
    case rpc:call(node(),app_discovery,query,[App]) of
	{badrpc,Err}->
	    Reply={error,[Err]};
	[AppNode|_] ->
	    Reply=rpc:call(AppNode,M,F,A);
	[] ->
	    Reply={error,[{?MODULE,?LINE,'no avaible application',App}]}
    end,
    Reply.