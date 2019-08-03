%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(log_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
%-compile(export_all).

-export([read_events/2,format_event/1,
	 add_event/6
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
read_events(Events,Num)->
    read_events(Events,Num,[]).

read_events([],_Num,EventList)->
    EventList;
read_events(_,0,EventList)->
    EventList;
read_events([Event|T],N,Acc)->
    NewAcc=[Event|Acc],
    NewN=N-1,
    read_events(T,NewN,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
format_event(Event)->
    {date,D}= lists:keyfind(date,1,Event),
    {time,T}= lists:keyfind(time,1,Event), 
    {node,Node}= lists:keyfind(node,1,Event),
    {event_level,Level}= lists:keyfind(event_level,1,Event),
    {event_info,Info}= lists:keyfind(event_info,1,Event),
    {D,T,Node,Level,Info}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_event(Date,Time,Event,Events,EventNr,MaxEvents)->
    LogElem=lists:append([{date,Date},{time,Time}],Event),
    NewEvents=[LogElem|lists:sublist(Events,MaxEvents-1)],
    case EventNr of
	MaxEvents->
	    NewEventNr=MaxEvents;
	_->
	    NewEventNr=lists:flatlength(NewEvents)
    end,
    {NewEvents,NewEventNr}.
	

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
