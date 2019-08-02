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
