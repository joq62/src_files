%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% {event,[?MODULE,?LINE,info]}        % Normal event information
%%% {notification,[?MODULE,?LINE,info]} % Strange behaviour ex unmatched signal
%%% {error,[?MODULE,?LINE,info]}        % Execution error
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(log).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(MAX_EVENTS,10).
-define(CALL_TIMEOUT,120*1000).

%% --------------------------------------------------------------------
%% Key Data structures
%% 
% Value   Severity      Description                                Condition
%    0 	Emergency    	System is unusable 	                  A panic condition.[8]
%    1 	Alert 		Action must be taken immediately 	  A condition that should be corrected immediately, 
%                                                                 such as a corrupted system database.[8]
%    2 	Critical 	Critical conditions 	                  Hard device errors.[8]
%    3 	Error   	error                                     Error conditions 	
%    4 	Warning 	warning                          	  Warning conditions 	
%    5 	Notice 		Normal but significant conditions 	  Conditions that are not error conditions, 
%                                                                 but that may require special handling.[8]
%    6 	Informational 	info 		                          Informational messages 	
%    7 	Debug 		Debug-level messages 	                  Messages that contain information 
%                                                                 normally of use only when debugging a program.[8]
%% --------------------------------------------------------------------
% -record ??



-export([add_event/1,print_event/0,
	 read_events/1,new_event/0
	]).

-export([start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {max_events,events,event_num,latest_event}).
%% ====================================================================
%% External functions
%% ====================================================================



%% Gen server functions
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
new_event()->
    gen_server:call(?MODULE, {new_event},infinity).

read_events(NumEvents)->
    gen_server:call(?MODULE, {read_events,NumEvents},infinity).

heart_beat()->
    gen_server:call(?MODULE, {heart_beat},infinity).

%%-----------------------------------------------------------------------

add_event(Event)->
    gen_server:cast(?MODULE, {add_event,Event}).  
print_event()->
    gen_server:cast(?MODULE, {print_event}).  
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    Event=[{node,node()},{event_level,info},{event_info,[?MODULE,?LINE,'service started',?MODULE]}],
    {NewEvents,NewEventNum}=rpc:call(node(),log_lib,add_event,[date(),time(),Event,[],0,?MAX_EVENTS]),
    [Latest_event|_]=NewEvents,
       
    {ok, #state{max_events=?MAX_EVENTS,events=NewEvents,event_num=NewEventNum,latest_event=[]}}.    
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({new_event}, _From, State) ->
    
    [Newest|_]=State#state.events,
    if 
	State#state.latest_event==Newest ->
	    Reply=false,
	    NewLatest=State#state.latest_event;
	true ->
	    Reply=Newest,
	    NewLatest=Newest
    end,
    NewState=State#state{latest_event=NewLatest},
    {reply, Reply, NewState};

handle_call({read_events,NumEvents}, _From, State) ->
    Reply=lists:sublist(State#state.events,NumEvents),
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%(dbase,IpAddrDbase,PortDbase)

handle_cast({print_event},State) ->
    
    {noreply, State};

handle_cast({add_event,Event},State) ->
    {NewEvents,NewEventNum}=rpc:call(node(),log_lib,add_event,[date(),time(),Event,State#state.events,State#state.event_num,State#state.max_events]),
    
       NewState=State#state{events=NewEvents, event_num=NewEventNum},
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info(Info, State) ->
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
    

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
