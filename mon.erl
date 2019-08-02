%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% {event,[?MODULE,?LINE,info]}        % Normal event information
%%% {notification,[?MODULE,?LINE,info]} % Strange behaviour ex unmatched signal
%%% {error,[?MODULE,?LINE,info]}        % Execution error
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mon).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(INTERVAL,3000).

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



-export([heart_beat/1
	]).

-export([start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {prev}).
%% ====================================================================
%% External functions
%% ====================================================================



%% Gen server functions
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).

%%-----------------------------------------------------------------------
 
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
    Prev=mon_lib:print_event([]),
    spawn(mon,heart_beat,[?INTERVAL]),
    
    {ok, #state{prev=Prev}}.    
    
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
handle_cast({heart_beat,Interval}, State) ->
    timer:sleep(Interval),
    NewPrev=mon_lib:print_event([State#state.prev]),
    NewState=State#state{prev=NewPrev},
    spawn(mon,heart_beat,[?INTERVAL]),   
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
