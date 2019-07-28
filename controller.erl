%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("interfacesrc/brd_local.hrl").

%% --------------------------------------------------------------------
-define(NUM_TRIES,5).
-define(DEPLOY_INTERVAL,5000).
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{
	      }).

%% --------------------------------------------------------------------
-define(SYNC_INTERVAL,10*1000).
%% ====================================================================
%% External functions
%% ====================================================================


-export([update_config/2,
	 sync/1
	]).

-export([start/0,
	 stop/0
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals

%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------Call ------------------------------------------------------------


%%----------Cast-------------------------------------------------------------
sync(Interval)->
    gen_server:cast(?MODULE, {sync,Interval}).    

update_config(Filename,Binary)->
    gen_server:cast(?MODULE, {update_config,Filename,Binary}).
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
    application:start(app_discovery),    
    application:start(app_deploy),
    R=rpc:call(node(),controller_lib,deploy_app_discovery,[?NUM_TRIES,?DEPLOY_INTERVAL,false]),
    io:format("R= ~p~n",[{date(),time(),?MODULE,?LINE,R}]),
    spawn(controller,sync,[?SYNC_INTERVAL]),
    io:format("Started server ~p~n",[{date(),time(),?MODULE}]),
    {ok, #state{}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({stop}, _From, State) ->
  %  io:format("stop ~p~n",[{?MODULE,?LINE}]),
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
handle_cast({sync,Interval},State) ->
    io:format("*************** Start Campaign ***********' ~p~n",[{date(),time()}]), 
    case rpc:call(node(),controller_lib,campaign,[],30000) of
	{badrpc,Err}->
	    io:format("StartResult = ~p~n",[{?MODULE,?LINE,error,Err}]);
	{StartResult,AvailableNodes,NodeAppsList}->
	    io:format("StartResult = ~p~n",[StartResult]),
	    io:format("AvailableNodes = ~p~n",[AvailableNodes]),
	    io:format("NodeAppsList = ~p~n",[NodeAppsList])
    end,

    io:format("------------- End Campaign ----------' ~p~n",[{date(),time()}]),
    spawn(controller_lib,tick,[Interval]),
    {noreply, State};

handle_cast({update_config,Filename,Binary},State) ->
    ok=file:write_file(Filename,Binary),
    rpc:call(node(),controller_lib,campaign,[],5000),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
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
%%% Exyernal functions
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

