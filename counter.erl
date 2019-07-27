%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(counter).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("interfacesrc/brd_local.hrl").

%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{tick}).

%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


-export([tick/0,num/0
	]).

-export([start/0,
	 stop/0
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================


%% Gen server function

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------

num()->
    gen_server:call(?MODULE, {num},5000).


%%-----------------------------------------------------------------------
tick()->
    gen_server:cast(?MODULE, {tick}).

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
    spawn(counter,tick,[]),
    io:format("Started server ~p~n",[{date(),time(),?MODULE}]),
    {ok, #state{tick=0}}.   
    
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

handle_call({num}, _From, State) ->
    Nodes=rpc:call(node(),app_discovery,query,[tick],5000),
    case Nodes of
	[]->
	    Reply={error,[?MODULE,?LINE,'eexists',tick]};
	{badrpc,Err}->
	    Reply={error,[?MODULE,?LINE,badrpc,Err,tick]};
	[Node|_]->
	    Reply={Node,rpc:call(Node,server,num,[],5000)}
    end,
    io:format("Num =  ~p~n",[Reply]),    
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

handle_cast({tick}, State) ->
    Nodes=rpc:call(node(),app_discovery,query,[tick],5000),
    case Nodes of
	[]->
	    Reply={error,[?MODULE,?LINE,'eexists',tick]};
	{badrpc,Err}->
	    Reply={error,[?MODULE,?LINE,badrpc,Err,tick]};
	[Node|_]->
	    rpc:call(Node,server,tick,[],5000),
	    Reply=rpc:call(Node,server,num,[],5000)
    end,
    io:format("Num =  ~p~n",[{time(),Reply}]), 
    timer:sleep(1000),
    spawn(counter,tick,[]),
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

