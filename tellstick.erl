%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tellstick).

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
-record(state,{}).

%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================


-export([ read_devices/1,set_devices/2,
	  read_sensors/1,
	  read_all/0
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



%%-----------------------------------------------------------------------

read_all()->
    gen_server:call(?MODULE, {read_all},15000).

set_devices(Id,NewValue)->
    gen_server:call(?MODULE, {set_devices,Id,NewValue},15000).
read_devices(Id)->
    gen_server:call(?MODULE, {read_devices,Id},15000).

read_sensors(Id)->
    gen_server:call(?MODULE, {read_sensors,Id},15000).

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
      %  os:cmd("sudo /etc/init.d/telldusd restart"),
  %  StartEvent=[{node,node()},{event_level,info},{event_info,['Started server',?MODULE]}],
  %  rpc:cast(node(),tellsstick_lib,cast,[log,{log,add_event,[StartEvent]}]),
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
handle_call({restart}, _From, State) ->
    os:cmd("sudo /etc/init.d/telldusd restart"),
    timer:sleep(5000),    
    Reply={ok,telldus_restarted},
    {reply, Reply, State};

handle_call({read_all}, _From, State) ->
    Reply= rpc:call(node(),tellstick_lib,read_all,[]),
    {reply, Reply, State};


handle_call({read_devices,Id}, _From, State) ->
    Reply= case rpc:call(node(),tellstick_lib,read_devices,[]) of
	       {badrpc,Err}->
		   {error,[?MODULE,?LINE,badrpc,Id,Err]};
	       DevicesTupleList->
		   case  rpc:call(node(),lists,keyfind,[Id,2,DevicesTupleList]) of
		       {badrpc,Err}->
			   {badrpc,Err};
		       false->
			   {error,[?MODULE,?LINE,'no_entry',Id]};
		       {_Num,Id,DeviceStatus} ->
			   DeviceStatus;
		       Err ->
			   {error,[?MODULE,?LINE,'no match',Id,Err]}
		   end
	   end,
    {reply, Reply, State};

handle_call({set_devices,Id,NewDeviceStatus}, _From, State) ->
    Reply= case rpc:call(node(),tellstick_lib,set_devices,[Id,NewDeviceStatus]) of
	       {badrpc,Err}->
		   {error,[?MODULE,?LINE,badrpc,Id,Err]};
	       {error,Err}->
		   {error,[?MODULE,?LINE,error,Id,Err]};
	       Result->
		  Result
	   end,
    {reply, Reply, State};

handle_call({read_sensors,Id}, _From, State) ->
    Reply= case rpc:call(node(),tellstick_lib,read_sensors,[]) of
	       {badrpc,Err}->
		   {error,[?MODULE,?LINE,badrpc,Id,Err]};
	       SensorsTupleList->
		   case  rpc:call(node(),lists,keyfind,[Id,3,SensorsTupleList]) of
		       {badrpc,Err}->
			   {badrpc,Err};
		       false->
			   {error,[?MODULE,?LINE,'no_entry',Id]};
		       {_Proto,Model,Id,Temp,Humidity,_Latest_update} ->
			   {Model,Temp,Humidity};
		       Err ->
			   {error,[?MODULE,?LINE,'no match',Id,Err]}
		   end
	   end,
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

