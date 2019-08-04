%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tellstick_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
%-compile(export_all).

-export([read_all/0,
	 read_devices/0,set_devices/2,
	 read_sensors/0,
	 call/2,cast/2
	]).


%% ====================================================================
%% External functions
%% ====================================================================
read_all()->
    lists:append(read_devices(),read_sensors()).

set_devices(WantedId,"ON")->
    set_devices(WantedId,"on");
set_devices(WantedId,"OFF")->
    set_devices(WantedId,"off");
set_devices(WantedId,NewDeviceStatus)->
    TdInfo=rpc:call('w50000@varmdo.asuscomm.com',os,cmd,["tdtool --list"]),
    [D,_S]=string:split(TdInfo,"SENSORS:"),
    [_|D1]=string:tokens(D,"\n"),
    D2=[string:tokens(Str,"\t")||Str<-D1],
    DevicesTupleList=[{Id,Name,State}||[Id,Name,State]<-D2],
    Reply=case lists:keyfind(WantedId,2,DevicesTupleList) of
	      false->
		  {error,[?MODULE,?LINE,'no_entry',WantedId]};
	      {Num,WantedId,_DeviceStatus} ->
		  Success="Turning "++NewDeviceStatus++" "++"device "++Num++", "++WantedId++" - "++"Success\n",
		  case rpc:call('w50000@varmdo.asuscomm.com',os,cmd,["tdtool --"++NewDeviceStatus++" "++Num]) of
		      Success->
			  ok;
		      _->
			  {error,[?MODULE,?LINE,'failed to set devices ',WantedId,NewDeviceStatus]}
		  end;		      
	      Err ->
		  {error,[?MODULE,?LINE,'no match',WantedId,Err]}
	  end,
    Reply.
	

read_devices()->
    TdInfo=rpc:call('w50000@varmdo.asuscomm.com',os,cmd,["tdtool --list"]),
    [D,_S]=string:split(TdInfo,"SENSORS:"),
    [_|D1]=string:tokens(D,"\n"),
    D2=[string:tokens(Str,"\t")||Str<-D1],
    DevicesTupleList=[{Id,Name,State}||[Id,Name,State]<-D2],
    DevicesTupleList.

read_sensors()->
    TdInfo=rpc:call('w50000@varmdo.asuscomm.com',os,cmd,["tdtool --list"]),
    [_D,S]=string:split(TdInfo,"SENSORS:"),
    [_|S1]=string:tokens(S,"\n"),
    S2=[string:tokens(Str,"\t")||Str<-S1],
    SensorTupleList=[{string:trim(Proto),string:trim(Model),string:trim(Id),string:trim(Temp),string:trim(Humidity),string:trim(Latest_update)}||[Proto,Model,Id,Temp,Humidity,Latest_update]<-S2],
    SensorTupleList.


call(App,{M,F,A})->
%    io:format("App,{M,F,A} = ~p~n",[{?MODULE,?LINE,App,{M,F,A}}]),
    case rpc:call(node(),app_discovery,query,[App]) of
	{badrpc,Err}->
	    Reply={error,[Err]};
	[AppNode|_] ->
	    Reply=rpc:call(AppNode,M,F,A);
	[] ->
	    Reply={error,[{?MODULE,?LINE,'no avaible application',App}]}
    end,
    Reply.

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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
% --------------------------------------------------------------------
%% Func: read_sensor/2
%% Purpose: Reads sensor value 
%% Returns: {ok,Temp}|{ok,no_value}
%% --------------------------------------------------------------------
