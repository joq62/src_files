%% This is the application resource file (.app file) for the 'base'
%% application.
{application, tick,
[{description, "tick" },
{vsn, "1.0.0" },
{modules, 
	  [tick_app,tick_sup,
	   server,server_lib]},
{registered,[server,tick]},
{applications, [kernel,stdlib]},
{mod, {tick_app,[]}},
{start_phases, []}
]}.
