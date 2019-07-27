%% This is the application resource file (.app file) for the 'base'
%% application.
{application, counter,
[{description, "counter" },
{vsn, "1.0.0" },
{modules, 
	  [counter_app,counter_sup,
	   counter]},
{registered,[counter]},
{applications, [kernel,stdlib]},
{mod, {counter_app,[]}},
{start_phases, []}
]}.
