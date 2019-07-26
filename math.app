%% This is the application resource file (.app file) for the 'base'
%% application.
{application, math,
[{description, "math" },
{vsn, "1.0.0" },
{modules, 
	  [math_app,math_sup,
	  adder,adder_lib,
	  server,server_lib]},
{registered,[adder,server,math]},
{applications, [kernel,stdlib]},
{mod, {math_app,[]}},
{start_phases, []}
]}.
