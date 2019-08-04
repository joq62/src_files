%% This is the application resource file (.app file) for the 'base'
%% application.
{application, tellstick,
[{description, "tellstick" },
{vsn, "1.0.0" },
{modules, 
	  [tellstick_app,tellstick_sup,
	   tellstick,tellstick_lib]},
{registered,[tellstick]},
{applications, [kernel,stdlib]},
{mod, {tellstick_app,[]}},
{start_phases, []}
]}.
