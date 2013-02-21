-module(transport_expert).
-author("Yasir M. Arsanukaev, 2009 oct 3").

% user interface
-export([start/0, stop/0, show_kb/0, guess/0, add_transport/0, add_property/0]).

% rules
-export([aerial/2, military/2]).

% defines knowledge base instance name
-define(ENGINE, transport).

stop() ->
  eresye:stop(?ENGINE).

start() ->
  % load ERESYE binary module if it hasn't been yet
  code:ensure_loaded(eresye),

  % start ERESYE engine with specified name
  eresye:start_link(?ENGINE),

  % add rules
  lists:foreach(fun(X) -> eresye:add_rule(?ENGINE, {?MODULE, X}) end, [aerial, military]),

  % populate knowledge base with some facts
  eresye:assert(?ENGINE,
	  [{props, [airscrew, armour, wings, bucket, chain, conditioner, 
				  engine, gun_tube, spokes, track, wheels]}, 
	   {transport, moskvitch412, [engine, wheels, conditioner]},
	   {transport, excavator, [engine, track, bucket]},
	   {transport, bicycle, [wheels, spokes, chain]},
	   {transport, motorcycle, [engine, wheels, spokes, chain]},
	   {transport, tank_t34, [armour, engine, track, gun_tube]},
	   {transport, helicopter_mi8, [airscrew, engine, wheels]},
	   {transport, helicopter_ka50, [airscrew, armour, engine, wheels]},
   	   {transport, plane_su27, [wings, armour, engine, wheels]}
	   ]).


%% ERESYE rule. Any transport than has airscrew or wings is interpreted as aerial
%% and the fact {kindof, aerial, TransporName} is asserted.
%%
%% if (Properties contain airscrew) or (Properties contain wings)
%%     then (Transport is aerial)
%%
aerial(Engine, {transport, Transport, Properties}) when is_list(Properties) ->
    case lists:any(fun(X) -> lists:member(X, [airscrew,wings]) end, Properties) of
	true ->
		eresye:assert(Engine, {kindof, aerial, Transport});
	_->
		void
    end.

%% if (Properties contain armour) or (Properties contain gun_tube)
%%     then (Transport is military)
military(Engine, {transport, Transport, Properties}) when is_list(Properties) ->
    case lists:any(fun(X) -> lists:member(X, [armour,gun_tube]) end, Properties) of
	true ->
		eresye:assert(Engine, {kindof, military, Transport});
	_->
		void
    end.

% Show knowledge base contents
show_kb() ->
  eresye:get_kb(?ENGINE).
    
% Gets input from user until an entered line meets these requirements: 
% starts with lowercase letter followed by lowercase letters, digits
% or underscore ('_'), e. g. example_1
get_name(Prompt) when is_list(Prompt) ->
  Line = io:get_line(Prompt),
  case re:run(Line, "^[a-z]*[a-z,0-9,_]*\n$") of
     {match, _} ->
	list_to_atom(hd(string:tokens(Line, "\n")));
     nomatch ->
	io:format("Wrong input, please use only lowercase Latin characters, digits and undersore.~n", []),
	get_name(Prompt)
  end.

%% Adds a new transport to the available properties list in the FB
add_transport() ->
  Transp = get_name("Please enter a transport name: "),
  eresye:assert(?ENGINE, {transport, Transp, ask_props()}).

%% Adds a new property to the available properties list in the FB
add_property() ->
  NewProp = get_name("Please enter a property name: "),
  CurProps = get_props(),
  eresye:retract(?ENGINE, {props, CurProps}),
  eresye:assert(?ENGINE, {props, [NewProp|CurProps]}).

%% @spec ask_props() -> list(atom())
%% Returns the list of properties selected by the user from available properties
ask_props() ->
  Filter = fun(Prop) -> 
		  case io:get_line("Does this kind of transport has " 
		  	++ atom_to_list(Prop) ++ "? ") of
		    "y"++_ ->
			true;
		    _ ->
			false
		  end
	   end,
  lists:filter(Filter, get_props()).

%% @spec get_props() -> list(atom())
%% Returns the list of properties that a kind of transport can possess
get_props() ->
  {props, Props} = hd(eresye:query_kb(?ENGINE, {props, '_'})),
  Props.

%% Asks the user for properties which transport possesses
%% and triggers guessing process
guess() ->
  guess(ask_props()).
  
%% Tries to find kinds of transport which match best to the given properties
guess(Properties) when is_list(Properties) ->
  % Filter out objects which have appropriate Properties
  % Any item which makes fun(X) return true is included in result
  %Fun1 = fun(X) -> lists:all(fun(Y) -> lists:member(Y, Properties) end, X) end,
  Fun2 = fun(X) -> if
		   	is_list(X) ->
			  lists:all(fun(Y)-> lists:member(Y, Properties) end, X);
		    	true ->
			  false
		   end
 	 end,
  TupleList = eresye:query_kb(?ENGINE, {transport, '_', Fun2}),

  % Explain result
  lists:foreach(fun({transport, Name, Props}) -> 
    io:format("I assume this might be ~w because it has the following properties: ~w~n",
	      [Name, Props]) end, TupleList).
