-module(transport_expert).
-export([start/0, stop/0, show_kb/0, guess/0, add_transport/0, add_property/0]).
-export([aerial/2, military/2]).
-define(ENGINE, transport).

stop() ->
  eresye:stop(?ENGINE).

start() ->
  code:ensure_loaded(eresye),
  eresye:start_link(?ENGINE),

  lists:foreach(fun(X) -> eresye:add_rule(?ENGINE, {?MODULE, X}) end, [aerial, military]),

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

aerial(Engine, {transport, Transport, Properties}) when is_list(Properties) ->
    case lists:any(fun(X) -> lists:member(X, [airscrew,wings]) end, Properties) of
	true ->
		eresye:assert(Engine, {kindof, aerial, Transport});
	_->
		void
    end.

military(Engine, {transport, Transport, Properties}) when is_list(Properties) ->
    case lists:any(fun(X) -> lists:member(X, [armour,gun_tube]) end, Properties) of
	true ->
		eresye:assert(Engine, {kindof, military, Transport});
	_->
		void
    end.

show_kb() ->
  eresye:get_kb(?ENGINE).
    
get_name(Prompt) when is_list(Prompt) ->
  Line = io:get_line(Prompt),
  case re:run(Line, "^[a-z]*[a-z,0-9,_]*\n$") of
     {match, _} ->
	list_to_atom(hd(string:tokens(Line, "\n")));
     nomatch ->
	io:format("Wrong input, please use only lowercase Latin characters, digits and undersore.~n", []),
	get_name(Prompt)
  end.

add_transport() ->
  Transp = get_name("Please enter a transport name: "),
  eresye:assert(?ENGINE, {transport, Transp, ask_props()}).

add_property() ->
  NewProp = get_name("Please enter a property name: "),
  CurProps = get_props(),
  eresye:retract(?ENGINE, {props, CurProps}),
  eresye:assert(?ENGINE, {props, [NewProp|CurProps]}).

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

get_props() ->
  {props, Props} = hd(eresye:query_kb(?ENGINE, {props, '_'})),
  Props.

guess() ->
  guess(ask_props()).
  
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
