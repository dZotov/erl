-module(expert).
-author("Dmitry Zotov").

-define(ENGINE, computer).

-export([start/0, stop/0, show_kb/0]).

stop() ->
  eresye:stop(?ENGINE).

show_kb() ->
  eresye:get_kb(?ENGINE).  

start() ->
  code:ensure_loaded(eresye),
  eresye:start(computer),

  eresye:assert(?ENGINE,
	  [
	  	{props,[
	  		"Есть ли изображение на экране", 
			"Загружается ли операционная система", 
			"Работают ли устройства ввода-вывода", 
			"Работают ли периферийные устройства",
			"Загружается ли BIOS",
			"Отображаются ли жесткие диски в BIOS",
			"Издает ли писк системный блок"
	  		]
	  	}, 
	   {computer, "Компьютер исправен либо проблема неизвестна", ["Есть ли изображение на экране", "Загружается ли операционная система", 
	   															 	"Работают ли устройства ввода-вывода", "Работают ли периферийные устройства"]},
	   {computer, "Переустановить ОС", ["Есть ли изображение на экране","Загружается ли BIOS","Отображаются ли жесткие диски в BIOS"]},
	   {computer, "Проверка кодов BIOS",["Есть ли изображение на экране", "Загружается ли BIOS", "Издает ли писк системный блок"]}
	   ]).

  
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
