-module(expert).
-author("Dmitry Zotov").

-define(ENGINE, computer).

-export([start/0, stop/0, show_kb/0]).

stop() ->
  eresye:stop(?ENGINE).

show_kb() ->
  eresye:get_kb(?ENGINE).  

% ask_props() ->
%   Filter = fun(Prop) -> 
% 		  case io:get_line("Does this kind of transport have " 
% 		  	++ atom_to_list(Prop) ++ "? ") of
% 		    "y"++_ ->
% 			true;
% 		    _ ->
% 			false
% 		  end
% 	   end,
%   lists:filter(Filter, get_props()).

% guess() ->
%   guess(ask_props()).

% guess(Properties) when is_list(Properties) ->
%   Fun2 = fun(X) -> if
% 		    	is_list(X) ->
% 			  lists:all(fun(Y) -> lists:member(Y, Properties) end, X);
% 		    	true ->
% 			  false
% 		    end
%  	  end,
%   TupleList = eresye:query_kb(?ENGINE, {transport, '_', Fun2}),

%   % Explain result
%   lists:foreach(fun({transport, Name, Props}) -> 
%     io:format("I assume this might be ~w because it has the following properties: ~w~n", [Name, Props]) end, TupleList).    


start() ->
  code:ensure_loaded(eresye),
  eresye:start(computer),

  eresye:assert(?ENGINE,
	  [
	  	{props,[
	  		"Есть ли изображение на экране",
			"Загружается ли операционная система",
			"Загружается ли настройка BIOS",
			"Отображается ли жесткий диск в BIOS",
			"Подключен ли жесткий диск к материнской плате",
			"Работают ли устройства ввода-вывода",
			"Работают ли периферийные устройства",
			"Включены ли системный блок и монитор в электросеть",
			"Работает ли системный блок",
			"Подключен ли монитор к системному блоку",
			"Исправен ли монитор",
			"Исправна ли видеокарта",
			"Исправен ли блок питания",
			"Подключен ли блок питания к материнской плате"
	  		]
	  	}, 
	   {computer, "Компьютер исправен либо проблема неизвестна", ["Есть ли изображение на экране", "Загружается ли операционная система"]}
	   ]).


% Rules = [
% 	{"Компьютер исправен либо проблема неизвестна", [1,2,6,7]},
% 	{"Требуется замена неисправного периферийного устройства", [1,2,7]},
% 	{"Необходимо заменить неисправное устройство ввода-вывода", [1,2]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
% 	{"Hеобходимо заменить жесткий диск", [1,2,3,4]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
% 	{"Tребуется переустановка операционной системы", [1,2,3,4]},
	
% ].
  