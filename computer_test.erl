-module(computer_test).
-author("Dmitry Zotov").

% user interface
-export([start/0, stop/0, show_kb/0, guess/0, add_problem/0, add_property/0]).

-define(ENGINE, computer).

stop() ->
  eresye:stop(?ENGINE).


start() ->
  code:ensure_loaded(eresye),
  eresye:start_link(?ENGINE),
  
  eresye:assert(?ENGINE,
    [{props, [
        'you see pictures on display',
        'BIOS is loading',
        'OS is loading',
        'BIOS doesn\'t display HD',
        'system block beeps',
        'can\'t access to system',
        'no sounds',
        'computer doesn\'t  accepts keyboard commands',
        'turn on system block into engine',
        'system block doesn\'t works',
        'you have wi-fi network',
        'you can\'t access to wi-fi network',
        'unusual OS working',
        'CPU overheating',
        'coolers problem sounds'
    ]}, 
     {?ENGINE, 'Computer is OK or unknown problem', ['you see pictures on display','BIOS is loading','OS is loading']},
     {?ENGINE, 'HD repair/replace',['BIOS is loading','BIOS doesn\'t display HD']},
     {?ENGINE, 'Check your BIOS signals table',['you see pictures on display','BIOS is loading','system block beeps']},
     {?ENGINE, 'Reinstall OS', ['you see pictures on display','BIOS is loading','can\'t access to system']},
     {?ENGINE, 'Check sound card drivers/repair sound card',['you see pictures on display','BIOS is loading','OS is loading', 'no sounds']},
     {?ENGINE, 'Change keyboard',['you see pictures on display','computer doesn\'t  accepts keyboard commands']},
     {?ENGINE, 'Reinstall wi-fi drivers/Change wi-fi card', ['you see pictures on display','BIOS is loading','OS is loading', 'you have wi-fi network', 'you can\'t access to wi-fi network']},
     {?ENGINE, 'Check computer for viruses',['you see pictures on display','BIOS is loading','OS is loading','unusual OS working']},
     {?ENGINE, 'Check computer coolers',['CPU overheating','coolers problem sounds']}
     ]).


show_kb() ->
  eresye:get_kb(?ENGINE).
    
ask_props() ->
  Filter = fun(Prop) -> 
		  case io:get_line("Do/Does " 
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
  TupleList = eresye:query_kb(?ENGINE, {?ENGINE, '_', Fun2}),

  % Explain result
  lists:foreach(fun({?ENGINE, Name, Props}) -> 
    io:format("I assume this might be ~w because it has the following properties:~n ~w~n",
	      [Name, Props]) end, TupleList).

get_name(Prompt) when is_list(Prompt) ->
  Line = io:get_line(Prompt),
  case re:run(Line, "^[a-z]*[a-z,0-9,_]*\n$") of
     {match, _} ->
  list_to_atom(hd(string:tokens(Line, "\n")));
     nomatch ->
  io:format("Wrong input, please use only lowercase Latin characters, digits and undersore.~n", []),
  get_name(Prompt)
  end.

add_problem() ->
  Transp = get_name("Please enter a computer problem: "),
  eresye:assert(?ENGINE, {?ENGINE, Transp, ask_props()}).

%% Adds a new property to the available properties list in the FB
add_property() ->
  NewProp = get_name("Please enter a property name: "),
  CurProps = get_props(),
  eresye:retract(?ENGINE, {props, CurProps}),
  eresye:assert(?ENGINE, {props, [NewProp|CurProps]}).



% 'Компьютер исправен либо проблема неизвестна', ['на экране видно изображение','загружается BIOS','загружается ОС']
% 'Замена жесткого диска',['загружается ли BIOS','не отображаются жесткие диски в BIOS']
% 'Проверка таблицы сигналов BIOS',['на экране видно изображение','загружается BIOS','Системный блок издает характерный сигнал/писк']
% 'Переустановка ОС', ['на экране видно изображение','загружается BIOS','не возможно войти в систему']
% 'Проверка драйверов звуковой карты/замена звуковой карты',['на экране видно изображение','загружается BIOS','загружается ОС', 'нет звуков']
% 'Замена клавиатуры',['на экране видно изображение','компьютер не реагирует на команды с клавиатуры']
% 'Замена блока питания',['полключе ли блок питания в сеть','не работает системный блок']
% 'Переустановка драйверов wi-fi карты/Замена wi-fi', ['на экране видно изображение','загружается BIOS','загружается ОС', 'есть wi-fi сеть', 'не возможно подключиться к wi-fi сети']
% 'Проверка компьютера на вредоносные программы',['на экране видно изображение','загружается BIOS','загружается ОС','ОС работает необычно']
% 'Проверка кулеров системного блока',['перегрев центрального процессора','характерный проблемый звук работы кулеров']

% 'Computer is OK or unknown problem', ['you see pictures on display','BIOS is loading','OS is loading']
% 'HD repair/replace',['BIOS is loading','BIOS doesn\'t display HD']
% 'Check your BIOS signals table',['you see pictures on display','BIOS is loading','system block beeps']
% 'Reinstall OS', ['you see pictures on display','BIOS is loading','can\'t access to system']
% 'Check sound card drivers/repair sound card',['you see pictures on display','BIOS is loading','OS is loading', 'no sounds']
% 'Change keyboard',['you see pictures on display','computer doesn\'t  accepts keyboard commands']
% 'Chenge energy block',['turn on system block into engine','system block doesn\'t works']
% 'Reinstall wi-fi drivers/Change wi-fi card', ['you see pictures on display','BIOS is loading','OS is loading', 'you have wi-fi network', 'you can\'t access to wi-fi network']
% 'Check computer for viruses',['you see pictures on display','BIOS is loading','OS is loading','unusual OS working']
% 'Check computer coolers',['CPU overheating','coolers problem sounds']


