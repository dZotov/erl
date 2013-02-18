-module(test).
-export([fac/1]).
-export([fib/1]).
 
fac(N) when N == 0 -> 1;
fac(N) when N > 0  -> N*fac(N-1).


fib(List) when len(List) == 0 -> fib([1]);
fib(List) when len(List) < 10 -> hd(List)+hd(tl(List)). %fib([hd(List)+hd(tl(List))|List]). 
