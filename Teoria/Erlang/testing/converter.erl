-module(converter).
-export([temperature/0]).

temperature() ->
  receive
    {toFahrenheit, T} -> io:format("~p°C -> ~p°F~n", [T, T * 9/5 + 32]), temperature();
    {toCelsius, T} -> io:format("~p°F -> ~p°C~n", [T, (T - 32) * 5/9]), temperature();
    stop -> io:format("Stop temperature service~n");
    Other -> io:format( "Unknown message received: < ~p >~n To convert temperature, send message as:~n    <Pid> ! {toFahrenheit|toCelsius, TemperatureValue}", [Other]),
             temperature()
  end.
