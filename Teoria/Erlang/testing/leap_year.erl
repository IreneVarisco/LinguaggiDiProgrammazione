-module(leap).

-export([leap_year/1]).

leap_year(_Year) -> (_Year mod 4) and (_Year mod 100) and (_Year mod 400).
