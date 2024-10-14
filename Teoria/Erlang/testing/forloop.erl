-module(forloop).
-export([for/2, for/3]).

for(F, Max) when Max < 0 -> for(F, 0, Max, -1);
for(F, Max)              -> for(F, 0, Max, +1).

for(F, Min, Max) when Min > Max  -> for(F, Min, Max, -1);
for(F, Min, Max)                 -> for(F, Min, Max, +1).

for(F, Max, Max, _) -> [F(Max)];
for(F, I, Max, Inc) -> [F(I) | for(F, I+Inc, Max, Inc)].
