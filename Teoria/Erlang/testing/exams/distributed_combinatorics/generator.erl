-module(generator).
-export([init/3]).
% P is the processor position in the sequence,
% N is the number of processors in the system and % M the number of possible values

init(P, N, M) ->
  counter(1, trunc(math:pow(M, (N-P))),
          trunc(math:pow(M, N)), 1, P, N, M).


% The counter advances after M^(N-P) ticks
% Seq is the iteration number
% Delay downcounts from the number of ticks needed before increasing the counter % Value is the current value of the counter, if greater than M the counter stops % T is the total number of iterations
counter(Seq, _, T, _, _, _, _) when Seq > T -> stop;

counter(Seq, _, T, Value, P, N, M) when (Value > M) ->
  counter(Seq, trunc(math:pow(M, (N-P))), T, 1, P, N, M);

counter(Seq, 1, T, Value, P, N, M) ->
  entrypoint ! {seq, Seq, val, Value, pos, P},
  counter(Seq+1, trunc(math:pow(M, (N-P))), T, Value+1, P, N, M);

counter(Seq, Delay, T, Value, P, N, M) ->
  entrypoint ! {seq, Seq, val, Value, pos, P}, counter(Seq+1, Delay-1, T, Value, P, N, M).
