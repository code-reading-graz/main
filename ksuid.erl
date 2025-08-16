-module(ksuid).

%% Based on https://github.com/segmentio/ksuid

-export([new/0]).
-export([new_with_timestamp/1]).
-export([from_parts/2]).
-export([timestamp/1]).
-export([unix_timestamp/1]).
-export([to_base62/1]).
-export([from_base62/1]).

-define(PAYLOAD_LENGTH, 16).
-define(EPOCH, 1400000000).
-define(BASE64_CHARS,
    <<"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz">>).

new() ->
    new_with_timestamp(os:timestamp()).

new_with_timestamp(Timestamp) ->
    Payload = crypto:strong_rand_bytes(?PAYLOAD_LENGTH),
    from_parts(Timestamp, Payload).

from_parts({MegaSecs, Secs, _}, Payload) when is_binary(Payload), byte_size(Payload) =:= 16 ->
    Ts = MegaSecs * 1000000 + Secs - ?EPOCH,
    << Ts: 32, Payload/binary >>.

timestamp(Ksuid) ->
    Ts = unix_timestamp(Ksuid),
    MegaSecs = Ts div 1000000,
    Secs = Ts rem 1000000,
    {MegaSecs, Secs, 0}.

unix_timestamp(<< Ts:32, _:?PAYLOAD_LENGTH/binary >>) ->
    Ts + ?EPOCH.

to_base62(<< Id:160 >>) ->
    to_base62(Id, <<>>).

to_base62(0, Acc) ->
    padzero(Acc, 27 - byte_size(Acc));
to_base62(Id, Acc) ->
    R = Id rem 62,
    << _:R/binary, C:8, _/binary >> = ?BASE64_CHARS,
    to_base62(Id div 62, << C:8, Acc/binary >>).


from_base62(<< Id:27/binary >>) ->
    from_base62(Id, 0).

from_base62(<<>>, Acc) ->
    << Acc:160 >>;
from_base62(<< C:8, Rest/binary >>, Acc) ->
    from_base62(Rest, (Acc*62) + b62(C)).


padzero(Id, 0) ->
    Id;
padzero(Id, N) when N > 0 ->
    padzero(<< $0, Id/binary >>, N - 1).


b62($0) ->  0;
b62($1) ->  1;
b62($2) ->  2;
b62($3) ->  3;
b62($4) ->  4;
b62($5) ->  5;
b62($6) ->  6;
b62($7) ->  7;
b62($8) ->  8;
b62($9) ->  9;
b62($A) -> 10;
b62($B) -> 11;
b62($C) -> 12;
b62($D) -> 13;
b62($E) -> 14;
b62($F) -> 15;
b62($G) -> 16;
b62($H) -> 17;
b62($I) -> 18;
b62($J) -> 19;
b62($K) -> 20;
b62($L) -> 21;
b62($M) -> 22;
b62($N) -> 23;
b62($O) -> 24;
b62($P) -> 25;
b62($Q) -> 26;
b62($R) -> 27;
b62($S) -> 28;
b62($T) -> 29;
b62($U) -> 30;
b62($V) -> 31;
b62($W) -> 32;
b62($X) -> 33;
b62($Y) -> 34;
b62($Z) -> 35;
b62($a) -> 36;
b62($b) -> 37;
b62($c) -> 38;
b62($d) -> 39;
b62($e) -> 40;
b62($f) -> 41;
b62($g) -> 42;
b62($h) -> 43;
b62($i) -> 44;
b62($j) -> 45;
b62($k) -> 46;
b62($l) -> 47;
b62($m) -> 48;
b62($n) -> 49;
b62($o) -> 50;
b62($p) -> 51;
b62($q) -> 52;
b62($r) -> 53;
b62($s) -> 54;
b62($t) -> 55;
b62($u) -> 56;
b62($v) -> 57;
b62($w) -> 58;
b62($x) -> 59;
b62($y) -> 60;
b62($z) -> 61.
