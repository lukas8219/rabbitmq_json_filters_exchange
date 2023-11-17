-module(json_filter).

-export([do_filter/2, parse/1]).

parse(Payload) ->
    jsone:decode(Payload, [{object_format, proplist}]).

do_filter(Payload, InputFilter) when is_list(Payload) and is_binary(InputFilter) ->
    DecodedFilter = parse(InputFilter),
    filter_matches(Payload, DecodedFilter);

do_filter(Payload, InputFilter) when is_binary(Payload) and is_list(InputFilter) ->
    DecodedPayload = parse(Payload),
    filter_matches(DecodedPayload, InputFilter);

do_filter(Payload, InputFilters) when is_binary(Payload) and is_binary(InputFilters) ->
    DecodedFilters = parse(InputFilters),
    PayloadAsMap = parse(Payload),
    filter_matches(PayloadAsMap, DecodedFilters);

do_filter(Payload, InputFilter) when is_list(Payload) and is_list(InputFilter) ->
        filter_matches(Payload, InputFilter).

filter_matches(_, []) -> true;
filter_matches(Payload, Filters) ->
    [Filter | Rest] = Filters,
    case match_filter_all(Payload, Filter) of
        true -> filter_matches(Payload, Rest);
        false -> false
    end.

match_filter_all(Payload, [Filter | Rest] = Filters) when is_list(Filters) -> 
    case Filter of
        {Key,Value} -> match_filter(Payload, {Key,Value}) and match_filter_all(Payload, Rest);
        _ -> case match_filter(Payload, Filter) of
                true -> match_filter(Payload, Rest);
                false -> false
            end
        end;

match_filter_all(_, []) -> true.

match_filter_any(Payload, [Filter | Rest] = Filters) when is_list(Filters) ->
    case Filter of
        {Key, Value} -> match_filter(Payload, {Key, Value}) or match_filter_any(Payload, Rest);
        [{Key,Value}] -> match_filter(Payload, {Key,Value}) or match_filter_any(Payload, Rest);
        _ -> case match_filter(Payload, Filter) of
            true -> true;
            false -> match_filter(Payload, Rest)
        end
    end;

match_filter_any(_, []) -> false.

match_filter_none(Payload, [Filter | Rest] = Filters) when is_list(Filters) ->
  case Filter of
    {Key,Value} -> not(match_filter(Payload, {Key, Value})) and match_filter_none(Payload, Rest);
    [{Key,Value}] -> not(match_filter(Payload, {Key, Value})) and match_filter_none(Payload, Rest);
    _ -> case match_filter(Payload, Filter) of
           true -> false;
           false -> match_filter(Payload, Rest)
         end
  end;

match_filter_none(_, []) -> true.

match_filter(Payload, {<<"$or">>, Value} = Map) when is_tuple(Map) ->
    match_filter_any(Payload, Value);

match_filter(Payload, {<<"$ne">>, Value} = Map) when is_tuple(Map) ->
    match_filter_none(Payload, Value);

match_filter(Payload, {KeyToMatch,ValueToMatch} = Map) when is_tuple(Map) ->
    MatchedTuples = [{Key, Value} || {Key, Value} <- Payload, Key =:= KeyToMatch],
    [{_, ValueMatched} | _] = MatchedTuples,
    ValueMatched == ValueToMatch.
