
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2023 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_exchange_type_json).
-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(rabbit_exchange_type).

-export([description/0, serialise_events/0, route/2, route/3]).
-export([validate/1, validate_binding/2,
         create/2, delete/2, policy_changed/2, add_binding/3,
         remove_bindings/3, assert_args_equivalence/2]).
-export([info/1, info/2]).

-rabbit_boot_step({?MODULE,
                   [{description, "exchange type json"},
                    {mfa,         {rabbit_registry, register,
                                   [exchange, <<"json">>, ?MODULE]}},
                    {requires,    rabbit_registry},
                    {enables,     kernel_ready}]}).

-define(JSON_FILTER_HEADER, "x-json-filters").

-import(json_filter, [parse/1]).

info(_X) -> [].
info(_X, _) -> [].

description() ->
    [{description, <<"JSON exchange">>}].

serialise_events() -> false.

route(#exchange{name = Name}, Msg) ->
    route(#exchange{name = Name}, Msg, #{}).

route(#exchange{name = Name}, _Msg, _) ->
    ContentBody = extract_message_binary(_Msg),
    ParsedMessage = parse(ContentBody),
    rabbit_router:match_bindings(Name, match_json_filter_routes(ParsedMessage)).

match_json_filter_routes(Payload) ->
    fun(#binding{args = Args, destination = Dst}) ->
        {_,VHOST,_,QNAME} = Dst,
        case rabbit_misc:table_lookup(Args, <<?JSON_FILTER_HEADER>>) of
            {longstr, _} ->
                ParsedFilter = persistent_term:get({?MODULE, VHOST, QNAME}),
                json_filter:do_filter(Payload, ParsedFilter);
            _ -> false
          end
        end.

extract_message_binary(Msg) ->
  {_, _, {_, _, _, _, _, BinaryDataList}, _} = Msg,
  erlang:iolist_to_binary(lists:reverse(BinaryDataList)).

validate(_X) -> ok.
validate_binding(_X, _B) -> ok.
create(_Serial, _X) -> ok.
delete(_Serial, _X) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Serial, _X, _B) -> 
    {_,_,_,{_,VHOST,_,QNAME},_Headers} = _B,
    [{<<?JSON_FILTER_HEADER>>, _, Filters}] = [{Key, longstr, _Filters} || {Key, longstr, _Filters} <- _Headers, Key =:= <<?JSON_FILTER_HEADER>>],
    ParsedFilters = parse(Filters),
    persistent_term:put({?MODULE, VHOST, QNAME}, ParsedFilters),
    ok.

remove_bindings(_Serial, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).
