%% @author bokner
%% @doc @todo Add description to default_handler.


-module(exml_default_handler).

-behaviour(exml_event_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  init/1,
  stream_start/4,
  element_start/4,
  element_end/2,
  cdata/2,
  document_start/4,
  document_end/2,
  stream_end/2]).

init(_) ->
  io:format("Created default handler.~n"),
  ignore.

stream_start(Name, XmlNS, Attrs, HandlerState) ->
  io:format("Stream started ~n"),
  HandlerState.

stream_end(Name, HandlerState) ->
  io:format("Stream ~s closed", [Name]),
  HandlerState.

element_start(Name, XmlNS, Attrs, HandlerState) ->
  io:format("<~s ~s>~n", [Name, attrs_to_iolist(Attrs, [])]),
  ignore.

element_end(Name, HandlerState) ->
  io:format("</~s>~n", [Name]),
  ignore.

cdata(CData, HandlerState) ->
  io:format("~s", [CData]),
  ignore.

document_start(Name, XmlNS, Attrs, HandlerState) ->
  io:format("Document ~s started~n", [Name]).

document_end(Name, HandlerState) ->
  io:format("Document ~s parsed", [Name]).
%% ====================================================================
%% Internal functions
%% ====================================================================
-spec attrs_to_iolist([{binary(), binary()}], iolist()) -> iolist().
attrs_to_iolist([], Acc) ->
  Acc;
attrs_to_iolist([{Name, Value} | Rest], Acc) ->
  attrs_to_iolist(Rest, [" ", Name, "='", Value, "'" | Acc]).


