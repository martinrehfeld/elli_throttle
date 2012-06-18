-module(elli_throttle_server).
-behaviour(gen_server).

%% API
-export([start_link/0, request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


request(Id) ->
    gen_server:cast(?MODULE, {request, Id}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.


handle_call(_, _From, State) ->
    {reply, ok, State}.


handle_cast({request, _Id}, State) ->
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
