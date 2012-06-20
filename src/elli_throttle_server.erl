-module(elli_throttle_server).
-behaviour(gen_server).

%% API
-export([start_link/0, request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state,    {startup_time}).
-record(counters, {hour,
                   hourly_requests,
                   day,
                   daily_requests}).


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
    {ok, #state{startup_time = current_timestamp()}}.


handle_call(_, _From, State) ->
    {reply, ok, State}.


handle_cast({request, Id}, #state{startup_time = StartupTime} = State) ->
    Key = {elli_throttle, Id},
    put(Key, update_counters(get(Key), StartupTime)),
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

update_counters(undefined, StartupTime) ->
    #counters{hour = current_hour(StartupTime),
              hourly_requests = 1,
              day = current_day(StartupTime),
              daily_requests = 1};

update_counters(#counters{hour = TrackedHour,
                          hourly_requests = HourlyRequests,
                          day = TrackedDay,
                          daily_requests = DailyRequests}, StartupTime) ->

    {NewHour, NewHourlyRequests} =
        updated_hourly(current_hour(StartupTime), TrackedHour, HourlyRequests),
    {NewDay, NewDailyRequests} =
        updated_daily(current_day(StartupTime), TrackedDay, DailyRequests),
    #counters{hour = NewHour,
              hourly_requests = NewHourlyRequests,
              day = NewDay,
              daily_requests = NewDailyRequests}.


updated_hourly(CurrentHour, TrackedHour, Count)
  when CurrentHour =:= TrackedHour ->
    {TrackedHour, Count + 1};
updated_hourly(CurrentHour, _TrackedHour, _Count) ->
    {CurrentHour, 1}.

updated_daily(CurrentDay, TrackedDay, Count)
  when CurrentDay =:= TrackedDay ->
    {TrackedDay, Count + 1};
updated_daily(CurrentDay, _TrackedDay, _Count) ->
    {CurrentDay, 1}.


current_hour(StartupTime) -> (current_timestamp() - StartupTime) div 3600.
current_day(StartupTime)  -> (current_timestamp() - StartupTime) div 86400.

current_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
