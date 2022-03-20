%%%-------------------------------------------------------------------
%%% @author USER
%%% @copyright (C) 2022, <COMPANY>
%%% @doc 井字棋
%%% @end
%%%-------------------------------------------------------------------
-module(xo_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(FST_WIN, 1).
-define(SEC_WIN, 2).
-define(MID_WIN, 0).

-define(FST_SIDE, 1).
-define(SEC_SIDE, 0).

-record(xo_server_state, {show_list = [], list = [], turn = 1, all_match_num = 0}).
%%-record(xo_proc, {p1, p2, p3, p4, p5, p6, p7, p8, p9}).
-record(xo, {process= [],win_result = 0}).
-compile(export_all).
%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

do(Pos) ->
  gen_server:call(?MODULE, {do, Pos}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ets:new(xo,[set, named_table, public, {keypos, #xo.process}]),
  dets:open_file(xo , [{file , "./xo.dets"} , {keypos, #xo.process} , {type , set}]) ,
  ets:from_dets(xo, xo),
%%  start(),
  All = length(ets:tab2list(xo)),
  io:format("~w",[All]),
  List =  lists:duplicate(3, lists:duplicate(3, ' ')),
%%  self() ! {start},

  {ok, #xo_server_state{show_list = List}}.

handle_call({do, Pos}, _From, State = #xo_server_state{show_list = List, list = L, turn = Turn}) ->
  AllPos = lists:seq(1, 9),

  case lists:keymember(Pos, 2, L) orelse not lists:member(Pos, AllPos) of
    true -> {reply, error_pos, State};
    false ->

  List2 = pos_replace_list(Pos, List, Turn),
  L2 = L ++ [{Turn, Pos}],
  MatchPos =  [Pos2||{_Index, Pos2}<-L2],
  Choose = AllPos -- MatchPos,

  {_Side, WishResult} = case (Turn + 1) rem 2 of
              1 -> {?FST_SIDE, ?FST_WIN};
              _ -> {?SEC_SIDE, ?SEC_WIN}
            end,

  Result = check_win(L2, Turn, Pos), %%我的回合是否胜利
  {ListX, L4, TurnX} =  case Result of
    0 ->
      List3 = add_rate(L2, Turn, WishResult, List2, Choose),

%%      L3 = L2 ++ [{Turn + 1, MaxPos}],
%%      Result2 = check_win(L3, Turn + 1, MaxPos), %% ai的回合是否胜利
      %% AI选泽
%%      List4 = pos_replace_list(MaxPos, List3, Turn + 1),
%%      check_reset(Turn + 1, Result2, List3, L3);
      Result2 = Result,
                          check_reset(Turn, Result, List3, L2);
    _ ->
      Result2 = Result,
      check_reset(Turn, Result, List2, L2)
                        end,

  {reply, print(ListX, Result2), State#xo_server_state{show_list = ListX, list = L4, turn = TurnX}}

  end.

add_rate(L2, Turn, WishResult, List2, Choose) ->
  {List3, {_MaxPosX, _}} = lists:foldl(fun(Pos1, Acc0) ->
    {Acc, Max} = Acc0,
    {MaxPos, MaxRate} = Max,
    {MatchWin, All} = find_data(L2, Turn, Pos1, WishResult),
    WinRate = MatchWin / All,
    Max1 = case WinRate > MaxRate orelse MaxPos =:= 0 of
             true -> {Pos1, WinRate};
             _ -> Max
           end,
    Acc1 = pos_replace_list(Pos1, Acc, Turn, WinRate),
    {Acc1, Max1}
                                     end, {List2, {0, 0}}, Choose),
  List3.

check_reset(Turn, Result, List2, L2) ->
  if
    Turn =:= 9 -> send_rest();
    Result > 0 -> send_rest();
true -> ok
  end,
  {List2, L2, Turn + 1}.

send_rest() ->
 erlang:send_after(3000, ?MODULE, {reset_game}).

find_data(L2, Turn, Pos1, WishResult) ->
  MatchWin = ets:foldl(fun(#xo{process = Proc, win_result = WinRes}, Acc1) ->
    case lists:prefix(L2 ++ [{Turn + 1, Pos1}], Proc) andalso WinRes =:= WishResult of
      true -> [Proc|Acc1];
      _ -> Acc1
    end
                       end, [], xo),
  AllMatch = ets:foldl(fun(#xo{process = Proc}, Acc1) ->
    case lists:prefix(L2, Proc) of
      true -> [Proc|Acc1];
      _ -> Acc1
    end
                       end, [], xo),
%%  io:format("~w ~w \n",[MatchWin, L2]),
  {length(MatchWin), length(AllMatch)}.


pos_replace_list(Pos, List, Turn) ->
  Replace = case Turn rem 2 of
              1 -> 'O';
              _ -> 'X'
            end,
  pos_replace_list(Pos, List, Turn, Replace).

pos_replace_list(Pos, List, _Turn, Replace) ->
  {X, Y} = {(Pos - 1) rem 3 + 1, (Pos - 1) div 3 + 1},
  XList = lists:nth(Y, List),

  XList1 = replace_list(X, XList, [Replace]),
  List2 = replace_list(Y, List, [XList1]),
  List2.

handle_cast(_Request, State = #xo_server_state{}) ->
  {noreply, State}.

handle_info({stop}, State ) ->
  {stop, normal, State};

handle_info({start}, State = #xo_server_state{show_list = List}) ->
  List2 = add_rate([], 0, ?FST_WIN, List, lists:seq(1,9)),
  print(List2, 0),
  {noreply, State};

handle_info({reset_game}, State = #xo_server_state{}) ->
  {ShowList, List, T} = {lists:duplicate(3, lists:duplicate(3, ".")), [], 1},
  {noreply, State#xo_server_state{show_list = ShowList, list = List, turn = T}};

handle_info(_Info, State = #xo_server_state{}) ->
  {noreply, State}.

print(List, Result) ->
  io:format("~p\n", [lists:nth(1, List)]),
  io:format("~p\n", [lists:nth(2, List)]),
  io:format("~p\n", [lists:nth(3, List)]),
  case Result of
    ?FST_WIN -> io:format("O is winner\n");
    ?SEC_WIN -> io:format("X is winner\n");
    _ -> ok
  end.


terminate(_Reason, _State = #xo_server_state{}) ->
  ets:to_dets(xo, xo),
  dets:close(xo),
  ok.

code_change(_OldVsn, State = #xo_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start() ->
  AllPos = lists:seq(1, 9),
  [start([], 1, TurnChoose) ||TurnChoose <-AllPos] .

start(Match, Turn, H) ->
  AllPos = lists:seq(1, 9),
  MatchPos =  [Pos||{_Index, Pos}<-Match],
  Choose = AllPos -- MatchPos,
  TPos = Choose -- [H],
  Match1 = Match ++ [{Turn, H}],

  {_ThisTurn, NextTurn} = case  (Turn + 1) rem 2 of
    1 -> {?SEC_SIDE, ?FST_SIDE};
    _ -> {?FST_SIDE, ?SEC_SIDE}
  end,
%%  AtkChoose = choose(Match, Turn, ThisTurn, Choose),
  DfdChoose = choose(Match, Turn + 1, NextTurn, Choose),
  case check_win(Match1, Turn, H) of
    ?MID_WIN -> %%如果自己赢不了，不能让对手赢
      case DfdChoose of
        [] ->
          [ start(Match1, Turn + 1, NextPos) ||NextPos <- TPos],
          TPos =:= [] andalso ets:insert(xo, #xo{process = Match1, win_result = ?MID_WIN});
                  _ ->
                    [do_dfd_choose(Choose, DfdH, Match, Turn, Match1) || DfdH<-DfdChoose]
      end;

    HasWinner ->
      ets:insert(xo, #xo{process = Match1, win_result = HasWinner})
  end.

do_dfd_choose(Choose, DfdH, Match, Turn, Match1) ->
  Choose1 = Choose -- [DfdH],
  [start(Match ++ [{Turn, DfdH}], Turn + 1, NextPos) || NextPos <- Choose1],
  Choose1 =:= [] andalso ets:insert(xo, #xo{process = Match1, win_result = ?MID_WIN}).

choose(Match, Turn, NextTurn, Choose) ->
  lists:foldl(fun(H1, Acc) ->
    OtherRes = check_win(Match ++ [{Turn, H1}], Turn, H1),
    if
      OtherRes =:= ?FST_WIN andalso NextTurn =:= ?FST_SIDE -> [H1 | Acc];
      OtherRes =:= ?SEC_WIN andalso NextTurn =:= ?SEC_SIDE -> [H1 | Acc];
      true -> Acc
    end
                          end, [], Choose).

check_win(Matchs, Turn, Pos) ->
  {FstPos, SecPos} = lists:partition(fun({Index, _Pos}) ->
                  Index rem 2 =:= 1
                  end,Matchs),
  Poses = case Turn rem 2 of
    1 -> FstPos;
    _ -> SecPos
  end,
  {AllX, AllY} = all_x_y(Pos),
  Lean1 = [1,5,9],
  Lean2 = [3,5,7],
%%  io:format("~w",[{AllX, FstPos}]),
    XFull = lists:all(fun(Xpos) -> lists:keymember(Xpos,2, Poses) end, AllX),
  YFull = lists:all(fun(Ypos) -> lists:keymember(Ypos,2, Poses) end, AllY),
  Lean1Full = lists:all(fun(Lpos1) -> lists:keymember(Lpos1,2, Poses) end, Lean1),
  Lean2Full = lists:all(fun(Lpos2) -> lists:keymember(Lpos2,2, Poses) end, Lean2),
  case XFull orelse YFull orelse Lean1Full orelse Lean2Full of
    true ->
      case Turn rem 2 of
              1 -> ?FST_WIN;
        _ -> ?SEC_WIN
            end;
    _ -> ?MID_WIN
  end.

all_x_y(Pos) ->
  {X, Y} = {Pos rem 3,( Pos -1) div 3},
  AllX = lists:seq(Y * 3 + 1, Y * 3 + 3),
  AllY = lists:seq(X, 9, 3),
  {AllX, AllY -- [0]}.


result() ->
  All = length(ets:tab2list(xo)),
  FstWin = length(ets:match(xo, {'_', '$1',1})),
  SecWin = length(ets:match(xo, {'_', '$1',2})),
  NoWin = length(ets:match(xo, {'_', '$1',0})),

  {All, FstWin, SecWin, NoWin}.

my_guess(Poses, WinResult) ->
  Guess = ets:foldl(fun(#xo{process = Match, win_result = Win}, Acc) ->
    Match1 = [P||{_Index,P} <-Match],
    case lists:prefix(Poses, Match1) andalso WinResult =:= Win of
      true -> [Match| Acc];
      _ -> Acc
    end
                    end, [], xo),
  Guess.

replace_list(Pos, PosList, Replace) ->
  replace_list(Pos, PosList, Replace, [], 1).

replace_list(_Pos, [], _Replace, Res, _CurPos) -> lists:reverse(Res);
replace_list(Pos, PosList, Replace, Res, CurPos) ->
  [H | R] = PosList,
  case Pos =:= CurPos of
    true -> lists:reverse(Res) ++ Replace ++ R;
    false -> replace_list(Pos, R, Replace, [H | Res], CurPos +1)
  end.

test(L2, Turn, Pos1)->
  ets:foldl(fun(#xo{process = Proc, win_result = _WinRes} = XO, Acc1) ->
    case lists:prefix(L2 ++ [{Turn + 1, Pos1}], Proc)  of
      true -> Acc1 ++ [XO];
      _ -> Acc1
    end
                       end, [], xo).
