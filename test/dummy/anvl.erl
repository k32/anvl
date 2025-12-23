-include("anvl.hrl").

conf() ->
  #{ plugins => [anvl_erlc]
   , conditions => [default]
   }.

init() ->
  anvl_erlc:add_pre_compile_hook(
    anvl_project:root(),
    fun(#{src_root := SrcRoot}) ->
        precondition(sources_created(SrcRoot, 1000))
    end).

default() ->
  anvl_erlc:app_compiled(default, dummy).

?MEMO(sources_created, Dir, N,
      begin
        newer([], filename(Dir, N)) andalso
          begin
            lists:foreach(
              fun(I) ->
                  FN = filename(Dir, I),
                  ok = file:write_file(FN, ["-module(", mod_name(I), ").\n"])
              end,
              lists:seq(1, N)),
            true
          end
      end).

filename(Dir, N) ->
  filename:join([Dir, "src", mod_name(N) ++ ".erl"]).

mod_name(N) ->
  "dummy_" ++ integer_to_list(N).
