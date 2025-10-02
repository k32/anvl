%% anvl macros and imports are contained in this header:
-include("anvl.hrl").

conf() ->
  #{ conditions => [executable_built]
   }.

?MEMO(source_compiled, Src, Obj,
      begin
        newer(Src, Obj) andalso
          anvl_lib:exec("gcc", ["-c", "-o", Obj, Src])
      end).

?MEMO(executable_built,
      begin
        Executable = "build/hello",
        %% Collect source files:
        Sources = filelib:wildcard("c_src/*.c"),
        %% Derive names of object files:
        Objs = lists:map(fun obj_name/1, Sources),
        precondition([source_compiled(Src, obj_name(Src)) || Src <- Sources]) or
          newer(Objs, Executable) andalso
          anvl_lib:exec("gcc", ["-o", Executable | Objs])
      end).

obj_name(Src) ->
  patsubst("build/${basename}.o", Src).
