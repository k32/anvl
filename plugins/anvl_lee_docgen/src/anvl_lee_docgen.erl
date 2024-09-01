-module(anvl_lee_docgen).

-behavior(anvl_plugin).

-export([model/0, conditions/1, init/0, hooks/0]).

model() ->
    #{anvl_lee_codegen =>
	  #{ docbook =>
		 {[map, cli_action],
		  #{ key_elements => []
		   , cli_operand => "lee-docbook"
		   },
		  #{ 
		   }}
	   }
     }.

hooks() ->
    #{}.

conditions(_ProjectRoot) ->
    [].

init() ->
    io:format("

Hello frim plugin!!


", []).
