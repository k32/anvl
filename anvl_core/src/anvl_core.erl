%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025 k32
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public License
%% version 3, as published by the Free Software Foundation
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%================================================================================

-module(anvl_core).
-moduledoc false.

-export([model/0, project_model/0]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

-doc false.
model() ->
  #{ root =>
       {[value, cli_param],
        #{ oneliner    => "Root project directory"
         , type        => string()
         , default     => "."
         , cli_operand => "root-dir"
         , cli_short   => $d
         }}
   , log =>
       #{ global_level =>
            {[value, os_env, cli_param, logger_level],
             #{ oneliner    => "Minimum severity of log messages"
              , default     => notice
              , type        => lee_logger:level()
              , cli_operand => "log-level"
              }}
        , default_handler_level =>
            {[value, os_env, logger_level],
             #{ oneliner       => "Log level for the default handler"
              , type           => lee_logger:level()
              , default_ref    => [log, global_level]
              , logger_handler => default
              }}
        }
   , custom_conditions =>
       {[value, cli_positional],
        #{ oneliner         => "List of conditions to satisfy"
         , doc              => "@anvl-custom-conditions"
         , type             => typerefl:list(atom())
         , cli_arg_position => rest
         }}
   , shell =>
       {[value, cli_param],
        #{ oneliner    => "Start Erlang shell after running the tasks"
         , type        => boolean()
         , cli_operand => "shell"
         , default     => false
         }}
   , debug =>
       #{ top =>
            #{ n_time =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Display top N slowest jobs"
                   , type        => union(non_neg_integer(), true)
                   , cli_operand => "top-time"
                   , default     => 0
                   }}
             , n_reds =>
                 {[value, cli_param, os_env],
                  #{ oneliner    => "Display top N jobs by reductions"
                   , type        => union(non_neg_integer(), true)
                   , cli_operand => "top-reds"
                   , default     => 0
                   }}
             }
        }
   , help =>
       #{ run =>
            {[value, cli_param],
             #{ oneliner    => "Get help and exit"
              , type        => boolean()
              , default     => false
              , cli_operand => "help"
              }}
        }
   }.

-doc false.
project_model() ->
   #{ plugins =>
       {[value],
        #{ oneliner => "List of plugins needed for the project"
         , type => list(anvl_plugin:t())
         , default => []
         }}
    , conditions =>
        {[value],
         #{ oneliner => "List of custom conditions declared by the project"
          , type => list(atom())
          , default => []
          }}
    }.
