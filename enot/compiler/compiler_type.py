from enum import Enum


class Compiler(Enum):
    ENOT = 'enot'  # prefer enot
    ERLANG_MK = 'erlang.mk'  # prefer erlang.mk
    REBAR = 'rebar'  # use rebar everywhere
    REBAR3 = 'rebar3'  # use rebar3 compiler
    NATIVE = 'native'  # use found by conf compiler (rebar.config, or erlang.mk exists)
    MAKEFILE = 'makefile'  # just call Makefile
    BOOTSTRAP = 'bootstrap'  # for those projects, who are afraid of Makefiles. Just call bootstrap in project's root
