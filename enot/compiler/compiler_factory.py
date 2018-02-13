from enot.compiler.abstract import AbstractCompiler
from enot.compiler.bootstrap import BootstrapCompiler
from enot.compiler.compiler_type import Compiler
from enot.compiler.enot import EnotCompiler
from enot.compiler.erlang_mk import ErlangMKCompiler
from enot.compiler.makefile import MakefileCompiler
from enot.compiler.rebar import RebarCompiler
from enot.global_properties import GlobalProperties
from enot.packages.package import Package


def get_compiler(global_config: GlobalProperties, define: str, package: Package) -> AbstractCompiler:
    if global_config.compiler == Compiler.NATIVE:
        return select_compiler(package.config.get_compiler(), define, package)
    else:
        return select_compiler(global_config.compiler, define, package)


def select_compiler(compiler: Compiler, define: str, package: Package):
    if compiler == Compiler.ENOT:
        return EnotCompiler(package, define)
    if compiler == Compiler.REBAR:
        return RebarCompiler(package)  # TODO how to determine rebar3?
    if compiler == Compiler.ERLANG_MK:
        return ErlangMKCompiler(package)
    if compiler == Compiler.MAKEFILE:
        return MakefileCompiler(package)
    if compiler == Compiler.BOOTSTRAP:
        return BootstrapCompiler(package)
    raise RuntimeError('Unknown compiler ' + compiler.value + ' for ' + package.name)
