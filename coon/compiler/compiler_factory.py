from coon.compiler.abstract import AbstractCompiler
from coon.compiler.bootstrap import BootstrapCompiler
from coon.compiler.compiler_type import Compiler
from coon.compiler.coon import CoonCompiler
from coon.compiler.erlang_mk import ErlangMKCompiler
from coon.compiler.makefile import MakefileCompiler
from coon.compiler.rebar import RebarCompiler
from coon.global_properties import GlobalProperties
from coon.packages.package import Package


def get_compiler(global_config: GlobalProperties, define: str, package: Package) -> AbstractCompiler:
    if global_config.compiler == Compiler.NATIVE:
        return select_compiler(package.config.get_compiler(), define, package)
    else:
        return select_compiler(global_config.compiler, define, package)


def select_compiler(compiler: Compiler, define: str, package: Package):
    if compiler == Compiler.COON:
        return CoonCompiler(package, define)
    if compiler == Compiler.REBAR:
        return RebarCompiler(package)  # TODO how to determine rebar3?
    if compiler == Compiler.ERLANG_MK:
        return ErlangMKCompiler(package)
    if compiler == Compiler.MAKEFILE:
        return MakefileCompiler(package)
    if compiler == Compiler.BOOTSTRAP:
        return BootstrapCompiler(package)
    raise RuntimeError('Unknown compiler ' + compiler.value + ' for ' + package.name)
