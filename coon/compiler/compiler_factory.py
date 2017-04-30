from coon.compiler.abstract import AbstractCompiler
from coon.compiler.compiler_type import Compiler
from coon.compiler.erlang_mk import ErlangMKCompiler
from coon.compiler.rebar import RebarCompiler
from coon.compiler.coon import CoonCompiler
from coon.compiler.makefile import MakefileCompiler
from coon.compiler.bootstrap import BootstrapCompiler
from coon.packages.package import Package

from coon.global_properties import GlobalProperties


def get_compiler(global_config: GlobalProperties, package: Package) -> AbstractCompiler:
    if global_config.compiler == Compiler.NATIVE:
        return select_compiler(package.config.get_compiler(), package)
    else:
        return select_compiler(global_config.compiler, package)


# TODO ensure system has compilers installed (try to install them if not).
# TODO ensure projects have compilers in case of using system non-coon. (obtain them + config, if don't).
def select_compiler(compiler: Compiler, package: Package):
    if compiler == Compiler.COON:
        return CoonCompiler(package)
    if compiler == Compiler.REBAR:
        return RebarCompiler(package)
    if compiler == Compiler.ERLANG_MK:
        return ErlangMKCompiler(package)
    if compiler == Compiler.MAKEFILE:
        return MakefileCompiler(package)
    if compiler == Compiler.BOOTSTRAP:
        return BootstrapCompiler(package)
    raise RuntimeError('Unknown compiler ' + compiler.value + ' for ' + package.name)
