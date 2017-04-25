from coon.compiler import AbstractCompiler, Compiler
from coon.compiler import ErlangMKCompiler
from coon.compiler import RebarCompiler
from coon.compiler import CoonCompiler
from coon.compiler import MakefileCompiler
from coon.compiler import BootstrapCompiler
from coon.global_properties import GlobalProperties
from coon.packages.config import ConfigFile


def get_compiler(global_config: GlobalProperties, package_config: ConfigFile) -> AbstractCompiler:
    if global_config.compiler == Compiler.NATIVE:
        return select_compiler(package_config.get_compiler(), package_config)
    else:
        return select_compiler(global_config.compiler, package_config)


def get_package_compiler(package_config: ConfigFile) -> AbstractCompiler:
    return select_compiler(package_config.get_compiler(), package_config)


# TODO ensure system has compilers installed (try to install them if not).
# TODO ensure projects have compilers in case of using system non-coon. (obtain them + config, if don't).
def select_compiler(compiler: Compiler, package_config: ConfigFile):
    if compiler == Compiler.COON:
        return CoonCompiler(package_config)
    if compiler == Compiler.REBAR:
        return RebarCompiler(package_config)
    if compiler == Compiler.ERLANG_MK:
        return ErlangMKCompiler(package_config)
    if compiler == Compiler.MAKEFILE:
        return MakefileCompiler(package_config)
    if compiler == Compiler.BOOTSTRAP:
        return BootstrapCompiler(package_config)
    raise RuntimeError('Unknown compiler ' + compiler.value + ' for ' + package_config.name)
