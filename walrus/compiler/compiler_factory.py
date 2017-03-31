from walrus.compiler import AbstractCompiler, Compiler, MakefileCompiler
from walrus.compiler import ErlangMKCompiler
from walrus.compiler import RebarCompiler
from walrus.compiler import WalrusCompiler
from walrus.global_properties import WalrusGlobalProperties
from walrus.packages.config import ConfigFile


def get_compiler(walrus_global_config: WalrusGlobalProperties, package_config: ConfigFile) -> AbstractCompiler:
    if walrus_global_config.compiler == Compiler.NATIVE:
        return select_compiler(package_config.get_compiler(), package_config)
    else:
        return select_compiler(walrus_global_config.compiler, package_config)


# TODO ensure system has compilers installed (try to install them if not).
# TODO ensure projects have compilers in case of using system non-walrus. (obtain them + config, if don't).
def select_compiler(compiler: Compiler, package_config: ConfigFile):
    if compiler == Compiler.WALRUS:
        return WalrusCompiler(package_config)
    if compiler == Compiler.REBAR:
        return RebarCompiler()
    if compiler == Compiler.ERLANG_MK:
        return ErlangMKCompiler()
    if compiler == Compiler.MAKEFILE:
        return MakefileCompiler(package_config)
    raise RuntimeError('Unknown compiler ' + compiler.value + ' for ' + package_config.name)
