from enot.compiler.abstract import AbstractCompiler


class BootstrapCompiler(AbstractCompiler):
    def __init__(self, package, executable='./bootstrap'):
        super().__init__(package, executable)
