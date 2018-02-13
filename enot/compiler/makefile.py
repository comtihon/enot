from enot.compiler.abstract import AbstractCompiler


class MakefileCompiler(AbstractCompiler):
    def __init__(self, package, executable='make'):
        super().__init__(package, executable)
