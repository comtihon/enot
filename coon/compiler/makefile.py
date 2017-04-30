from coon.compiler.abstract import AbstractCompiler
from coon.packages.package import Package


class MakefileCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='make'):
        super().__init__(package, executable)
