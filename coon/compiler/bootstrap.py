from coon.compiler.abstract import AbstractCompiler
from coon.packages.package import Package


class BootstrapCompiler(AbstractCompiler):
    def __init__(self, package: Package, executable='./bootstrap'):
        super().__init__(package, executable)
