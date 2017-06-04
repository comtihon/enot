from coon.compiler.rebar import RebarCompiler


class Rebar3Compiler(RebarCompiler):
    def compile(self):
        super().compile()

    def unit(self) -> bool:
        return super().unit()

    def common(self, log_dir: str) -> bool:
        return super().common(log_dir)
