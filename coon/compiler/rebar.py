from coon.compiler.abstract import AbstractCompiler, run_cmd


class RebarCompiler(AbstractCompiler):
    def compile(self):
        return run_cmd([self.executable, 'compile'], self.project_name, self.root_path)

    def unit(self) -> bool:
        return run_cmd([self.executable, 'eunit'], self.project_name, self.root_path)

    def common(self, log_dir: str) -> bool:  # TODO log_dir
        return run_cmd([self.executable, 'ct'], self.project_name, self.root_path)
