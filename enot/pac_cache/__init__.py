import shlex
import subprocess


class Static:
    @staticmethod
    def get_erlang_version():
        try:
            vsn = subprocess.check_output(
                shlex.split("erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell"))
        # Try to figure out vsn via cat as when called from Erlang it fails to get version via erl
        except subprocess.CalledProcessError:
            with open('/usr/lib/erlang/releases/RELEASES', 'r') as rel:
                file = rel.read()
                return file.split(',')[2].strip('"')
        return vsn.decode('utf-8').strip("\n\r\"")
