from coon.action.prebuild.action import Action, ActionType
from coon.action.prebuild.shell import Shell


def get_action(action_type: str, params: str) -> Action:
    if action_type == ActionType.SHELL.value:  # TODO ActionType(action_type) == ActionType.SHELL?
        return Shell(params)
    raise RuntimeError('Unknown action ' + action_type)
