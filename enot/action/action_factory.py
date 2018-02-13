from enot.action.action import Action, ActionType
from enot.action.release import Release
from enot.action.shell import Shell


def get_action(action_type: str, params: str or dict) -> Action:
    if ActionType(action_type) == ActionType.SHELL:
        return Shell(params)
    if ActionType(action_type) == ActionType.RELEASE:
        return Release(params)
    raise RuntimeError('Unknown action ' + action_type)
