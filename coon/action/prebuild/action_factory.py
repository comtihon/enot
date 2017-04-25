from coon.action.prebuild import Action, ActionType, Shell


def get_action(action_type: str, params: str) -> Action:
    if action_type == ActionType.SHELL.value:
        return Shell(params)
    raise RuntimeError('Unknown action ' + action_type)
