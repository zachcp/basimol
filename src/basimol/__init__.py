import logging

from basilisp import main as basilisp
from basilisp.lang import compiler


COMPILER_OPTS = compiler.compiler_opts()
basilisp.init(COMPILER_OPTS)

LOGGER = logging.getLogger("basimol")
LOGGER.addHandler(logging.StreamHandler())
