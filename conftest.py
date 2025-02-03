import sys
import os

sys.path.append(os.path.join(os.path.dirname(__file__), "src"))

pytest_plugins = ["basilisp.contrib.pytest"]
