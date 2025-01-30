# from basilisp_blender.nrepl import server_start
# shutdown_fn = server_start(nrepl_port_filepath="<project-root-path>/.nrepl-port")
#
# from basilisp_blender.nrepl import server_start

# shutdown_fn = server_start(
#     nrepl_port_filepath="/Users/zcpowers/Desktop/molnodes/.nrepl-port"
# )


import sys
import os
from basilisp_blender.nrepl import server_start


def start_nrepl_server():
    project_root = os.getcwd()  # Or specify your project root path
    nrepl_port_file = os.path.join(project_root, ".nrepl-port")
    shutdown_fn = server_start(nrepl_port_filepath=nrepl_port_file)
    print(f"nREPL server started. Port file: {nrepl_port_file}")
    return shutdown_fn


shutdown_fn = start_nrepl_server()
