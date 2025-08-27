"""
Must use bin/app to get relative imports.
"""

import argparse
from .jsonrpyc import RPC
from .authenticated_reddit import AuthenticatedReddit

def main ():
    parser = argparse.ArgumentParser()
    parser.add_argument("--log", help="log filename")
    parser.add_argument("--localhost", help="ip or hostname of localhost", default='127.0.0.1')
    args = parser.parse_args()
    RPC(target=AuthenticatedReddit(check_for_updates=False,
                                   log_prefix=args.log,
                                   localhost=args.localhost))
