#!/usr/bin/env python

import re
import os
#from subprocess import check_output

# The gmailauth file must be in the format:
# login XXXX clientid XXXX secret XXXX refresh XXXX


def get_authinfo_password(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s" % (machine, login, port)
    p = re.compile(s)
    #Use pgp encryption if desired.
    #authinfo = check_output("gpg -dq ~/.authinfo.gpg", shell=True)
    authinfo = open(os.path.expanduser("~/.authinfo")).read()
    return p.search(authinfo).group(1)
