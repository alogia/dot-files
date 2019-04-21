#!/usr/bin/env python
import re
from subprocess import check_output

# The gmailauth file must be in the format:
# login XXXX clientid XXXX secret XXXX refresh XXXX

#goauthinfo = os.popen("gpg -q --no-tty -d ~/.gmailauth.gpg").read()



def construct_re(login):
    s = "login %s clientid ([^ ]+) secret ([^ ]+) refresh ([^ ]+)\n" % (login)
    return re.compile(s)


def get_oath_clientid(login):
    p = construct_re(login)
    return p.search(goauthinfo).group(1)


def get_oath_secret(login):
    p = construct_re(login)
    return p.search(goauthinfo).group(2)


def get_oath_refresh(login):
    p = construct_re(login)
    return p.search(goauthinfo).group(3)


def get_authinfo_password(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s" % (machine, login, port)
    p = re.compile(s)
    authinfo = check_output("gpg -dq ~/.authinfo.gpg", shell=True)
    return p.search(authinfo).group(1)
