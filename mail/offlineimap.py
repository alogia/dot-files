#!/usr/bin/env python
import re, os

#The gmailauth file must be in the format:
#login XXXX clientid XXXX secret XXXX refresh XXXX

authinfo = os.popen("gpg -q --no-tty -d ~/.gmailauth.gpg").read()


def construct_re(login):
    s = "login %s clientid ([^ ]+) secret ([^ ]+) refresh ([^ ]+)\n" % (login)
    return re.compile(s)


def get_oath_clientid(login):
    p = construct_re(login)
    return p.search(authinfo).group(1)


def get_oath_secret(login):
    p = construct_re(login)
    return p.search(authinfo).group(2)


def get_oath_refresh(login):
    p = construct_re(login)
    return p.search(authinfo).group(3)
