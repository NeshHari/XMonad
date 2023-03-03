#!/usr/bin/env python

import imaplib

obj = imaplib.IMAP4_SSL("imap.gmail.com", 993)
# if using 2FA with GMail, generate App Password (https://myaccount.google.com/security -> App passwwords)
obj.login("your email id", "password")
obj.select()
print(len(obj.search(None, "UnSeen")[1][0].split()))
