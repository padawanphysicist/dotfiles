#!/usr/bin/gawk -e { if (/^```/) { i++; next } if ( i % 2 == 1) { print } }
# mdlp - agnostic literate programming for github flavored markdown.
# I release this script into the public domain.
# Rich Traube, Fri Feb 15 09:07:27 EST 2013
