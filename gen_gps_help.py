#!/usr/bin/env python

import os
import os.path
import re

pkg_re = re.compile(r"^(private)?\s*package\s*(\S+)")


def recursive_ls(dir):
    """Return the list of ads files in dir and its subdirs"""
    result = set()
    for f in os.listdir(dir):
        if f.endswith(".ads") and f.startswith("gnatcoll-"):
            private = False
            pkg = ""
            for line in open(os.path.join(dir, f)).readlines():
                m = pkg_re.search(line)
                if m:
                    private = m.group(1)
                    pkg = m.group(2)
                    break

            if not private:
                result.add((pkg, os.path.splitext(f)[0]))

        elif os.path.isdir(os.path.join(dir, f)):
            result = result.union(recursive_ls(os.path.join(dir, f)))

    return result


list = recursive_ls("..")
out = open("help_gnatcoll-db.py", "w")
out.write(
    """XML = r'''<?xml version="1.0"?>
<GPS>
"""
)

for pkg, f in sorted(list):
    if "__" in f:
        # An internal package with a specific naming scheme
        continue

    menu = pkg.replace(".", "/").replace("_", "__")

    # Do we have a submenu ?
    in_front = False
    for _, b in list:
        if b.startswith(f + "-"):
            item = menu[menu.rfind("/") + 1 :]
            menu = menu + "/&lt;" + item + "&gt;"
            break

    out.write(
        """  <documentation_file>
     <shell>Editor.edit "%(file)s.ads"</shell>
     <descr>%(package)s</descr>
     <menu>/Help/%(menu)s</menu>
     <category>GNAT Components Collection</category>
  </documentation_file>

"""
        % {"file": f, "menu": menu, "package": pkg}
    )

out.write(
    """</GPS>'''
import GPS
GPS.parse_xml(XML)
"""
)
out.close()
