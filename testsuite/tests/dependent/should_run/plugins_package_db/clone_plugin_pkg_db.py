#!/usr/bin/env python3
from pathlib import Path
import subprocess
import sys

ghc_bin, libdir, ghc_pkg, pkg_db_str = sys.argv[1:5]
comp_dir = Path(ghc_bin).resolve().parent
pkg_db = Path(pkg_db_str)
pkg_db.mkdir(parents=True, exist_ok=True)
libdir_p = Path(libdir).resolve()

for conf in (libdir_p / "package.conf.d").glob("*.conf"):
    bname = conf.name
    txt = conf.read_text().replace("${pkgroot}", libdir_p.as_posix())
    if (comp_dir / "modules").is_dir() and (comp_dir / "package").is_dir() and bname.startswith("ghc-10."):
        txt = txt.replace("import-dirs:", f'import-dirs:\n    "{comp_dir.as_posix()}/modules"', 1)
        txt = txt.replace("library-dirs:", f'library-dirs:\n    "{comp_dir.as_posix()}/package"', 1)
        txt = txt.replace("dynamic-library-dirs:", f'dynamic-library-dirs: "{comp_dir.as_posix()}/package" ', 1)
    (pkg_db / bname).write_text(txt)

subprocess.run([ghc_pkg, "--package-db", pkg_db.as_posix(), "recache"], check=True, stdout=subprocess.DEVNULL)
