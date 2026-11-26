#!/usr/bin/env python3
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys

libdir, ghc_pkg, out_dir = sys.argv[1], sys.argv[2], Path(sys.argv[3])
original_db = Path(libdir) / "package.conf.d"
copy_db = out_dir / "db"
interfaces = out_dir / "interfaces"
copy_db.mkdir(parents=True, exist_ok=True)
interfaces.mkdir(parents=True, exist_ok=True)

pkgroot = original_db.parent.resolve().as_posix()
[internal] = original_db.glob("ghc-internal-*.conf")
for registration in original_db.glob("*.conf"):
    contents = registration.read_text().replace("${pkgroot}", pkgroot)
    if registration == internal:
        def field(name):
            return rf"(?m)^{name}:[^\n]*(?:\n[ \t][^\n]*)*"

        paths = re.search(field("import-dirs"), contents).group().split(":", 1)[1]
        for path in map(Path, shlex.split(paths)):
            for suffix in ("*.hi", "*.dyn_hi"):
                for hi in path.rglob(suffix):
                    dest = interfaces / hi.relative_to(path)
                    if not dest.exists():
                        dest.parent.mkdir(parents=True, exist_ok=True)
                        shutil.copyfile(hi, dest)
        contents = re.sub(field("import-dirs"), lambda _: f'import-dirs: "{interfaces.resolve().as_posix()}"', contents)
        for name in ("library-dirs", "library-dirs-static", "dynamic-library-dirs", "bytecode-library-dirs"):
            contents = re.sub(field(name), "", contents)
    (copy_db / registration.name).write_text(contents)

subprocess.run([ghc_pkg, f"--package-db={copy_db}", "recache"], check=True, stdout=subprocess.DEVNULL)
