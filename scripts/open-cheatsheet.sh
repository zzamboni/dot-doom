#!/bin/sh
# Reuse Evince's registered document, including across Flatpak sandboxes.
set -eu
repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
pdf="$repo_dir/cheatsheet.pdf"
uri=$(python3 -c 'import pathlib,sys; print(pathlib.Path(sys.argv[1]).as_uri())' "$pdf")
if command -v gdbus >/dev/null 2>&1; then
    owner=$(gdbus call --session --timeout 3 \
        --dest org.gnome.evince.Daemon --object-path /org/gnome/evince/Daemon \
        --method org.gnome.evince.Daemon.FindDocument "$uri" false 2>/dev/null \
        | sed -n "s/^(':\([0-9][0-9.]*\)',)$/:\1/p") || owner=
    if [ -n "$owner" ]; then
        if gdbus call --session --timeout 5 --dest "$owner" \
            --object-path /org/gnome/evince/Evince \
            --method org.gnome.evince.Application.Reload '{}' 0 >/dev/null; then
            exit 0
        fi
    fi
fi
exec flatpak run org.gnome.Evince "$pdf"
