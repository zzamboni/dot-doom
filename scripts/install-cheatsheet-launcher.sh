#!/bin/sh
# Register the launcher and its icon with the current user's desktop.
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
data_dir=${XDG_DATA_HOME:-"$HOME/.local/share"}

install_link() {
    source=$1
    destination=$2
    if [ -e "$destination" ] && [ ! -L "$destination" ]; then
        printf 'Refusing to replace existing file: %s\n' "$destination" >&2
        exit 1
    fi
    mkdir -p -- "$(dirname -- "$destination")"
    ln --symbolic --force --no-dereference -- "$source" "$destination"
    printf 'Registered %s\n' "$destination"
}

if command -v desktop-file-validate >/dev/null 2>&1; then
    desktop-file-validate "$repo_dir/doom-cheatsheet.desktop"
fi
install_link "$repo_dir/doom-cheatsheet.desktop" \
    "$data_dir/applications/doom-cheatsheet.desktop"
mkdir -p "$data_dir/icons/hicolor/512x512@2/apps"
install_link "$repo_dir/splash/doom-cheatsheet-icon.png" \
    "$data_dir/icons/hicolor/512x512@2/apps/doom-cheatsheet.png"

if command -v gtk-update-icon-cache >/dev/null 2>&1; then
    gtk-update-icon-cache --force --ignore-theme-index "$data_dir/icons/hicolor"
fi
