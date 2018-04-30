#!/bin/sh

if [ -s ~/.Xresources ]; then
    xrdb -merge ~/.Xresources
fi

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi


# Disable touchpad while typing
syndaemon -i 1 -K -d
xbindkeys

hsetroot -solid "#000000"
nm-applet &

exec i3
