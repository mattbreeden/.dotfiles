#!/bin/sh

if [ -s ~/.Xresources ]; then
    xrdb -merge ~/.Xresources
fi

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

exec i3
