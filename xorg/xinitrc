[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

#eval "$(dbus-launch --sh-syntax --exit-with-session)"
# Start a D-Bus session
#source /etc/X11/xinit/xinitrc.d/80-dbus


xset fp+ /usr/share/fonts/terminus

urxvtd --quiet --opendisplay --fork
setxkbmap -option caps:escape dvorak

export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
fcitx &

#exec dbus-launch --sh-syntax --exit-with-session xmonad

exec xmonad 

