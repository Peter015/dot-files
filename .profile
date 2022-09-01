# This file is read each time a login shell is started.
# All other interactive shells will only read .bashrc; this is particularly
# important for language settings, see below.

test -z "$PROFILEREAD" && . /etc/profile || true

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)

export EDITOR=/usr/bin/helix

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server

# Some people don't like fortune. If you uncomment the following lines,
# you will have a fortune each time you log in ;-)

if [ -x /usr/bin/fortune ] ; then
    echo
    /usr/bin/fortune
    echo
fi



if [ -e /home/peterz/.nix-profile/etc/profile.d/nix.sh ]; then . /home/peterz/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# add the nix to XIR_DATA_DIRS to have access to .desktop files
export XDG_DATA_DIRS="$HOME/.nix-profile/share:${XDG_DATA_DIRS}"

export PATH="$PATH:/home/peterz/julia-1.8.0/bin"

export GOPATH=/home/peterz/Go/libs
export PATH="$PATH:$GOPATH/bin"
export GOPATH="$GOPATH:/home/peterz/Go/code"



