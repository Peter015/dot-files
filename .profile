# This file is read each time a login shell is started.
# All other interactive shells will only read .bashrc; this is particularly
# important for language settings, see below.

test -z "$PROFILEREAD" && . /etc/profile || true

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)

export EDITOR=hx

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server


export XDG_DATA_DIRS="/home/your_user/.nix-profile/share:$XDG_DATA_DIRS"


export GOPATH=/home/peterz/Go/libs
export PATH="$PATH:$GOPATH/bin"
export GOPATH="$GOPATH:/home/peterz/Go/code"



