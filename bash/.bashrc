# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

unset rc

# History
HISTSIZE=100000

# TeXLive
export MANPATH=$MANPATH:/usr/local/texlive/2024/texmf-dist/doc/man:
export INFOPATH=$INFOPATH:/usr/local/texlive/2024/texmf-dist/doc/info:
export PATH=$PATH:/usr/local/texlive/2024/bin/x86_64-linux:$HOME/.local/bin

# Go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

# Rust
. "$HOME/.cargo/env"

# Volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
