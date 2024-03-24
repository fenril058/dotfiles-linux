###########################################################################
#                                                                         #
#                          This is .zshenv file                           #
#                                                                         #
#      inital setup file for both interactive and noninteractive zsh      #
#                                                                         #
###########################################################################

# プロファイル(計測)
# zmodload zsh/zprof && zprof

export LANG=en_US.UTF-8
export ZDOTDIR=$HOME/.zsh
export EDITOR=vim
export VISUAL=vim

# For emacs
if [ "$EMACS" ];then
   export TERM=Eterm-color
fi

# For hunspell
export DICTIONARY=en_US
export WORDLIST=$HOME/.hunspell_dict

# TeXLive
export MANPATH=$MANPATH:/usr/local/texlive/2024/texmf-dist/doc/man:
export INFOPATH=$INFOPATH:/usr/local/texlive/2024/texmf-dist/doc/info:
export PATH=$PATH:/usr/local/texlive/2024/bin/x86_64-linux

# Go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

# Rust
. "$HOME/.cargo/env"

# for pyenv
if [[ -e '~/.pyenv' ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
fi

# rbenv
if [[ -e '~/.rbenv' ]]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init --no-rehash - zsh)"
    (rbenv rehash &) 2> /dev/null
fi

# volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# haskell
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

# bin
export PATH=$HOME/.local/bin:$PATH

# auto zcompile
if [ ~/.zshenv -nt ~/.zshenv.zwc ]; then
   zcompile ~/.zshenv
fi

