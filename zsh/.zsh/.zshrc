###########################################################################
#                                                                         #
#                           This is .zshrc file                           #
#               inital setup file for only interactive zsh                #
#              This file is read after .zshenv file is read.              #
#                                                                         #
###########################################################################


autoload colors
colors

#
# Terminal title
#
function _window_title_cmd () {
    local pwd="${PWD/~HOME/~}"
    print -n "\e]0;"
    print -n "${pwd##*/} (${HOST%%.*})"
    print -n "\a"
}

function _window_title_exec () {
    local pwd="${PWD/~HOME/~}"
    print -n "\e]0;"
    print -n "${1%% *}:${pwd##*/} (${HOST%%.*})"
    print -n "\a"
}

autoload -Uz add-zsh-hook
[[ "$TERM" =~ "^xterm" ]] && {
    add-zsh-hook precmd _window_title_cmd
    add-zsh-hook preexec _window_title_exec
}

#
# Promptの設定
#

autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:*' enable git svn hg # 少しだけ高速化
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{magenta}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{yellow}+"
zstyle ':vcs_info:*' formats "%F{cyan}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }

nprom () {
    local GRAY=$'%{\e[1;30m%}'
    local LIGHT_GRAY=$'%{\e[0;37m%}'
    local WHITE=$'%{\e[1;37m%}'
    local LIGHT_BLUE=$'%{\e[1;36m%}'
    local YELLOW=$'%{\e[1;33m%}'
    local PURPLE=$'%{\e[1;35m%}'
    local GREEN=$'%{\e[1;32m%}'
    local BLUE=$'%{\e[1;34m%}'
    local RED=$'%{\e[1;31m%}'
    local ORIG_COLOR=$'%{\e[0m%} '
    local lf=$'\n'
    if ! [ "$EMACS" ];then
       HEADER='%B%F{%(?|green|%18(?|green|%148(?|yellow|%130(?|yellow|red))))}%n@%m%f%b{%F{014}%~%f}%F{cyan}$vcs_info_msg_0_%f'
       PROMPT=$HEADER%4(~|$lf|)'%F{green}%#%f '
       RPROMPT=' (%j jobs)'
    fi
       if [ "$EMACS" ];then
          PROMPT=%(?|$RED|%18(?|$RED|$YELLOW))"%n@%m{%~}"$'\n'\
                  "%#"\
                  $ORIG_COLOR
       fi
          PROMPT2=$PURPLE"%_ >"$ORIG_COLOR
          PROMPT3=$GREEN"?#"$ORIG_COLOR
          SPROMPT=$YELLOW"correct: %R -> %r ?"$ORIG_COLOR
}
nprom

#
# 履歴の設定
#

HISTFILE=~/.zsh/.zsh_history
HISTSIZE=50000
SAVEHIST=10000
setopt hist_ignorespace         # 先頭がスペースで始まる場合追加しない
setopt hist_ignore_dups         # # 直前と同じコマンドラインは追加しない
setopt hist_no_store     # history (fc -l) コマンドを追加しない
setopt extended_history hist_save_nodups # 時刻を記録 / ダブりを削除
setopt share_history inc_append_history  # 履歴ファイルを共有 / 即座に履歴を書き出す

#
# alias
#

# 履歴に保存しないコマンド
alias fg=' fg' bg=' bg' history=' history'
alias cd=' cd'
alias exit=' exit'

# for interactive operation
alias rm='rm -i' cp='cp -i' mv='mv -i'
alias copy='cp -ip' del='rm -i' move='mv -i'
alias fullreset='echo "\ec\ec"'
h () {history $* | less}

# alias for some shortcuts for different directory listings
alias ls=' ls -hF --color=auto'
alias ll=' ls -la' la=' ls -A' l=' ls -CF'
alias l.='ls -d .*'

# alias for tmux
alias pbcopy='tee > /dev/clipboard'

# grep
alias grep='grep --color=auto'
alias rgrep='grep -r'

# misc
alias e='emacsclient'
alias pu=pushd po=popd dirs='dirs -v'
alias cte='crontab -e'

#
# Set Shell option
#

# setopt auto_remove_slash
setopt auto_name_dirs
setopt prompt_subst transient_rprompt
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars
# setopt sh_word_split
setopt auto_param_keys pushd_ignore_dups
setopt interactive_comments
setopt auto_cd # ディレクトリ名でcdできるように
# setopt auto_pushd # 移動したディレクトリを記録
unsetopt bgnice

#
# binding keys
#
bindkey -e
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

#
# 補完
#
zstyle :compinstall filename '~/.zsh/.zshrc'
autoload -Uz compinit && compinit -u
zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時小文字は大文字もかねる
zstyle ':completion:*:default' menu select          # 矢印キーで候補選択

autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs

zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

#
# Functions
#

# cdする度に ls するけど、長すぎるときは省略する
chpwd() {
    ls_abbrev
}
ls_abbrev() {
    local cmd_ls='ls'
    local -a opt_ls
    opt_ls=('-aCF' '--color=always')
    local ls_result
    ls_result=$(CLICOLOR_FORCE=1 COLUMNS=$COLUMNS command $cmd_ls ${opt_ls[@]} | sed $'/^\e\[[0-9;]*m$/d')

    local ls_lines=$(echo "$ls_result" | wc -l | tr -d ' ')

    if [ $ls_lines -gt 10 ]; then
        echo "$ls_result" | head -n 5
        echo '...'
        echo "$ls_result" | tail -n 5
        echo "$(command ls -1 -A | wc -l | tr -d ' ') files exist"
    else
        echo "$ls_result"
    fi
}

# Return で ls と git status 表示
function do_enter() {
    zle accept-line
    if [[ -z "$BUFFER" ]]; then
        echo ''
        ls
        # ls_abbrev
        if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
            echo
            echo -e "\e[0;33m--- git status ---\e[0m"
            git status -sb
        fi
    fi
}
zle -N do_enter
bindkey '^m' do_enter

# w3m で google検索する
function google() {
    if [ $# != 0 ]; then
        URL1="http://www.google.co.jp/search?hl=ja&rls=ig&q=";
        URL2="&aq=f&aqi=g-e10&aql=&oq=&gs_rfai="
        w3m $URL1$*$URL2
    else
        w3m "https://www.google.co.jp"
    fi
}

# w3m で wikipedia を検索する
function wiki_ja() {
    if [ $# != 0 ]; then
        w3m "http://ja.wikipedia.org/wiki/$*?ref=sa"
    else
        w3m "http://ja.wikipedia.org/wiki/Main_Page"
    fi
}
function wiki_en() {
    if [ $# != 0 ]; then
        w3m "http://en.wikipedia.org/wiki/$*?ref=sa"
    else
        w3m "http://en.wikipedia.org/wiki/Main_Page"
    fi
}

# easy extract
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.tar.xz)    tar xvJf $1    ;;
            *.tar)       tar xvf $1     ;;
            *.tbz2)      tar xvjf $1    ;;
            *.tgz)       tar xvzf $1    ;;
            *.zip)       unzip $1       ;;
            *.lzma)      lzma -dv $1    ;;
            *.zoo)       zoo x $1       ;;
            *.lzh)       lharc x $1     ;;
            *.arc)       arc x $1       ;;
            *.shar)      unshar $1      ;;
            *.rar)       unrar x $1     ;;
            *.7z)        7z x $1        ;;
            *.gz)        gunzip $1      ;;
            *.dz)        dictunzip $1   ;;
            *.bz2)       bunzip2 $1     ;;
            *.xz)        unxz $1        ;;
            *.Z)         uncompress $1  ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

#
# peco
#

# 過去に実行したコマンドを選択。ctrl-rにバインド
function peco-select-history() {
    BUFFER=$(\history -n -r 1 | peco --query "$LBUFFER" | sed 's/\\n/\n/')
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

# search a destination from cdr list
function peco-get-destination-from-cdr() {
    cdr -l | \
        sed -e 's/^[[:digit:]]*[[:blank:]]*//' | \
        peco --query "$LBUFFER"
}

# 過去に移動したことのあるディレクトリを選択。ctrl-zにバインド
function peco-cdr() {
    local destination="$(peco-get-destination-from-cdr)"
    if [ -n "$destination" ]; then
        BUFFER="cd $destination"
        zle accept-line
    else
        zle reset-prompt
    fi
}
zle -N peco-cdr
bindkey '^z' peco-cdr

# zstyle ':chpwd:*' recent-dirs-file ~/.zsh_histories/.chpwd-recent-dirs

# for ghq & GitHub CLI
peco-src () {
    local repo=$(ghq list | peco --query "$LBUFFER")
    if [ -n "$repo" ]; then
        repo=$(ghq list --full-path --exact $repo)
        cd ${repo}
    fi
}
alias g='peco-src'
alias ghb='pushd $(ghq root)/$(ghq list | peco) && gh browse && popd'

# ブランチの検索. git co B でブランチ切り替え.
alias -g B='`git branch | peco --prompt "GIT BRANCH>" | head -n 1 | sed -e "s/^\*\s*//g"`'
# コミットハッシュの検索 2種. git show C, git reset --hard C, etc.
alias -g C='`git log --oneline | peco | cut -d" " -f1`'
alias -g L='`lg`'
# 最近の操作を検索. git reset --hard R, etc.
alias -g R='`git reflog | peco | cut -d" " -f1`'

function lg {
    line=`\git log --pretty=format:'%h <%an> -%d %s' --abbrev-commit --graph --no-color | peco`
    if [ $line ]; then
        `echo "$line" | awk 'match($0,/[a-f0-9]+/) {print substr($0,RSTART,RLENGTH)}'`
    fi
}

# interactiveにdocker exec
alias de='docker exec -it $(docker ps | peco | cut -d " " -f 1) /bin/bash'

# interactive な pgrep/pkill
function ppgrep() {
    if [[ $1 == "" ]]; then
        PECO=peco
    else
        PECO="peco --query $1"
    fi
    ps aux | eval $PECO | awk '{ print $2 }'
}

function ppkill() {
    if [[ $1 =~ "^-" ]]; then
        QUERY=""            # options only
    else
        QUERY=$1            # with a query
        [[ $# > 0 ]] && shift
    fi
    ppgrep $QUERY | xargs kill $*
}

# for Emacs vterm-mode
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# auto zcompile
if [ ~/.zsh/.zshrc -nt ~/.zsh/.zshrc.zwc ]; then
   zcompile ~/.zsh/.zshrc
fi

# プロファイル(計測)
# if (which zprof > /dev/null 2>&1) ;then
#   zprof
# fi

###
###
### end of this file
