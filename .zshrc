# Enable fancy tab completion
fpath=($fpath $HOME/.zshfunc)
autoload -Uz compinit
compinit -u

unsetopt beep

bindkey -v

# Fix delay when entering command mode
export KEYTIMEOUT=1
bindkey -M vicmd "^[" undefined-key

# Sensible backspace
bindkey "^?" backward-delete-char
bindkey "^H" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^U" backward-kill-line

bindkey "^[[A" history-search-backward
bindkey "^[[B" history-search-forward
bindkey -M vicmd "^[[A" history-search-backward
bindkey -M vicmd "^[[B" history-search-forward
bindkey -M vicmd "j" history-search-backward
bindkey -M vicmd "k" history-search-forward

# Visual-line mode edits command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd "V" edit-command-line

tabs -4

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:$HOME/bin:$HOME/.cargo/bin"
export EDITOR="emacsclient -a '' -c"

alias ec="emacsclient -a '' -n"
alias ls="ls --color=auto"
alias l="ls -lah --color=auto"
alias diff="diff --color=auto"

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY_TIME

# Git prompt info
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )

setopt prompt_subst
PROMPT="%F{%(?.green.red)}[%3~]%f "
RPROMPT="\${vcs_info_msg_0_}"
ZLE_RPROMPT_INDENT=0

zstyle ":vcs_info:*" enable git

# Enable debug logging of vcs_info process
#zstyle ":vcs_info:*+*:*" debug true

zstyle ":vcs_info:git:*" get-revision true
zstyle ":vcs_info:git:*" check-for-changes true
zstyle ":vcs_info:git:*" check-for-stagged-changes true

zstyle ":vcs_info:git:*" formats "%b%u%c"
zstyle ":vcs_info:git:*" actionformats "(%F{yellow}%a%f) %b%u%c"

zstyle ":vcs_info:git:*" stagedstr "%F{green}+%f"
zstyle ":vcs_info:git:*" unstagedstr "%F{red}~%f"
untrackedstr="%F{yellow}*%f"
aheadbehindstr="%F{cyan}^%f"

zstyle ":vcs_info:git*+set-message:*" hooks git-fancy-branch git-untracked-files

function +vi-git-fancy-branch() {
    if [[ "${hook_com[branch]}" =~ "[~^]" ]]; then
        hook_com[branch]="${hook_com[revision]:0:9}"
    elif [[ "${hook_com[branch]}" =~ "/" ]]; then
        hook_com[branch]="${hook_com[branch]##remotes/}"
        hook_com[branch]="%F{blue}${hook_com[branch]%%/*}%f/${hook_com[branch]#*/}"
    elif git rev-parse --quiet --verify "origin/${hook_com[branch]}" 1> /dev/null 2>&1; then
        ahead=$(git rev-list "${hook_com[branch]}..origin/${hook_com[branch]}" --count 2> /dev/null)
        behind=$(git rev-list "origin/${hook_com[branch]}..${hook_com[branch]}" --count 2> /dev/null)
        if [ ${ahead:-0} -gt 0 ] || [ ${behind:-0} -gt 0 ]; then
            hook_com[branch]="$aheadbehindstr${hook_com[branch]}"
        fi
    fi
}

function +vi-git-untracked-files() {
    if git status --porcelain 2> /dev/null | grep "^??" 1> /dev/null 2>&1; then
        hook_com[unstaged]="$untrackedstr${hook_com[unstaged]}"
    fi

    # Add space between branch and status symbols if any symbols exist
    if [ "${hook_com[staged]}" -o "${hook_com[unstaged]}" ]; then
        hook_com[branch]="${hook_com[branch]} "
    fi
}

# Cursor indicates vi mode
function zle-line-init zle-keymap-select {
    if [ $KEYMAP = vicmd ]; then
        echo -ne "\e[2 q"
    else
        echo -ne "\e[6 q"
    fi
}

zle -N zle-line-init
zle -N zle-keymap-select

# Set cursor back to block before running a command
preexec() {
    echo -ne "\e[2 q"
}

export LS_COLORS="rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=1;33;100:st=30;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:"
# Enable directory colors in tab completion
zstyle ':completion:*' list-colors "$LS_COLORS"

source $HOME/.keys
