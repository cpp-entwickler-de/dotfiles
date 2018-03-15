# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
#ZSH_THEME="random"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Show elapsed time for command if longer than 10 seconds
REPORTTIME=30

# Show time output in hh:mm:ss.ttt format
TIMEFMT=$'\nreal   %*E\nuser   %*U\nsystem %*S\ncpu    %P'

# Close shell after 15 minutes of inactivity if last command finished successfully
LAST_EXIT_CODE=0
TIME_SINCE_LAST_COMMAND=0

TMOUT=1800
TRAPALRM() {
    if [ $? -eq 0 ]; then
        exit
    fi
}

# Notify user after long running command finishes
autoload -U add-zsh-hook

COMMAND_START_TIME=0
function cpped-save-timestamp() {
    COMMAND_START_TIME=$SECONDS
}

function cpped-notify-command-finished() {
    RETURN_CODE=$?
    TIME_NEEDED=$((SECONDS - COMMAND_START_TIME))
    if [ $TIME_NEEDED -gt $REPORTTIME ]; then
        TIME_OUTPUT="Time: "

        DAYS=$((TIME_NEEDED / 60 / 60 / 24))
        if [ $DAYS -gt 0 ]; then
            TIME_OUTPUT="${DAYS}d "
        fi

        HOURS=$((TIME_NEEDED / 60 / 60 % 24))
        if [ $HOURS -gt 0 ]; then
            TIME_OUTPUT+="${HOURS}:"
        fi

        MINUTES=$((TIME_NEEDED / 60 % 60))
        TIME_OUTPUT+="${MINUTES}:$((TIME_NEEDED % 60))s"

        if [ $RETURN_CODE -eq 0 ]; then
            ICON="utilities-terminal"
            RESULT="successfully"
            ERROR_INFO=""
            TIMEOUT=7200
        else
            ICON="emblem-important"
            RESULT="with error"
            ERROR_INFO="Code: $RETURN_CODE<br><br>"
            TIMEOUT=0
        fi

        COMMAND=$(fc -ln -1)
        notify-send --expire-time=$TIMEOUT --urgency=normal --icon="$ICON" "'$COMMAND' finished $RESULT." "$ERROR_INFO$TIME_OUTPUT"
    fi
}

add-zsh-hook preexec cpped-save-timestamp
add-zsh-hook precmd cpped-notify-command-finished

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(adb autojump bd colored-man-pages colorize command-not-found dirhistory extract git gnu-utils safe-paste setenv sudo systemd up wd zsh-256color zsh-autopair zsh-autosuggestions zsh-dwim zsh-peco-history zsh-reentry-hook zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 4 numeric
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true
zstyle :compinstall filename '/home/player/.zshrc'

# Set language environment
export LANG=en_US.UTF-8
export LC_CTYPE=de_DE.UTF-8
export LC_NUMERIC=de_DE.UTF-8
export LC_TIME=de_DE.UTF-8
export LC_MONETARY=de_DE.UTF-8
export LC_PAPER=de_DE.UTF-8
export LC_NAME=de_DE.UTF-8
export LC_ADDRESS=de_DE.UTF-8
export LC_TELEPHONE=de_DE.UTF-8
export LC_MEASUREMENT=de_DE.UTF-8

# Preferred editor for local and remote sessions
if [[ -z $SSH_CONNECTION ]]; then
    export EDITOR=emacsclient
else
    export EDITOR=nano
fi
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=nano

# Configure pager
export LESS_COMMAND="less --HILITE-UNREAD --quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --squeeze-blank-lines --tilde +Gg"
export PAGER=$LESS_COMMAND
export MANPAGER=$PAGER
export LESSCHARSET=utf-8
export GROFF_NO_SGR=1

# Select browser
if [ -n "$DISPLAY" ]; then
    export BROWSER=firefox
else 
    export BROWSER=links2
fi

autoload -U pick-web-browser
zstyle ':mime:*' x-browsers firefox
zstyle ':mime:*' tty-browsers links2
alias -s htm=pick-web-browser
alias -s html=pick-web-browser

# Set history size
export HISTSIZE=2000
export HISTFILESIZE=1000
export HISTFILE=~/.zsh_history
export HISTIGNORE="ls:ll:cd:fg:j:jobs"

# Show sizes in human readable format
BLOCK_SIZE=human-readable

# Disable user communication via terminal
mesg n

# Set default permission mask
umask 037

# Change directory without cd command
setopt autocd
# Automatically add directories to stack
setopt autopushd
# Try to expand unknown directory names as variables
setopt cdablevars
# Remove duplicate directory entries
setopt pushdignoredups
# Do not show stack after push
setopt pushdsilent
# Set cursor to end after completion
setopt alwaystoend
# Automatically insert end characters
setopt autoparamkeys
setopt autoparamslash
setopt autoremoveslash
# List completions horizontally
setopt listrowsfirst
# Save timestamps in history
setopt extended_history
# Append to history file
setopt appendhistory
# Disable history bell
setopt nohistbeep
# Remove superfluous whitespace
setopt histreduceblanks
# Remove older duplicates from history
setopt histsavenodups
# Do not add commands beginning with space to history
setopt histignorespace
# When expanding history, do not execute immediately
setopt histverify
# Write history invrementally instead of at exit
setopt incappendhistory
# Share history between processes
setopt share_history
# Do not overwrite files
setopt noclobber
# Correct spelling
setopt correctall
# Print exit value if not 0
setopt printexitvalue
# Ask when calling rm with *
setopt normstarsilent
# Print hexadecimals like C
setopt cbases
setopt octalzeroes
# Disable bell
setopt nobeep
# Use emacs keybindings
setopt emacs
# Use existing process if available
setopt autoresume 

# Disable line editor in emacs shell mode
[[ $EMACS = t ]] && unsetopt zle

case $(tty) in /dev/tty[0-9]*)
     setterm -blength 0 
esac

# Skip directories with no files
zmodload zsh/parameter
function chpwd() {
    LIST=true
    if [[ ! $history[$HISTCMD] =~ "cd +[\./]+" ]]; then
        ITEMS=$(ls -A)
        ITEM_COUNT=$(echo $ITEMS | wc -l)
        if [[ $ITEM_COUNT = "1" && -d $ITEMS ]]; then
            cd "$ITEMS"
            LIST=false
        fi
    fi

    # list contents of final directory
    if [[ "$LIST" = true ]]; then
        l
    fi
}

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
alias cd..="cd .."
alias cp="cp -i"
alias du="du -h"
alias gti="git"
alias gitroot='cd $(git root)'

function helm()
{
    "$@" | peco --select-1
}

KILL_COMMAND=$(which kill)
function kill()
{
    for ARGUMENT in "$@"; do
        case $ARGUMENT in
            -*)
            local KILL_ARGUMENTS=($KILL_ARGUMENTS $ARGUMENT)
            ;;
            *)
            local PROCESS_ARGUMENTS=($PROCESS_ARGUMENTS $ARGUMENT)
            ;;
        esac
    done
    for ARGUMENT in $PROCESS_ARGUMENTS; do
        if [[ "$ARGUMENT" =~ "^[0-9]+$" ]]; then
            local PIDS=($PIDS $ARGUMENT)
        else
            local PROCESS_NAMES=($PROCESS_NAMES $ARGUMENT)
        fi
    done
    if [[ -n "$PROCESS_NAMES" || -z "$PIDS" ]]; then
        local PIDS=($PIDS $(ps aux | peco --query "$PROCESS_NAMES" | while IFS= read -r LINE; do 
                                                                         echo $LINE | tr -s " " | cut --delimiter=\  -f 2; 
                                                                     done))
    fi
    if [[ -n "$PIDS" ]]; then
        $KILL_COMMAND $KILL_ARGUMENTS $PIDS
    fi
}

alias dnf="sudo dnf"
alias log="sudo lnav"
alias sysinfo="glances -1 --tree --fs-free-space --process-short-name -C ~/.config/glances"

alias e='emacsclient --no-wait'
alias ag='ag --smart-case --pager="less -MIRFX"'
alias r=$LESS_COMMAND
alias l="ls -AFhl1v --color --group-directories-first $*"
alias lstree="l -R $*"
alias make="make -j"
alias mc="mc --nosubshell"
alias mkdir="mkdir -p"
alias mv="mv -i"
alias raw-peco=$(which peco)
alias peco="peco --layout=bottom-up --select-1"
alias p="ps aux"
alias t="htop"
alias top="htop"
alias x="extract"
alias yank="yank-cli"

alias dos2unix="recode ibmpc..lat1"
alias unix2dos="recode lat1..ibmpc"
alias unix2mac="recode lat1..mac"
alias mac2unix="recode mac..lat1"

alias magit='e --eval "(magit-status \"$(pwd)\")" &> /dev/null'

function smart-enter ()
{
    if [[ -n $BUFFER ]]; then
        zle reset-prompt
    else
        echo
        l
    fi
    zle accept-line
}

zle -N smart-enter
bindkey "^M" smart-enter

function ssh-connect ()
{
    CONNECTIONS=$(fc -ln -10000 | grep -E "^ssh\s" | sed -e 's/\s*$//' | sort | uniq -c | sort -nr | sed -e "s/^\s*[0-9]*\s//")
    if [ "$CONNECTIONS" ]; then
        SELECTION=$(echo "$CONNECTIONS" | raw-peco --prompt "Select a connection")
    else
        echo "No previous connections found."
    fi
    print -S "$SELECTION"
    eval $SELECTION
}

function set-password()
{
    USER=$(whoami)
    echo -n "Current Password: "
    OLD_PASSWORD=$(read -e -s)

    # determine new password
    PASSWORD=${1:-$(apg -a 0 -n 20 -M SNCL -m 10 -x 10 -y -t | peco | cut --delimiter=\  -f 1)}

    if [ -n "$PASSWORD" -a -n "$OLD_PASSWORD" ]; then
        if [ -n "$DOMAIN" -a -n "$DOMAIN_CONTROLLER" ]; then
            # change domain password
            echo -e -n "\nSetting domain password..."
            echo -e "$OLD_PASSWORD\n$PASSWORD" | smbpasswd -s -r $DOMAIN_CONTROLLER -U $USER
            if [ $? -eq 0 ]; then
                echo "ok"

                # change CIFS password
                echo -n "Setting CIFS password....."
                if [ -s ~/.cifs-credentials ]; then
                    sed -i -e "s/\(password=\).*/\1$PASSWORD/g" ~/.cifs-credentials
                else
                    echo "$PASSWORD" > ~/.cifs-credentials
                fi
                echo "ok"

                # change proxy password
                which cntlm
                if [ $? -eq 0 ]; then
                    echo -n "Setting proxy password...."
                    PASSWORD_NTLM=$(echo "$PASSWORD" | cntlm -a NTLM -u $USER -d $(domainname) -H)
                    PASSWORD_NT=$(echo $PASSWORD_NTLM | grep "PassNT ")
                    PASSWORD_LM=$(echo $PASSWORD_NTLM | grep "PassLM ")
                    sudo sed -i -e "s/PassNT.*/$PASSWORD_NT/g" -e "s/PassLM.*/$PASSWORD_LM/g" /etc/cntlm.conf
                    echo "ok"
                    echo -n "Restarting Proxy.........."
                    sudo systemctl restart cntlm
                    echo "ok"
                fi
            else
                return 1
            fi
        else
            echo "No Domain configured. Set DOMAIN and DOMAIN_CONTROLLER if you also want to change your domain password."
        fi

        # change linux password
        echo -n "Setting system password..."
        echo "$PASSWORD" | sudo passwd --stdin $USER > /dev/null
        echo "ok"

        echo
        echo "User:         $(whoami)"
        echo "New Password: $PASSWORD"
    fi
}

alias -s txt=r
alias -s md=r

alias -s bat=e
alias -s bin=e
alias -s c=e
alias -s cpp=e
alias -s cxx=e
alias -s h=e
alias -s hpp=e
alias -s hxx=e

alias -s cfg=e
alias -s config=e

alias -s el=e
alias -s org=e

alias -s pdf=e
alias -s ps=gv
alias -s dvi=xdvi

alias -s zip="unzip -l"
alias -s rar="unrar l"
alias -s tar="tar tf"
alias -s tar.gz="tar ztf"
alias -s tar.bz2="tar jtf"
alias -s ace="unace l"
alias -s log="tail -f"
alias -s png="display -antialias"
alias -s jpg="display -antialias"
alias -s jpeg="display -antialias"
alias -s svg="display -antialias"
alias -s xml=e
alias -s xsl=e
alias -s xslt=e

export LSCOLORS="XXfxhxhxCxdxdxBXBXxxxx"
export LS_COLORS="di=1;0;1:ln=35:so=37:pi=37:ex=1;32:bd=33:cd=33:su=1;31;1:sg=1;31;1:tw=0:ow=0"

# Ninja-build
export NINJA_STATUS="[%p] "
alias ninja=ninja-build
alias -s ninja=ninja-build

# Theme
if [[ $USER == "root" ]]; then
    PROMPT_COLOR=$fg[red]
else
    PROMPT_COLOR=$FG[250]
fi

if [ -n "$DISPLAY" ]; then
    PROMPT_SYMBOL="▶"
    LINE_BREAK_SYMBOL="↪"
    CALENDAR_SYMBOL="" # The FontAwesome calendar symbol
    SEPARATOR_SYMBOL=""
else
    PROMPT_SYMBOL=">"
    LINE_BREAK_SYMBOL="L"
    CALENDAR_SYMBOL="[1]"
    SEPARATOR_SYMBOL=""
fi

function user_host()
{
    if [[ -n $SSH_CONNECTION ]]; then
        if [ -n "$DISPLAY" ]; then
            USER_HOST=" %n %m" # The FontAwesome user and desktop symbols
        else
            DOCKER_ICON="%n@%M"
        fi
    else
        if [[ $LOGNAME != $USER ]]; then
            if [ -n "$DISPLAY" ]; then
                USER_HOST=" %n" # The FontAwesome user symbol
            else
                USER_HOST="%n"
            fi
        fi
    fi
    if [[ -n $USER_HOST ]]; then
        echo "%{$BG[237]%}%{$FG[245]%}$USER_HOST%{$BG[069]%}%{$FG[237]%}$SEPARATOR_SYMBOL"
    fi
}

function git_info()
{
    GIT_DIR=$(git rev-parse --git-dir 2>/dev/null)
    if [[ -n $GIT_DIR ]]; then
        REF=$(git symbolic-ref --short HEAD 2>/dev/null) || REF=$(git rev-parse --short HEAD 2> /dev/null)

        GIT_STATUS=$(git status --porcelain --ignore-submodules)
        if [[ -n "$GIT_STATUS" ]]; then
            if echo $GIT_STATUS | ag --silent -c "^[ ]?M" -- >/dev/null; then
                if [ -n "$DISPLAY" ]; then
                    MODIFIED="✎"
                else
                    MODIFIED="M"
                fi
            fi

            if echo $GIT_STATUS | ag --silent -c "^.?" -- >/dev/null; then
                if [ -n "$DISPLAY" ]; then
                    MODIFIED="$MODIFIED🟊"
                else
                    MODIFIED="$MODIFIED*"
                fi
            fi
        fi

        LOCAL=$(git rev-parse @ 2>&1)
        REMOTE=$(git rev-parse @{u} 2>&1)
        BASE=$(git merge-base @ @{u} 2>&1) # last common commit
        if [[ $LOCAL != $REMOTE ]]; then
            if [[ $BASE == $REMOTE ]]; then
                if [ -n "$DISPLAY" ]; then
                    SYNC="⇡"
                else
                    SYNC="^"
                fi
            fi
            if [[ $BASE == $LOCAL ]]; then
                if [ -n "$DISPLAY" ]; then
                    SYNC=$SYNC"⇣"
                else
                    SYNC=$SYNC"v"
                fi
            fi

            SYNC=" $SYNC"
        fi

        CONFLICTS=$(git diff --name-only --diff-filter=U)
        FILE_COUNT=$(echo -n "$CONFLICTS" | ag -c '^' --)
        if [[ "$FILE_COUNT" -gt 0 ]]; then
            TOTAL=$(echo $CONFLICTS | ag -c '^=======$' --)
            if [[ $TOTAL -gt 0 ]]; then
                if [ -n "$DISPLAY" ]; then
                    CONFLICTS="⚡"
                else
                    CONFLICTS="N"
                fi
                CONFLICTS="$CONFLICTS($FILES|$TOTAL)"
            fi
        fi

        if [ -d "$GIT_DIR/rebase-merge" -o -d "$GIT_DIR/rebase-apply" ]; then
            if [ -n "$DISPLAY" ]; then
                REBASE="☈"
            else
                REBASE="R"
            fi
        fi

        if [ -n "$DISPLAY" ]; then
            STATUS_SYMBOL=" "
        fi
        echo "$STATUS_SYMBOL $REF $MODIFIED$REBASE$SYNC $CONFLICTS"
    fi
}

function emoji-clock() {
    if [ -n "$DISPLAY" ]; then
        # Add 15 minutes to the current time and save the value as $minutes.
        (( minutes = $(date '+%_M') + 15 ))
        (( hour = $(date '+%_I') + $minutes / 60 ))
        # make sure minutes and hours don't exceed 60 nor 12 respectively
        (( minutes %= 60 ))
        (( hour %= 12 ))

        case $hour in
            0) clock="🕛"; [ $minutes -ge 30 ] && clock="🕧";;
            1) clock="🕐"; [ $minutes -ge 30 ] && clock="🕜";;
            2) clock="🕑"; [ $minutes -ge 30 ] && clock="🕝";;
            3) clock="🕒"; [ $minutes -ge 30 ] && clock="🕞";;
            4) clock="🕓"; [ $minutes -ge 30 ] && clock="🕟";;
            5) clock="🕔"; [ $minutes -ge 30 ] && clock="🕠";;
            6) clock="🕕"; [ $minutes -ge 30 ] && clock="🕡";;
            7) clock="🕖"; [ $minutes -ge 30 ] && clock="🕢";;
            8) clock="🕗"; [ $minutes -ge 30 ] && clock="🕣";;
            9) clock="🕘"; [ $minutes -ge 30 ] && clock="🕤";;
            10) clock="🕙"; [ $minutes -ge 30 ] && clock="🕥";;
            11) clock="🕚"; [ $minutes -ge 30 ] && clock="🕦";;
            *) clock="";;
        esac
        echo $clock
    else
        echo ""
    fi
}

PROMPT='
$(user_host)%B%{$BG[069]%}%{$FG[252]%}%8~%b%{$reset_colors%}%{$BG[235]%}%{$FG[069]%}$SEPARATOR_SYMBOL%{$FG[245]%}$(git_info)%E%{$reset_color%}
%{$PROMPT_COLOR%}$PROMPT_SYMBOL '
PROMPT2='%{$PROMPT_COLOR%}$LINE_BREAK_SYMBOL '
PROMPT3='%{$PROMPT_COLOR%}? '
RPROMPT='%{$(echotc UP 1)%}%B%{$BG[235]%}%{$FG[245]%}$CALENDAR_SYMBOL %D{%a, %x [%V]} $(emoji-clock) %*%b%{$reset_color%}%{$(echotc DO 1)%}'

if [ -e ~/.zshrc.user ]; then
    source ~/.zshrc.user
fi
