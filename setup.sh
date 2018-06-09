#!/bin/bash

function show_help
{
    echo "Usage:"
    echo "    --packages       Install packages"
    echo "    --links          Create links for configuration files from this repository to ~/"
    echo "    --printers       Download and install pretty-printers for GDB"
    echo "    --fonts          Download and install special fonts for emacs"
    echo "    --documentation  Download and install DASH documentation files"
    echo "    --shell          Install zsh and plugins"
    echo "    --all            Do all of the above"
    exit 1
}

if [ $# -eq 0 ]; then
    show_help
    exit 0
fi

for ARGUMENT in "$@"; do
    case $ARGUMENT in
        --packages)
        PACKAGES=TRUE
        ;;
        --links)
        LINKS=TRUE
        ;;
        --printers)
        PRINTERS=TRUE
        ;;
        --fonts)
        FONTS=TRUE
        ;;
        --documentation)
        DOCUMENTATION=TRUE
        ;;
        --shell)
        SHELL=TRUE
        ;;
        --all)
        PACKAGES=TRUE
        LINKS=TRUE
        PRINTERS=TRUE
        FONTS=TRUE
        DOCUMENTATION=TRUE
        SHELL=TRUE
        ;;
        *)
        show_help
        ;;
    esac
    shift
done

# set up symlinks
function make_link
{
    FILE=$1
    LINK=$2
    if [ -e "$LINK" ]; then
        if [ -L "$LINK" ]; then
            echo "Removing existing link $LINK."
            rm -Rf "$LINK"
        else
            BACKUP="$LINK.bak"
            echo "Renaming existing $LINK to $BACKUP"
            mv "$LINK" "$BACKUP"
        fi
    fi
    mkdir -p "$(dirname "$LINK")"
    echo "Creating $LINK -> $FILE"
    ln -s "$FILE" "$LINK"
}

if [ "$PACKAGES" = TRUE ]; then
    # install packages
    sudo dnf install PackageKit-command-not-found adobe-source-code-pro-fonts adobe-source-sans-pro-fonts adobe-source-serif-pro-fonts autojump-zsh clang clang-analyzer clang-devel clang-tools-extra cmake-gui colorgcc docker emacs-lucid gcc gdb gdouros-symbola-fonts git git-lfs htop ImageMagick iotop kcachegrind links llvm llvm-devel lnav progress pv recode rubygems sushi util-linux-user valgrind wireless-tools yank zsh

    # glances and modules
    sudo pip install glances psutil

    sudo pip install howdoi

    echo "Install Peco from https://github.com/peco/peco and RTags from https://github.com/Andersbakken/rtags manually."
fi

if [ "$LINKS" = TRUE ]; then
    DOTFILES_DIRECTORY=$(dirname "$(readlink -f "$0")")
    for FILE in $DOTFILES_DIRECTORY/home/.[!.]*; do
        LINK="$(realpath ~)/$(basename "$FILE")"
        make_link "$FILE" "$LINK"
    done

    make_link "$DOTFILES_DIRECTORY/home/cpp-entwickler.de-theme.el" "$(realpath ~)/.emacs.d/cpp-entwickler.de-theme.el"
    make_link "$DOTFILES_DIRECTORY/home/glances" "$(realpath ~)/.config/glances"
    make_link "$DOTFILES_DIRECTORY/home/mc" "$(realpath ~)/.config/mc"
    make_link "$DOTFILES_DIRECTORY/home/htoprc" "$(realpath ~)/.config/htop/htoprc"
    make_link "$DOTFILES_DIRECTORY/home/peco.json" "$(realpath ~)/.config/peco/config.json"
    
    EMACS_USER_FILE=~/.emacs.user
    GIT_USER_FILE=~/.gitconfig.user
    if [ ! -s "$EMACS_USER_FILE" ] || [ ! -s "$GIT_USER_FILE" ]; then
        USER_NAME=$(/bin/grep -P "^$(whoami):" /etc/passwd | cut -f5 -d:)
        read -r -e -p "Please enter your full name:     " -i "$USER_NAME" USER_NAME
        read -r -e -p "Please enter your email address: " -i "$(whoami)@$(dnsdomainname)" EMAIL
        read -r -e -p "Please enter the (short) company name: " COMPANY_NAME
        read -r -e -p "Please enter the full company name: " -i "$COMPANY_NAME" COMPANY_FULL_NAME

        if [ -e "$GIT_USER_FILE" ]; then
            mv "$GIT_USER_FILE" "$GIT_USER_FILE"".bak"
        fi
        echo -e "[user]\nname = $USER_NAME\nemail = $EMAIL" > "$GIT_USER_FILE"
        
        function read_xml
        {
            local IFS=\>
            read -r -d \< TAG VALUE
        }
        
        while read_xml; do
            case $TAG in
                City)
                    CITY="$VALUE"
                ;;
                Latitude)
                    LATITUDE="$VALUE"
                ;;
                Longitude)
                    LONGITUDE="$VALUE"
                ;;
                *)
                ;;
            esac
        done <<< "$(wget -qO- freegeoip.net/xml)"
        
        read -r -e -p "Please enter your location name: " -i "$CITY" CITY
        read -r -e -p "Please enter your latitude:      " -i "$LATITUDE" LATITUDE
        read -r -e -p "Please enter your longitude:     " -i "$LONGITUDE" LONGITUDE

        if [ -e "$EMACS_USER_FILE" ]; then
            mv "$EMACS_USER_FILE" "$EMACS_USER_FILE"".bak"
        fi
        echo "(defvar username \"$(whoami)\")
(setq company-name \"$COMPANY_NAME\"
      company-full-name \"$COMPANY_FULL_NAME\"
      user-full-name \"$USER_NAME\"
      user-mail-address \"$EMAIL\"
      calendar-latitude $LATITUDE
      calendar-longitude $LONGITUDE
      calendar-location-name \"$CITY\")
(define-abbrev-table 'global-abbrev-table '((\"$(whoami)\" \"$USER_NAME\")))" > "$EMACS_USER_FILE"
    fi

    make_link "$DOTFILES_DIRECTORY/home/IDE.desktop" "$(realpath ~)/.local/share/applications/IDE.desktop"
    systemctl enable --user emacs
fi

if [ "$PRINTERS" = TRUE ]; then
    # install pretty printers for gdb
    echo "Downloading GDB pretty printers."
    PRETTY_PRINTER_DIR=~/.gdb/
    mkdir -p $PRETTY_PRINTER_DIR/stl
    CPP_PRINTER_FILES="__init__.py printers.py xmethods.py"
    for FILE in $CPP_PRINTER_FILES; do
        wget --quiet --no-directories --output-document="$PRETTY_PRINTER_DIR/stl/$FILE" "https://gcc.gnu.org/git/?p=gcc.git;a=blob_plain;f=libstdc%2B%2B-v3/python/libstdcxx/v6/$FILE"
    done
    mkdir -p $PRETTY_PRINTER_DIR/qt
    QT_PRINTER_FILES="helper.py kde.py qt.py"
    for FILE in $QT_PRINTER_FILES; do
        wget --quiet --no-directories --output-document="$PRETTY_PRINTER_DIR/qt/$FILE" "https://cgit.kde.org/kdevelop.git/plain/debuggers/gdb/printers/$FILE"
    done
fi

if [ "$FONTS" = TRUE ]; then
    # install all-the-icons fonts
    echo "Downloading fonts."
    FONT_DIR=~/.fonts/
    ALL_THE_ICONS_FONTS="all-the-icons file-icons fontawesome octicons weathericons"
    for FONT in $ALL_THE_ICONS_FONTS; do
        wget --quiet --timestamping --no-directories --directory-prefix="$FONT_DIR" "https://github.com/domtronn/all-the-icons.el/raw/master/fonts/$FONT.ttf"
    done
    make_link "$DOTFILES_DIRECTORY/home/10-symbols.conf" "$(realpath ~)/.config/fontconfig/conf.d/10-symbols.conf"
fi

if [ "$DOCUMENTATION" = TRUE ]; then
    # install dash docsets
    echo "Installing DASH docsets."
    DOCSET_DIR=~/.docsets
    DOCSETS="Bash Boost C C++ CMake Docker Emacs_Lisp GLib HTML Man_Pages OpenGL_4 Qt_5 SVG XSLT"
    for DOCSET in $DOCSETS; do
        wget --quiet --show-progress --timestamping --no-directories --directory-prefix="$DOCSET_DIR" "http://sanfrancisco.kapeli.com/feeds/$DOCSET.tgz"
        tar xfz "$DOCSET_DIR/$DOCSET.tgz" --overwrite -C "$DOCSET_DIR"
        rm "$DOCSET_DIR/$DOCSET.tgz"
    done
fi

# install zsh plugins
function install_zsh_plugin
{
    PLUGIN_ADDRESS=$1
    PLUGIN_NAME=$(basename "$PLUGIN_ADDRESS")

    PLUGIN_DIRECTORY=$OH_MY_ZSH_DIR/custom/plugins/$PLUGIN_NAME
    if [ ! -d "$PLUGIN_DIRECTORY" ]; then
        git clone --quiet "$PLUGIN_ADDRESS" "$PLUGIN_DIRECTORY"
    else
        cd "$PLUGIN_DIRECTORY" || exit
        git pull --quiet
    fi
}

if [ "$SHELL" = TRUE ]; then
    # install oh-my-zsh
    OH_MY_ZSH_DIR=~/.oh-my-zsh
    if [ ! -d "$OH_MY_ZSH_DIR" ]; then
        git clone --quiet https://github.com/robbyrussell/oh-my-zsh.git "$OH_MY_ZSH_DIR"
    fi

    install_zsh_plugin https://github.com/kalpakrg/setenv
    install_zsh_plugin https://github.com/Tarrasch/zsh-bd
    install_zsh_plugin https://github.com/hlissner/zsh-autopair
    install_zsh_plugin https://github.com/zsh-users/zsh-autosuggestions
    install_zsh_plugin https://github.com/oknowton/zsh-dwim
    install_zsh_plugin https://github.com/RobSis/zsh-reentry-hook
    install_zsh_plugin https://github.com/zsh-users/zsh-syntax-highlighting
    install_zsh_plugin https://github.com/chrissicool/zsh-256color
    install_zsh_plugin https://github.com/jimeh/zsh-peco-history
    
    # change default shell
    sudo chsh -s "$(which zsh)" "$(whoami)" 2> /dev/null
fi
