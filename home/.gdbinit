# save history
set history save on
set history size 10000
set history remove-duplicates unlimited
set history expansion on

# disable confirmation
set confirm off

# disable pagination
set pagination off

# use zsh prompt format, show frame and thread
set extended-prompt \[\e[38;5;250;48;5;69m\]GDB \[\e[38;5;231m\]\f \[\e[38;5;250m\]thread \[\e[38;5;231m\]\t\[\e[0m\]\[\e[38;5;69m\]\n\[\e[38;5;231m\]▶ 
show extended-prompt

# print all frame arguments in backtrace
set print frame-arguments all

# print complete arrays
set print array on
set print array-indexes on
set print elements unlimited

# print object runtime information
set print object on

# show status of asynchronous processes
set exec-done-display on

# do not break on SIG32
handle SIG32 pass nostop

# set default number format
set output-radix 0x10
set input-radix 0x10

# Enable pretty-printing
set print pretty on

python
import sys
from os.path import expanduser

sys.path.insert(0, expanduser("~/.gdb"))
from stl import register_libstdcxx_printers 
register_libstdcxx_printers (None) 

sys.path.insert(0, '~/.gdb/Boost-Pretty-Printer')
import boost
boost.register_printers()

sys.path.insert(0, expanduser("~/.gdb/qt"))
from qt import register_qt_printers
register_qt_printers (None)
from kde import register_kde_printers
register_kde_printers (None)

end

# include user-specific config
source ~/.gdbinit.user

# allow breakpoints to be set later
set breakpoint pending on

# skip standard library sources
skip -rfunction ^std::.*\(
skip -rfunction ^boost::.*\(

# automatically break when this is called
#break function-name

# remove startup message
shell clear
