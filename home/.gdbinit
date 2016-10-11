python
import sys
from os.path import expanduser

sys.path.insert(0, expanduser("~/.gdb"))
from stl import register_libstdcxx_printers 
register_libstdcxx_printers (None) 

sys.path.insert(0, expanduser("~/.gdb/qt"))
from qt import register_qt_printers
register_qt_printers (None)
from kde import register_kde_printers
register_kde_printers (None)

end
