"""Load the installed IWFM library and check its version and API generation."""
import ctypes
import os
import sys
from pathlib import Path

prefix = Path(sys.prefix)
if os.name == "nt":
    lib = prefix / "Library" / "bin" / "IWFM_C_x64.dll"
else:
    lib = prefix / "lib" / "libiwfm_c.so"
dll = ctypes.CDLL(str(lib))

n = ctypes.c_int(500)
buf = ctypes.create_string_buffer(n.value)
status = ctypes.c_int(0)
dll.IW_GetVersion(ctypes.byref(n), buf, ctypes.byref(status))
text = buf.value.decode()
print(text)
assert status.value == 0, status.value
assert text.startswith("IWFM"), text

expected = os.environ["PKG_VERSION"]
assert expected in text, f"library reports {text!r}, package is {expected}"

# Handle-based C API: every model procedure takes the model ID; there is no
# active-model switch.
assert not hasattr(dll, "IW_Model_Switch")
for name in ("IW_Model_New", "IW_Model_GetNNodes", "IW_Budget_OpenFile",
             "IW_ZBudget_OpenFile", "IW_Model_FEInterpolate"):
    assert hasattr(dll, name), name
print("libiwfm OK")
