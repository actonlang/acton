"""
Acton LLDB plugin

Features:
- acton.bt: filtered backtrace with basic Acton demangling
- acton.locals: print locals with Acton demangled names
- acton.demangle: demangle a single symbol or the selected frame name

Load in LLDB:
  (lldb) command script import /path/to/repo/utils/lldb/acton.py
  (lldb) acton.bt
"""

from __future__ import annotations

import lldb  # type: ignore
import argparse
import os
import re
from typing import List, Optional


# ------------------------------
# Config and defaults
# ------------------------------

DEFAULT_HIDE_FILE_SUBSTR = [
    # "/base/rts/",
    # "/base/builtin/",
    "/deps/",
]

DEFAULT_HIDE_SYMBOL_REGEX = [
    r"^uv_",  # libuv internals
    r"^pthread_",
    # r"^__.*",  # many system trampolines, but could also conflict perhaps? TODO: figure this out
    r"^_sigtramp$",
    r"^start$",
    r"^start_wqthread$",
]

DEFAULT_HIDE_MODULE_SUBSTR = [
    "libsystem_",
    "libdispatch",
    "libdyld",
    "libobjc.A.dylib",
    "libc++.",
]


class Config:
    def __init__(self) -> None:
        self.hide_file_substrings: List[str] = list(DEFAULT_HIDE_FILE_SUBSTR)
        self.hide_symbol_res: List[re.Pattern[str]] = [
            re.compile(p) for p in DEFAULT_HIDE_SYMBOL_REGEX
        ]
        self.hide_module_substrings: List[str] = list(DEFAULT_HIDE_MODULE_SUBSTR)
        self.enable_demangle: bool = True
        self.show_original: bool = False
        self.enable_color: bool = True
        self.show_arg_values: bool = True


CFG = Config()

# ------------------------------
# Demangler (heuristic)
# ------------------------------

_re_q_sep = re.compile(r"Q_")
_re_d_magic = re.compile(r"D___")  # e.g. $ActorD___init__
_re_d_sep = re.compile(r"D_")      # general Derived separator, e.g. mainD_foo -> main.foo
_re_g_sep = re.compile(r"G_")


def demangle_acton(name: str) -> str:
    """Demangling of Acton/RTS symbol

    - Replace module separators: 'Q_' -> '.' (e.g. actonQ_rtsQ_X -> acton.rts.X)
    - Convert runtime method separators: 'D___' -> '.' (.__init__/__call__ etc.)
    - Convert generic/section separators: 'G_' -> '.'
    - Replace '$' with '.' to improve readability (e.g., to$int -> to.int)
    - Trim leading '$' or common C prefixes ("_" on Darwin) while preserving name content.
    """
    if not name:
        return name

    # Darwin symbols may be prefixed with '_'
    if name.startswith("_"):
        name = name[1:]

    # Quick return for plain C names, but still allow builtin/unboxed prefix stripping.
    # Include 'U_' so we don't early-return for unboxed locals like U_5v3.
    if ("$" not in name) and ("Q_" not in name) and ("D___" not in name) and ("G_" not in name) and ("B_" not in name) and ("U_" not in name):
        return name

    s = name
    s = _re_q_sep.sub(".", s)
    s = _re_d_magic.sub(".", s)
    s = _re_d_sep.sub(".", s)
    s = _re_g_sep.sub(".", s)
    # Drop builtin prefix 'B_' (B_str -> str, B_dict -> dict, etc.)
    s = re.sub(r"\bB_", "", s)
    # Drop unboxed prefix 'U_' used by codegen for C locals (U_5v3, U_tmp)
    s = re.sub(r"\bU_", "", s)
    # After removing U_, drop any leading numeric index left by codegen: 5v3 -> v3, 6v4 -> v4
    s = re.sub(r"^\d+([A-Za-z].*)$", r"\1", s)

    # Drop trailing ".local" produced from G_local (local method variant marker)
    if s.endswith(".local"):
        s = s[: -len(".local")]

    # Remove possible duplicate dots created by substitutions
    # s = re.sub(r"\.\.+", ".", s)

    # Trim leading dot if introduced
    s = s[1:] if s.startswith(".") else s
    return s


def maybe_demangle(name: str) -> str:
    if not CFG.enable_demangle:
        return name
    try:
        d = demangle_acton(name)
        return d
    except Exception:
        return name


# ------------------------------
# Utilities
# ------------------------------

def _frame_is_hidden(frame: lldb.SBFrame) -> bool:
    # By file path
    le = frame.GetLineEntry()
    if le and le.IsValid():
        fs = le.GetFileSpec()
        if fs and fs.IsValid():
            path = os.path.join(fs.GetDirectory() or "", fs.GetFilename() or "")
            for part in CFG.hide_file_substrings:
                if part in path:
                    return True

    # By module image
    mod = frame.GetModule()
    if mod and mod.IsValid():
        mod_name = mod.GetFileSpec().GetFilename() or ""
        for part in CFG.hide_module_substrings:
            if part in mod_name:
                return True

    # By symbol name regex
    sym_name = frame.GetFunctionName() or frame.GetSymbol().GetName() or ""
    for rex in CFG.hide_symbol_res:
        try:
            if rex.search(sym_name or ""):
                return True
        except Exception:
            # Ignore miscompiled regex
            pass

    return False


def _format_frame(idx: int, frame: lldb.SBFrame) -> str:
    pc = frame.GetPCAddress()
    load_addr = pc.GetLoadAddress(frame.GetThread().GetProcess().GetTarget())
    fn = frame.GetDisplayFunctionName() or frame.GetFunctionName() or frame.GetSymbol().GetName() or "?"
    demangled = maybe_demangle(fn)
    args_str = _format_args(frame)
    if CFG.show_original and demangled != fn:
        name_part = f"{demangled}{args_str} [{fn}]"
    else:
        name_part = f"{demangled}{args_str}"

    le = frame.GetLineEntry()
    if le and le.IsValid():
        fs = le.GetFileSpec()
        filename = fs.GetFilename() or "?"
        line_no = le.GetLine()
        # Colorize file and line to match LLDB feel
        loc = _fmt_file_line(filename, line_no)
    else:
        loc = "?"

    return f"#{idx:2d} 0x{load_addr:x} {name_part} at {loc}"


def _iter_frames(only_visible: bool, thread: Optional[lldb.SBThread] = None):
    if thread is None:
        thread = lldb.debugger.GetSelectedTarget().process.GetSelectedThread()
    if not thread or not thread.IsValid():
        return []
    frames = []
    for f in thread:
        if only_visible and _frame_is_hidden(f):
            continue
        frames.append(f)
    return frames


# ------------------------------
# LLDB Commands
# ------------------------------

def cmd_bt(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:  # noqa: D401
    """acton.bt: Print a filtered backtrace with demangling.

    Usage: acton.bt [-a|--all] [-f|--full] [--no-demangle] [--show-original]
           [--hide-regex REGEX ...] [--clear-hides]

    -a/--all: include hidden frames
    -f/--full: include all threads (not just selected)
    --no-demangle: disable demangling
    --show-original: show original name alongside demangled
    --hide-regex: add extra hide regex for symbol names (repeatable)
    --clear-hides: clear all custom hide regexes
    """
    try:
        parser = argparse.ArgumentParser(prog="acton.bt", add_help=False)
        parser.add_argument("-a", "--all", action="store_true")
        parser.add_argument("-f", "--full", action="store_true")
        parser.add_argument("--no-demangle", action="store_true")
        parser.add_argument("--show-original", action="store_true")
        parser.add_argument("--hide-regex", action="append", default=[])
        parser.add_argument("--clear-hides", action="store_true")
        parser.add_argument("--color", dest="color", action="store_true")
        parser.add_argument("--no-color", dest="no_color", action="store_true")
        parser.add_argument("--no-arg-values", action="store_true", help="do not print argument values in bt")

        opts = parser.parse_args(shlex_split(command))
    except SystemExit:
        # argparse error; show help
        result.PutCString(cmd_bt.__doc__ or "")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    CFG.enable_demangle = not opts.no_demangle
    CFG.show_original = bool(opts.show_original)
    if opts.color:
        CFG.enable_color = True
    if opts.no_color:
        CFG.enable_color = False
    if opts.no_arg_values:
        CFG.show_arg_values = False

    if opts.clear_hides:
        CFG.hide_symbol_res = []
    for r in (opts.hide_regex or []):
        try:
            CFG.hide_symbol_res.append(re.compile(r))
        except re.error as e:
            result.PutCString(f"Invalid regex '{r}': {e}")

    target = debugger.GetSelectedTarget()
    proc = target.process
    if not proc or not proc.IsValid():
        result.PutCString("No process. Run or attach first.")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    threads = list(proc)
    printed_any = False

    def print_thread(th: lldb.SBThread):
        nonlocal printed_any
        header = f"thread #{th.GetIndexID()}"
        result.PutCString(header)
        frames = _iter_frames(not opts.all, th)
        for idx, fr in enumerate(frames):
            result.PutCString(_format_frame(idx, fr))
        printed_any = True

    if opts.full:
        for th in threads:
            print_thread(th)
    else:
        print_thread(proc.GetSelectedThread())

    if not printed_any:
        result.PutCString("No frames to display.")


def cmd_locals(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:  # noqa: D401
    """acton.locals: Print current frame locals with demangled names.

    Usage: acton.locals [--no-demangle] [--all] [--args-only] [--no-hide-meta]

    Defaults mimic `frame variable` more closely:
    - show only in-scope arguments + locals
    - exclude statics and artificial (compiler-generated) variables
    """
    try:
        parser = argparse.ArgumentParser(prog="acton.locals", add_help=False)
        parser.add_argument("--no-demangle", action="store_true")
        parser.add_argument("--all", action="store_true", help="include statics, out-of-scope, artificial")
        parser.add_argument("--args-only", action="store_true", help="only show function arguments")
        parser.add_argument("--no-hide-meta", action="store_true", help="do not hide Acton meta vars (methods/class)")
        opts = parser.parse_args(shlex_split(command))
    except SystemExit:
        result.PutCString(cmd_locals.__doc__ or "")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    frame = exe_ctx.GetFrame() if hasattr(exe_ctx, "GetFrame") else None
    if not frame or not frame.IsValid():
        result.PutCString("No selected frame.")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    demangle_on = CFG.enable_demangle and not opts.no_demangle

    show_args = True
    show_locals = not opts.args_only
    show_statics = bool(opts.all)
    in_scope_only = not opts.all

    vals = frame.GetVariables(show_args, show_locals, show_statics, in_scope_only)

    def is_meta_var(val: lldb.SBValue, dname: str, type_name: str) -> bool:
        # Hide common Acton meta artifacts
        raw = val.GetName() or ""
        if hasattr(val, "IsArtificial") and val.IsArtificial():
            return True
        # Names that look like vtable/method tables
        if dname.endswith(".methods") or dname.endswith(".$class") or dname.endswith(".class"):
            return True
        # Types that are class/method tables
        if "G_class" in type_name or type_name.endswith(".class"):
            return True
        # Heuristic: internal $-prefixed or double-underscore names
        if raw.startswith("$") or raw.startswith("__"):
            return True
        return False

    printed = 0
    def _label_from_gcinfo(v: lldb.SBValue) -> Optional[str]:
        try:
            base = v.Dereference() if v.TypeIsPointerType() else v
            if not base or not base.IsValid():
                return None
            cls = base.GetChildMemberWithName("$class")
            if not cls or not cls.IsValid():
                return None
            cls_d = cls.Dereference() if cls.TypeIsPointerType() else cls
            gc = cls_d.GetChildMemberWithName("$GCINFO")
            if not gc or not gc.IsValid():
                return None
            addr = gc.GetValueAsUnsigned()
            if not addr:
                return None
            proc = v.GetTarget().GetProcess()
            err = lldb.SBError()
            s = proc.ReadCStringFromMemory(addr, 256, err)
            if (err.Success() or not err.Fail()) and s:
                return maybe_demangle(s)
            return None
        except Exception:
            return None
    for v in vals:
        if not v or not v.IsValid():
            continue
        name = v.GetName() or "?"
        # Hide CPS continuation argument in locals by default
        if name == "C_cont" and not opts.no_hide_meta:
            continue
        dname = demangle_acton(name) if demangle_on else name
        type_name = maybe_demangle(v.GetTypeName() or "")
        if not type_name or type_name in ("$WORD", "WORD"):
            lbl = _label_from_gcinfo(v)
            if lbl:
                type_name = lbl

        if not opts.no_hide_meta and is_meta_var(v, dname, type_name):
            continue

        try:
            # Prefer LLDB's summary providers (works in DAP), fallback to our eval-based summary
            summary = v.GetSummary() or _acton_value_summary(v, exe_ctx) or v.GetValue() or ""
        except Exception:
            summary = v.GetValue() or ""
        summary = (summary or "").strip()

        if type_name:
            line = f"{dname}: {type_name} = {summary}"
        else:
            line = f"{dname} = {summary}"
        result.PutCString(line)
        printed += 1

    if printed == 0:
        result.PutCString("(no in-scope arguments/locals)")


def cmd_demangle(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:  # noqa: D401
    """acton.demangle: Demangle a raw Acton/RTS name or the current frame.

    Usage: acton.demangle [raw_name]
           If no name is provided, demangle the selected frame function.
    """
    raw = command.strip()
    if not raw:
        frame = exe_ctx.GetFrame() if hasattr(exe_ctx, "GetFrame") else None
        if not frame or not frame.IsValid():
            result.PutCString("No selected frame and no name provided.")
            result.SetStatus(lldb.eReturnStatusFailed)
            return
        raw = frame.GetDisplayFunctionName() or frame.GetFunctionName() or frame.GetSymbol().GetName() or ""
    if not raw:
        result.PutCString("No symbol name found.")
        result.SetStatus(lldb.eReturnStatusFailed)
        return
    result.PutCString(maybe_demangle(raw))


# ------------------------------
# Registration
# ------------------------------

def __lldb_init_module(debugger: lldb.SBDebugger, _internal_dict) -> None:
    # Group commands under 'acton'
    # Register commands (multiple spellings for compatibility with LLDB parser)
    # Dotted form (may be ambiguous on some LLDB builds)
    _add_cmd(debugger, "acton.bt", cmd_bt, "Filtered backtrace with Acton demangling")
    _add_cmd(debugger, "acton.locals", cmd_locals, "Print locals with Acton demangled names")
    _add_cmd(debugger, "acton.demangle", cmd_demangle, "Demangle a symbol or current frame")
    # Hyphenated aliases
    _add_cmd(debugger, "acton-bt", cmd_bt, "Filtered backtrace with Acton demangling")
    _add_cmd(debugger, "acton-locals", cmd_locals, "Print locals with Acton demangled names")
    _add_cmd(debugger, "acton-demangle", cmd_demangle, "Demangle a symbol or current frame")
    # Group command with subcommands: `acton bt|locals|demangle`
    _add_cmd(debugger, "acton", cmd_root, "Acton command group: bt | locals | demangle")
    # Breakpoint helper
    _add_cmd(debugger, "acton.break", cmd_break, "Set breakpoint at Acton file:line")
    _add_cmd(debugger, "acton-bp", cmd_break, "Set breakpoint at Acton file:line")

    # Environment-driven config (optional)
    _apply_env_overrides()

    print("Acton LLDB plugin loaded. Commands: acton bt|locals|demangle; aliases: acton-bt, acton-locals, acton-demangle, acton.bt, acton.locals, acton.demangle")
    try:
        _register_formatters()
    except Exception:
        # Keep plugin usable even if formatters fail to register
        pass


def _add_cmd(debugger: lldb.SBDebugger, name: str, func, help_text: str) -> None:
    # Register a Python function as an LLDB command.
    # Keep this compatible with stock LLDB on macOS (no SBCommandPluginInterface).
    # Use --overwrite so reloads work without manual settings tweaks.
    debugger.HandleCommand(
        f"command script add --overwrite -f {__name__}.{func.__name__} {name}"
    )
    # Note: LLDB doesn't expose a stable API to set rich help text here.
    # The function's docstring is shown when you run `help {name}`.


def _apply_env_overrides() -> None:
    # ACTON_LLDB_HIDE_REGEX: comma-separated regexes
    rx = os.getenv("ACTON_LLDB_HIDE_REGEX")
    if rx:
        for r in rx.split(","):
            r = r.strip()
            if not r:
                continue
            try:
                CFG.hide_symbol_res.append(re.compile(r))
            except re.error:
                pass
    # ACTON_LLDB_NO_DEMANGLE: if set, disable demangling
    if os.getenv("ACTON_LLDB_NO_DEMANGLE"):
        CFG.enable_demangle = False
    # ACTON_LLDB_SHOW_ORIGINAL: if set, print original symbol alongside demangled
    if os.getenv("ACTON_LLDB_SHOW_ORIGINAL"):
        CFG.show_original = True
    if os.getenv("ACTON_LLDB_NO_COLOR"):
        CFG.enable_color = False
    if os.getenv("ACTON_LLDB_COLOR"):
        CFG.enable_color = True
    if os.getenv("ACTON_LLDB_NO_ARG_VALUES"):
        CFG.show_arg_values = False


# ------------------------------
# Data formatters (type summaries)
# ------------------------------

def _expr_opts() -> lldb.SBExpressionOptions:
    opts = lldb.SBExpressionOptions()
    opts.SetIgnoreBreakpoints(True)
    opts.SetUnwindOnError(True)
    opts.SetLanguage(lldb.eLanguageTypeC)
    return opts


def _read_cstring(valobj: lldb.SBValue, char_ptr_val: lldb.SBValue, max_len: int = 2048) -> Optional[str]:
    try:
        if not char_ptr_val or not char_ptr_val.IsValid():
            return None
        addr = char_ptr_val.GetValueAsUnsigned()
        if not addr:
            return None
        process = valobj.GetTarget().GetProcess()
        err = lldb.SBError()
        s = process.ReadCStringFromMemory(addr, max_len, err)
        if (err.Success() or not err.Fail()) and s is not None:
            return s
        return None
    except Exception:
        return None


def sum_B_str(valobj: lldb.SBValue, _internal_dict) -> Optional[str]:
    """Summary for B_str values: print quoted UTF-8 string."""
    try:
        ty = valobj.GetTypeName() or ""
        ptr = valobj.GetValue() or ""
        # Prefer calling fromB_str to get a C string pointer
        if ptr.startswith("0x"):
            expr = f"(char*)fromB_str(({ty}){ptr})"
            sev = valobj.EvaluateExpression(expr, _expr_opts())
            s = _read_cstring(valobj, sev)
            if s is not None:
                s = s.replace('\\', r'\\').replace('"', r'\"')
                return f'"{s}"'
        # Fallback: read fields directly
        base = valobj.Dereference() if valobj.TypeIsPointerType() else valobj
        if not base or not base.IsValid():
            return None
        nb = base.GetChildMemberWithName("nbytes")
        sp = base.GetChildMemberWithName("str")
        if nb and nb.IsValid() and sp and sp.IsValid():
            nbytes = nb.GetValueAsUnsigned()
            addr = sp.GetValueAsUnsigned()
            if nbytes and addr:
                process = valobj.GetTarget().GetProcess()
                err = lldb.SBError()
                data = process.ReadMemory(addr, int(nbytes), err)
                if (err.Success() or not err.Fail()) and data is not None:
                    try:
                        s = data.decode('utf-8', errors='replace')
                        s = s.replace('\\', r'\\').replace('"', r'\"')
                        return f'"{s}"'
                    except Exception:
                        return None
        return None
    except Exception:
        return None


def _summary_via___str__(valobj: lldb.SBValue) -> Optional[str]:
    try:
        ptr = valobj.GetValue() or ""
        if not ptr.startswith("0x"):
            return None
        # Dynamically dispatch via vtable: ((B_value)ptr)->$class->__str__((B_value)ptr)
        expr = f"(char*)fromB_str(((B_value){ptr})->$class->__str__(((B_value){ptr})))"
        sev = valobj.EvaluateExpression(expr, _expr_opts())
        return _read_cstring(valobj, sev)
    except Exception:
        return None


def _summary_via_gcinfo(valobj: lldb.SBValue) -> Optional[str]:
    try:
        base = valobj.Dereference() if valobj.TypeIsPointerType() else valobj
        if not base or not base.IsValid():
            return None
        cls = base.GetChildMemberWithName("$class")
        if cls and cls.IsValid():
            cls_d = cls.Dereference() if cls.TypeIsPointerType() else cls
            gc = cls_d.GetChildMemberWithName("$GCINFO")
            if gc and gc.IsValid():
                gaddr = gc.GetValueAsUnsigned()
                if gaddr:
                    process = valobj.GetTarget().GetProcess()
                    err = lldb.SBError()
                    gstr = process.ReadCStringFromMemory(gaddr, 256, err)
                    if (err.Success() or not err.Fail()) and gstr:
                        label = maybe_demangle(gstr)
                        ptr = valobj.GetValue() or "?"
                        return f"<{label} object at {ptr}>"
        return None
    except Exception:
        return None


def sum_B_value(valobj: lldb.SBValue, _internal_dict) -> Optional[str]:
    """Generic summary for Acton boxed values: use __str__ → fromB_str."""
    # Strings handled by sum_B_str; here we handle everything else
    try:
        ty = (valobj.GetTypeName() or "").strip()
        if "B_str" in ty:
            return None
        # Fast paths for common builtins via their specific __str__
        ptr = valobj.GetValue() or ""
        if ptr.startswith("0x"):
            if "B_int" in ty:
                # Try to read small integers directly from limbs for speed/robustness
                try:
                    base = valobj.Dereference() if valobj.TypeIsPointerType() else valobj
                    vfield = base.GetChildMemberWithName("val")
                    if vfield and vfield.IsValid():
                        sz = vfield.GetChildMemberWithName("size")
                        nfield = vfield.GetChildMemberWithName("n")
                        if sz and sz.IsValid() and nfield and nfield.IsValid():
                            size = sz.GetValueAsSigned()
                            naddr = nfield.GetValueAsUnsigned()
                            if size == 0:
                                return "0"
                            if abs(size) == 1 and naddr:
                                process = valobj.GetTarget().GetProcess()
                                limb_err = lldb.SBError()
                                limb_bytes = process.ReadMemory(naddr, 8, limb_err)
                                if (limb_err.Success() or not limb_err.Fail()) and limb_bytes is not None and len(limb_bytes) == 8:
                                    limb = int.from_bytes(limb_bytes, byteorder='little', signed=False)
                                    return f"{-limb if size < 0 else limb}"
                except Exception:
                    pass
                # Fallback via __str__ helper
                expr = f"(char*)fromB_str(B_intD___str__((B_int){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_i64" in ty:
                expr = f"(char*)fromB_str(B_i64D___str__((B_i64){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_i32" in ty:
                expr = f"(char*)fromB_str(B_i32D___str__((B_i32){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_i16" in ty:
                expr = f"(char*)fromB_str(B_i16D___str__((B_i16){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_u64" in ty:
                expr = f"(char*)fromB_str(B_u64D___str__((B_u64){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_u32" in ty:
                expr = f"(char*)fromB_str(B_u32D___str__((B_u32){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_u16" in ty:
                expr = f"(char*)fromB_str(B_u16D___str__((B_u16){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_float" in ty:
                expr = f"(char*)fromB_str(B_floatD___str__((B_float){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
            elif "B_bool" in ty:
                # fromB_bool -> C int, but we keep unified path via __str__ to get "True"/"False"
                expr = f"(char*)fromB_str(B_boolD___str__((B_bool){ptr}))"
                sev = valobj.EvaluateExpression(expr, _expr_opts())
                s = _read_cstring(valobj, sev)
                if s is not None:
                    return s
        # Generic: try dynamic __str__ (may fail with unusual toolchains)
        s = _summary_via___str__(valobj)
        if s is not None:
            return s
        return _summary_via_gcinfo(valobj)
    except Exception:
        return None


def sum_user_value(valobj: lldb.SBValue, _internal_dict) -> Optional[str]:
    """Summary for user objects (e.g., stackdemoQ_main): use __str__ fallback."""
    try:
        s = _summary_via___str__(valobj)
        if s is not None:
            return s
        return _summary_via_gcinfo(valobj)
    except Exception:
        return None


def _register_formatters() -> None:
    """Register LLDB Python type summaries so `frame variable` and DAP show Acton values."""
    if os.getenv("ACTON_LLDB_NO_FORMATTERS"):
        return
    dbg = lldb.debugger
    # Define category (no enable yet to avoid 'empty category enabled' warning)
    dbg.HandleCommand("type category define Acton")
    # Add summaries into the Acton category
    dbg.HandleCommand(f"type summary add -w Acton -F {__name__}.sum_B_str B_str")
    dbg.HandleCommand(f"type summary add -w Acton -x -F {__name__}.sum_B_value '^B_.*$'")
    dbg.HandleCommand(f"type summary add -w Acton -x -F {__name__}.sum_user_value '^[A-Za-z_][A-Za-z0-9_]*Q_.*$'")
    # Many Acton locals show as $WORD/WORD (generic value). Provide a generic summary for those.
    dbg.HandleCommand(f"type summary add -w Acton -F {__name__}.sum_WORD '$WORD'")
    dbg.HandleCommand(f"type summary add -w Acton -F {__name__}.sum_WORD 'WORD'")
    dbg.HandleCommand("type category enable Acton")
    # Also install essential summaries in the default category so DAP shows them even if Acton is disabled
    dbg.HandleCommand(f"type summary add -F {__name__}.sum_B_str B_str")
    dbg.HandleCommand(f"type summary add -x -F {__name__}.sum_B_value '^B_.*$'")
    dbg.HandleCommand(f"type summary add -F {__name__}.sum_WORD '$WORD'")
    dbg.HandleCommand(f"type summary add -F {__name__}.sum_WORD 'WORD'")


def sum_WORD(valobj: lldb.SBValue, _internal_dict) -> Optional[str]:
    """Summary for generic Acton value type ($WORD/WORD) — treat as B_value pointer."""
    # TODO: we should be able to do better by following $class pointer and figuring out actual type
    try:
        s = _summary_via___str__(valobj)
        if s:
            return s
        return _summary_via_gcinfo(valobj) or ""
    except Exception:
        return ""


# ------------------------------
# Small helpers
# ------------------------------

def cmd_root(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:
    """Acton command group.

    Usage:
      acton bt [options]
      acton locals [options]
      acton demangle [name]
    """
    args = shlex_split(command)
    if not args:
        result.PutCString(cmd_root.__doc__ or "")
        return
    sub = args[0]
    rest = " ".join(args[1:])
    if sub in ("bt",):
        return cmd_bt(debugger, rest, exe_ctx, result)
    if sub in ("locals",):
        return cmd_locals(debugger, rest, exe_ctx, result)
    if sub in ("demangle",):
        return cmd_demangle(debugger, rest, exe_ctx, result)
    if sub in ("bp", "break"):
        return cmd_break(debugger, rest, exe_ctx, result)
    result.PutCString(f"Unknown subcommand '{sub}'. Try: bt, locals, demangle")

def shlex_split(s: str) -> List[str]:
    """A minimal shlex split that tolerates LLDB inputs without importing shlex eagerly."""
    try:
        import shlex

        return shlex.split(s)
    except Exception:
        return s.split()

# ------------------------------
# Color helpers
# ------------------------------

def _c(code: str, s: str) -> str:
    if not CFG.enable_color:
        return s
    return f"\x1b[{code}m{s}\x1b[0m"

def _fmt_file_line(filename: str, line: int) -> str:
    # Match LLDB bt-ish style: file in cyan/teal, line in yellow
    return f"{_c('36', filename)}:{_c('33', str(line))}"

def _format_args(frame: lldb.SBFrame) -> str:
    try:
        args = frame.GetVariables(True,  # arguments
                                  False, # locals
                                  False, # statics
                                  True)  # in_scope_only
    except Exception:
        return "()"

    items: List[str] = []
    for v in args:
        try:
            name = v.GetName() or "?"
            if name == "C_cont":
                continue
            dname = demangle_acton(name)
            if CFG.show_arg_values:
                pretty = _acton_value_summary(v, lldb.SBExecutionContext(frame))
                if pretty is not None:
                    val = pretty
                else:
                    val = v.GetSummary() or v.GetValue()
                if val is None:
                    items.append(dname)
                else:
                    sval = str(val).strip()
                    # keep brief; avoid long blobs/newlines
                    if "\n" in sval:
                        sval = sval.split("\n", 1)[0] + " …"
                    if len(sval) > 120:
                        sval = sval[:117] + "…"
                    items.append(f"{dname}={sval}")
            else:
                items.append(dname)
        except Exception:
            continue
    return "(" + ", ".join(items) + ")"

def _acton_value_summary(val: lldb.SBValue, exe_ctx: lldb.SBExecutionContext) -> Optional[str]:
    try:
        raw_name = val.GetName() or ""
        # Unboxed values start with 'U' — skip boxed pretty-print
        if raw_name.startswith("U"):
            return None
        # Need a pointer value to the boxed object
        ptr = val.GetValue() or ""
        if not ptr.startswith("0x"):
            return None
        frame = exe_ctx.GetFrame() if hasattr(exe_ctx, "GetFrame") else None
        if not frame or not frame.IsValid():
            return None
        tyname = val.GetTypeName() or ""
        process = frame.GetThread().GetProcess()

        # Generic __str__ path early for non-strings: this covers actors/classes/self and
        # most builtins (ints, floats, bools) uniformly. We keep a dedicated fast path
        # for B_str below so strings still get quoted.
        if "B_str" not in tyname:
            try:
                opts0 = lldb.SBExpressionOptions()
                opts0.SetIgnoreBreakpoints(True)
                opts0.SetUnwindOnError(True)
                opts0.SetLanguage(lldb.eLanguageTypeC)
                sev0 = frame.EvaluateExpression(f"(B_str)B_valueD___str__((B_value){ptr})", opts0)
                if sev0 and sev0.IsValid() and (not sev0.GetError() or not sev0.GetError().Fail()):
                    s_base0 = sev0.Dereference() if sev0.TypeIsPointerType() else sev0
                    nb0 = s_base0.GetChildMemberWithName("nbytes")
                    sp0 = s_base0.GetChildMemberWithName("str")
                    if nb0 and nb0.IsValid() and sp0 and sp0.IsValid():
                        nbytes0 = nb0.GetValueAsUnsigned()
                        addr0 = sp0.GetValueAsUnsigned()
                        if nbytes0 and addr0:
                            err0 = lldb.SBError()
                            data0 = process.ReadMemory(addr0, int(nbytes0), err0)
                            if (err0.Success() or not err0.Fail()) and data0 is not None:
                                try:
                                    return data0.decode('utf-8', errors='replace')
                                except Exception:
                                    pass
            except Exception:
                pass
        # Integers (boxed) — extract underlying value via from$*/fromB_*
        int_expr = None
        if "B_int" in tyname:
            # Get B_str via B_int.__str__ and read bytes
            opts = lldb.SBExpressionOptions()
            opts.SetIgnoreBreakpoints(True)
            opts.SetUnwindOnError(True)
            opts.SetLanguage(lldb.eLanguageTypeC)
            sev = frame.EvaluateExpression(f"(B_str)B_intD___str__((B_int){ptr})", opts)
            if sev and sev.IsValid() and (not sev.GetError() or not sev.GetError().Fail()):
                s_base = sev.Dereference() if sev.TypeIsPointerType() else sev
                nb = s_base.GetChildMemberWithName("nbytes")
                sp = s_base.GetChildMemberWithName("str")
                if nb and nb.IsValid() and sp and sp.IsValid():
                    nbytes = nb.GetValueAsUnsigned()
                    addr = sp.GetValueAsUnsigned()
                    if nbytes and addr:
                        errn = lldb.SBError()
                        data = process.ReadMemory(addr, int(nbytes), errn)
                        if (errn.Success() or not errn.Fail()) and data is not None:
                            try:
                                return data.decode('utf-8', errors='replace')
                            except Exception:
                                pass
            # Field-based quick path for small ints
            base = val.Dereference() if val.TypeIsPointerType() else val
            if base and base.IsValid():
                vfield = base.GetChildMemberWithName("val")
                if vfield and vfield.IsValid():
                    sz = vfield.GetChildMemberWithName("size")
                    nfield = vfield.GetChildMemberWithName("n")
                    if sz and sz.IsValid() and nfield and nfield.IsValid():
                        size = sz.GetValueAsSigned()
                        naddr = nfield.GetValueAsUnsigned()
                        if size == 0:
                            return "0"
                        if abs(size) == 1 and naddr:
                            limb_err = lldb.SBError()
                            limb_bytes = process.ReadMemory(naddr, 8, limb_err)
                            if (limb_err.Success() or not limb_err.Fail()) and limb_bytes is not None and len(limb_bytes) == 8:
                                limb = int.from_bytes(limb_bytes, byteorder='little', signed=False)
                                if size < 0:
                                    return f"-{limb}"
                                else:
                                    return str(limb)
            # Fallback: try to get numeric value if available (may fail due to '$' in name)
            int_expr = f"from$int((B_int){ptr})"
        elif "B_i64" in tyname:
            int_expr = f"fromB_i64((B_i64){ptr})"
        elif "B_i32" in tyname:
            int_expr = f"fromB_i32((B_i32){ptr})"
        elif "B_i16" in tyname:
            int_expr = f"fromB_i16((B_i16){ptr})"
        elif "B_u64" in tyname:
            int_expr = f"fromB_u64((B_u64){ptr})"
        elif "B_u32" in tyname:
            int_expr = f"fromB_u32((B_u32){ptr})"
        elif "B_u16" in tyname:
            int_expr = f"fromB_u16((B_u16){ptr})"
        if int_expr is not None:
            opts = lldb.SBExpressionOptions()
            opts.SetIgnoreBreakpoints(True)
            opts.SetUnwindOnError(True)
            opts.SetLanguage(lldb.eLanguageTypeC)
            iev = frame.EvaluateExpression(int_expr, opts)
            if iev and iev.IsValid() and (not iev.GetError() or not iev.GetError().Fail()):
                ival = iev.GetValue()
                if ival is not None:
                    return str(ival)
            # if failed, fall through

        # Booleans
        if "B_bool" in tyname:
            opts = lldb.SBExpressionOptions()
            opts.SetIgnoreBreakpoints(True)
            opts.SetUnwindOnError(True)
            opts.SetLanguage(lldb.eLanguageTypeC)
            bev = frame.EvaluateExpression(f"fromB_bool((B_bool){ptr})", opts)
            if bev and bev.IsValid() and (not bev.GetError() or not bev.GetError().Fail()):
                bval = bev.GetValueAsUnsigned()
                return "True" if bval else "False"

        # Floats
        if "B_float" in tyname:
            opts = lldb.SBExpressionOptions()
            opts.SetIgnoreBreakpoints(True)
            opts.SetUnwindOnError(True)
            opts.SetLanguage(lldb.eLanguageTypeC)
            fev = frame.EvaluateExpression(f"fromB_float((B_float){ptr})", opts)
            if fev and fev.IsValid() and (not fev.GetError() or not fev.GetError().Fail()):
                fval = fev.GetValue()
                if fval is not None:
                    return str(fval)

        # Special-case B_str: read bytes directly to avoid null-termination issues
        if "B_str" in tyname:
            base = val.Dereference() if val.TypeIsPointerType() else val
            if not base or not base.IsValid():
                return None
            nb = base.GetChildMemberWithName("nbytes")
            sp = base.GetChildMemberWithName("str")
            if nb and nb.IsValid() and sp and sp.IsValid():
                nbytes = nb.GetValueAsUnsigned()
                addr = sp.GetValueAsUnsigned()
                if nbytes and addr:
                    err = lldb.SBError()
                    data = process.ReadMemory(addr, int(nbytes), err)
                    if err.Success() and data is not None:
                        try:
                            s = data.decode('utf-8', errors='replace')
                            # Quote string values for clarity
                            s = s.replace('\\', r'\\').replace('"', r'\"')
                            return f'"{s}"'
                        except Exception:
                            return None
            # Fall through to generic path if direct read fails
        # Generic path: get char* from __str__ quickly and read C-string
        cexpr = f"(char*)fromB_str(B_valueD___str__((B_value){ptr}))"
        opts = lldb.SBExpressionOptions()
        opts.SetIgnoreBreakpoints(True)
        opts.SetUnwindOnError(True)
        # C works, and the target is compiled as C; allow summary providers
        opts.SetLanguage(lldb.eLanguageTypeC)
        cev = frame.EvaluateExpression(cexpr, opts)
        if cev and cev.IsValid():
            caddr = cev.GetValueAsUnsigned()
            if caddr:
                e3 = lldb.SBError()
                cstr = process.ReadCStringFromMemory(caddr, 512, e3)
                if (e3.Success() or not e3.Fail()) and cstr is not None:
                    return cstr

        # Last-resort fallback for objects (actors/classes): use $class->$GCINFO
        try:
            base = val.Dereference() if val.TypeIsPointerType() else val
            if base and base.IsValid():
                cls = base.GetChildMemberWithName("$class")
                if cls and cls.IsValid():
                    cls_d = cls.Dereference() if cls.TypeIsPointerType() else cls
                    gc = cls_d.GetChildMemberWithName("$GCINFO")
                    if gc and gc.IsValid():
                        gaddr = gc.GetValueAsUnsigned()
                        if gaddr:
                            gerr = lldb.SBError()
                            gstr = process.ReadCStringFromMemory(gaddr, 256, gerr)
                            if (gerr.Success() or not gerr.Fail()) and gstr:
                                label = maybe_demangle(gstr)
                                return f"<{label} object at {ptr}>"
        except Exception:
            pass
        return None
    except Exception:
        return None


def cmd_break(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:
    """acton.break: Set a breakpoint at Acton file:line

    Usage:
      acton break test.act:37
      acton break test.act 37

    Notes:
    - Requires DWARF with Acton file paths (acton --debug recommended)
    - If it resolves to 0 locations, you may need a source-map.
    """
    s = command.strip()
    if not s:
        result.PutCString(cmd_break.__doc__ or "")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    file = None
    line = None
    if ":" in s and not s.startswith(":"):
        file, _, rest = s.partition(":")
        try:
            line = int(rest.strip())
        except Exception:
            pass
    else:
        parts = shlex_split(s)
        if len(parts) == 2:
            file, line_s = parts
            try:
                line = int(line_s)
            except Exception:
                line = None

    if not file or not line:
        result.PutCString("Usage: acton break <file.act>:<line> or acton break <file.act> <line>")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    target = debugger.GetSelectedTarget()
    if not target or not target.IsValid():
        result.PutCString("No target. Create or run a target first.")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    # Try to set breakpoint by file:line
    fs = lldb.SBFileSpec()
    fs.SetFilename(os.path.basename(file))
    # If a directory was included, set it as well
    dname = os.path.dirname(file)
    if dname:
        fs.SetDirectory(dname)
    bp = target.BreakpointCreateByLocation(fs, int(line))

    if bp and bp.IsValid() and bp.GetNumLocations() > 0:
        result.PutCString(f"Breakpoint {bp.GetID()}: {bp.GetNumLocations()} locations at {file}:{line}")
        return

    # Fallback: try matching compile units with same basename to hint source-map
    hinted = False
    from_dirs = set()
    for mod in target.module_iter():
        for i in range(mod.GetNumCompileUnits()):
            cu = mod.GetCompileUnitAtIndex(i)
            cu_fs = cu.GetFileSpec()
            if cu_fs and cu_fs.IsValid() and cu_fs.GetFilename() == os.path.basename(file):
                from_dirs.add(cu_fs.GetDirectory())
                hinted = True
    if hinted and from_dirs:
        for src_dir in sorted(from_dirs):
            result.PutCString(
                f"Hint: DWARF references '{os.path.basename(file)}' under '{src_dir}'. If your sources live elsewhere, use:\n"
                f"  settings set target.source-map {src_dir} <your-project-root>"
            )

    result.PutCString(f"No locations resolved for {file}:{line}. Ensure the binary has debug info (acton --debug) and source-map is correct.")
    result.SetStatus(lldb.eReturnStatusFailed)
