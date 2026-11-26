"""
Acton LLDB plugin

Features:
- acton.bt: filtered backtrace with basic Acton demangling
- acton.locals: print locals with Acton demangled names
- acton.demangle: demangle a single symbol or the selected frame name
- acton refs: show what keeps GC objects alive
- acton jump0: show the GC objects that the workers' jump0 frames point to
- acton outgoing: show the mailbox head and $outgoing messages of actors

Load in LLDB:
  (lldb) command script import /path/to/repo/utils/lldb/acton.py
  (lldb) acton.bt
"""

from __future__ import annotations

import lldb  # type: ignore
import argparse
import os
import re
import struct
from collections import Counter
from typing import List, Optional, Tuple


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
_re_d_magic = re.compile(r"D___")  # e.g. B_ActorD___init__
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
    _add_cmd(debugger, "acton-refs", cmd_refs, "Show what keeps GC objects alive")
    _add_cmd(debugger, "acton-jump0", cmd_jump0, "Show the GC objects that jump0 points to")
    _add_cmd(debugger, "acton-outgoing", cmd_outgoing, "Show the $outgoing messages of actors")
    # Group command with subcommands: `acton bt|locals|demangle|refs|jump0|outgoing`
    _add_cmd(debugger, "acton", cmd_root, "Acton command group: bt | locals | demangle | refs | jump0 | outgoing")
    # Breakpoint helper
    _add_cmd(debugger, "acton.break", cmd_break, "Set breakpoint at Acton file:line")
    _add_cmd(debugger, "acton-bp", cmd_break, "Set breakpoint at Acton file:line")

    # Environment-driven config (optional)
    _apply_env_overrides()

    print("Acton LLDB plugin loaded. Commands: acton bt|locals|demangle|refs|jump0|outgoing; aliases: acton-bt, acton-locals, acton-demangle, acton-refs, acton-jump0, acton-outgoing, acton.bt, acton.locals, acton.demangle")
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
      acton refs [options] ADDRESS...
      acton jump0
      acton outgoing ACTOR...
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
    if sub in ("refs",):
        return cmd_refs(debugger, rest, exe_ctx, result)
    if sub in ("jump0",):
        return cmd_jump0(debugger, rest, exe_ctx, result)
    if sub in ("outgoing",):
        return cmd_outgoing(debugger, rest, exe_ctx, result)
    result.PutCString(f"Unknown subcommand '{sub}'. Try: bt, locals, demangle, refs, jump0, outgoing")

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


# ------------------------------
# GC: what keeps an object alive
# ------------------------------
#
# These commands read a stopped process. They take struct layouts and the
# collector's tables from the debug info, and they ask the collector about
# heap objects by calling its functions on the selected thread while every
# other thread stays stopped.

# Acton runs only on 64-bit little-endian machines
WORD = 8

# The search reads memory in chunks of this many bytes
CHUNK = 1 << 24

# Object kinds of the collector that need a name: GC_I_PTRFREE (0) from
# gc_inline.h, and UNCOLLECTABLE (2) and AUNCOLLECTABLE (3) from gc_priv.h.
# The collector does not scan the words of pointer-free objects. It never
# collects uncollectable objects and scans those that hold pointers at every
# collection, which makes them roots.
GC_KIND_NAMES = {0: "pointer-free", 2: "uncollectable", 3: "uncollectable, pointer-free"}
GC_POINTER_FREE = (0, 3)
GC_UNCOLLECTABLE = 2


def _is_stopped(target: lldb.SBTarget, result: lldb.SBCommandReturnObject) -> bool:
    process = target.GetProcess()
    if process.IsValid() and process.GetState() == lldb.eStateStopped:
        return True
    result.PutCString("No stopped process. Run or attach first, and stop it.")
    result.SetStatus(lldb.eReturnStatusFailed)
    return False


def _stopped_opts() -> lldb.SBExpressionOptions:
    # Run only the selected thread, so that the memory the commands read does
    # not change under them
    opts = _expr_opts()
    opts.SetStopOthers(True)
    opts.SetTryAllThreads(False)
    return opts


def _call(target: lldb.SBTarget, expr: str) -> int:
    """Evaluate expr, a call of a collector function, in the stopped process"""
    v = target.EvaluateExpression(f"(unsigned long)({expr})", _stopped_opts())
    if v.GetError().Fail():
        raise RuntimeError(f"{expr}: {v.GetError().GetCString()}")
    return v.GetValueAsUnsigned()


def _address(target: lldb.SBTarget, text: str) -> int:
    """The value of text, a number or an expression such as self"""
    try:
        return int(text, 0)
    except ValueError:
        pass
    frame = target.GetProcess().GetSelectedThread().GetSelectedFrame()
    v = frame.EvaluateExpression(text, _stopped_opts())
    if v.GetError().Fail():
        raise RuntimeError(f"{text}: {v.GetError().GetCString()}")
    return v.GetValueAsUnsigned()


def _class_of(target: lldb.SBTarget, obj: int) -> str:
    """The symbol that the first word of obj points to, which for an Acton
    object is its class, or ? when the word is not the start of a symbol"""
    err = lldb.SBError()
    cls = target.GetProcess().ReadPointerFromMemory(obj, err)
    sym = target.ResolveLoadAddress(cls).GetSymbol()
    if err.Success() and sym.IsValid() and sym.GetStartAddress().GetLoadAddress(target) == cls:
        return sym.GetName()
    return "?"


def _thread_name(t: lldb.SBThread) -> str:
    name = f"thread #{t.GetIndexID()}"
    return f"{name} ({t.GetName()})" if t.GetName() else name


def _field_at(t: lldb.SBType, offset: int) -> str:
    """The name of the field of struct type t at offset, with the offset into
    the field when it is not 0"""
    for f in t.GetCanonicalType().fields:
        start = f.GetOffsetInBytes()
        if start <= offset < start + f.GetType().GetByteSize():
            return f.GetName() if offset == start else f"{f.GetName()}+{offset - start}"
    return f"+{offset}"


def _local_at(frame: lldb.SBFrame, addr: int) -> Tuple[str, str]:
    """The name of the variable of frame that holds addr and where in it addr
    is (an array index or an offset), or empty strings"""
    for v in frame.GetVariables(True, True, False, False):
        start = v.GetLoadAddress()
        if start == lldb.LLDB_INVALID_ADDRESS or not start <= addr < start + v.GetByteSize():
            continue
        elem = v.GetType().GetArrayElementType().GetByteSize() if v.GetType().IsArrayType() else 0
        if elem:
            return v.GetName(), f"[{(addr - start) // elem}]"
        return v.GetName(), f"+{addr - start}" if addr > start else ""
    return "", ""


class _Memory:
    """A copy of the memory that the collector scans for pointers in the
    stopped process (its heap, its static roots and the thread stacks), and
    what each word in it belongs to"""

    def __init__(self, target: lldb.SBTarget) -> None:
        self.target = target
        self.process = target.GetProcess()
        self.chunks: List[Tuple[int, bytes]] = []
        # The collector's own tables of heap sections, static roots and
        # ranges excluded from the static roots (struct _GC_arrays in
        # bdwgc's gc_priv.h)
        gc = target.FindFirstGlobalVariable("GC_arrays")
        self.hdr_type = target.FindFirstType("struct hblkhdr")
        if not gc.IsValid() or not self.hdr_type.IsValid():
            raise RuntimeError("No debug info for the collector: is this a debug build of an Acton program?")

        def table(name: str, count: str) -> List[lldb.SBValue]:
            entries = gc.GetChildMemberWithName(name)
            n = gc.GetChildMemberWithName(count).GetValueAsUnsigned()
            return [entries.GetValueForExpressionPath(f"[{i}]") for i in range(n)]

        def field(v: lldb.SBValue, name: str) -> int:
            return v.GetChildMemberWithName(name).GetValueAsUnsigned()

        self.heap = [(field(s, "hs_start"), field(s, "hs_start") + field(s, "hs_bytes"))
                     for s in table("_heap_sects", "_n_heap_sects")]
        self.statics = [(field(r, "r_start"), field(r, "r_end"))
                        for r in table("_static_roots", "_n_root_sets")]
        excluded = sorted((field(e, "e_start"), field(e, "e_end"))
                          for e in table("_excl_table", "_excl_table_entries"))
        heap = sum(self._read(start, end) for start, end in self.heap)
        static = 0
        for start, end in self.statics:
            for e_start, e_end in excluded:
                if e_start < end and start < e_end:
                    static += self._read(start, e_start)
                    start = max(start, e_end)
            static += self._read(start, end)
        # The collector scans the registers of each thread, and its stack
        # from the stack pointer up
        self.stacks: List[Tuple[lldb.SBThread, int, int]] = []
        self.registers: List[Tuple[lldb.SBThread, str, int]] = []
        stack = 0
        for t in self.process:
            frame = t.GetFrameAtIndex(0)
            info = lldb.SBMemoryRegionInfo()
            if self.process.GetMemoryRegionInfo(frame.GetSP(), info).Success():
                self.stacks.append((t, frame.GetSP(), info.GetRegionEnd()))
                stack += self._read(frame.GetSP(), info.GetRegionEnd())
            gprs = frame.GetRegisters().GetValueAtIndex(0)
            for j in range(gprs.GetNumChildren()):
                r = gprs.GetChildAtIndex(j)
                # Skip the narrower views of registers, such as w0 of x0
                if r.GetByteSize() == WORD:
                    self.registers.append((t, r.GetName(), r.GetValueAsUnsigned()))
        self.sizes = (heap, static, stack)
        self.found: dict = {}       # (start, end) -> address and value of the words that point there
        self.bases: dict = {}       # word address -> start of its GC object, or 0
        self.objects: dict = {}     # start of a GC object -> (size, kind, marked, class)

    def _read(self, start: int, end: int) -> int:
        """Copy the readable memory between start and end, and return its size"""
        size = 0
        start -= start % WORD
        while start < end:
            info = lldb.SBMemoryRegionInfo()
            if self.process.GetMemoryRegionInfo(start, info).Fail() or info.GetRegionEnd() <= start:
                break
            stop = min(end, info.GetRegionEnd())
            if info.IsReadable():
                for a in range(start, stop, CHUNK):
                    err = lldb.SBError()
                    data = self.process.ReadMemory(a, min(CHUNK, stop - a), err)
                    if err.Success() and data:
                        self.chunks.append((a, data))
                        size += len(data)
            start = stop
        return size

    def header(self) -> str:
        heap, static, stack = (f"{n >> 20} MiB" if n >> 20 else f"{n >> 10} KiB" for n in self.sizes)
        return (f"searched {heap} of heap, {static} of static roots, "
                f"and the stacks ({stack}) and registers of {len(self.stacks)} threads")

    def find(self, start: int, end: int) -> List[Tuple[int, int]]:
        """The address and value of each aligned word whose value is at
        least start and below end"""
        if (start, end) not in self.found:
            hits = []
            # Search for the upper six bytes of each 64 KiB window of
            # addresses that the range overlaps, and check the whole word
            for window in range(start >> 16, ((end - 1) >> 16) + 1):
                pattern = struct.pack("<Q", window << 16)[2:]
                for chunk, data in self.chunks:
                    i = data.find(pattern, 2)
                    while i != -1:
                        if i % WORD == 2:
                            value = int.from_bytes(data[i - 2:i + 6], "little")
                            if start <= value < end:
                                hits.append((chunk + i - 2, value))
                        i = data.find(pattern, i + 1)
            self.found[(start, end)] = sorted(hits)
        return self.found[(start, end)]

    def stack_word(self, addr: int) -> Optional[Tuple[str, str]]:
        """Where addr is on a thread's stack and the root that the word is,
        or None when addr is on no stack"""
        for t, sp, end in self.stacks:
            if not sp <= addr < end:
                continue
            thread = _thread_name(t)
            for f in t:
                if f.GetSP() <= addr < f.GetCFA():
                    func = f.GetFunctionName() or "?"
                    name, at = _local_at(f, addr)
                    text = f"stack of {thread}, frame #{f.GetFrameID()} {func}"
                    root = f"stack of {thread}, {func}"
                    if name:
                        text += f", {name}{at}"
                        root += f", {name}"
                    return text, root
            return f"stack of {thread}, sp+{addr - sp}", f"stack of {thread}"
        return None

    def static_word(self, addr: int) -> Optional[Tuple[str, str]]:
        """Where addr is in the static roots and the root that the word is,
        or None when addr is in no static root"""
        for start, end in self.statics:
            if start <= addr < end:
                break
        else:
            return None
        a = self.target.ResolveLoadAddress(addr)
        sym = a.GetSymbol()
        if a.GetModule().IsValid() and sym.IsValid():
            root = f"static {a.GetModule().GetFileSpec().GetFilename()}`{sym.GetName()}"
            return f"{root}+{addr - sym.GetStartAddress().GetLoadAddress(self.target)}", root
        root = f"static root 0x{start:x}-0x{end:x}"
        return f"{root}+{addr - start}", root

    def object_of(self, addr: int) -> Optional[Tuple[int, int, int, bool, str]]:
        """The start, size, kind, mark and class of the GC object that holds
        addr, or None when addr is in no GC object"""
        if addr not in self.bases:
            self.bases[addr] = _call(self.target, f"((void *(*)(void *))GC_base)((void *)0x{addr:x})")
        base = self.bases[addr]
        if not base:
            return None
        if base not in self.objects:
            # The header of the object's block (struct hblkhdr in bdwgc's
            # gc_priv.h) holds the size and kind of its objects
            hdr = _call(self.target, f"((void *(*)(void *))GC_find_header)((void *)0x{base:x})")
            hdr = self.target.CreateValueFromAddress("hdr", lldb.SBAddress(hdr, self.target), self.hdr_type)
            self.objects[base] = (
                hdr.GetChildMemberWithName("hb_sz").GetValueAsUnsigned(),
                hdr.GetChildMemberWithName("hb_obj_kind").GetValueAsUnsigned(),
                _call(self.target, f"((int (*)(void *))GC_is_marked)((void *)0x{base:x})") != 0,
                _class_of(self.target, base))
        return (base,) + self.objects[base]


def _describe_object(obj: Tuple[int, int, int, bool, str]) -> str:
    _, size, kind, marked, cls = obj
    text = f"{cls}, {size} bytes, {'marked' if marked else 'unmarked'}"
    if kind in GC_KIND_NAMES:
        text += f", {GC_KIND_NAMES[kind]}"
    elif kind != 1:
        text += f", kind {kind}"
    return text


def _referrers(mem: _Memory, start: int, end: int, depth: int, max_depth: int, path: List[str],
               seen: set, lines: List[str], roots: List[Tuple[List[str], str]]) -> None:
    """Append to lines the roots and the GC objects that hold words that
    point between start and end, and below each marked GC object among them,
    what points into that object, up to max_depth objects deep. Append to
    roots each root that the walk reaches, with the path of objects to it."""
    indent = "  " * depth
    held: dict = {}      # description of a root -> how many words in it point here
    holders: dict = {}   # GC object (None for free heap memory) -> its words that point here
    for t, name, v in mem.registers:
        if start <= v < end:
            root = f"register {name} of {_thread_name(t)}"
            held[root] = held.get(root, 0) + 1
            roots.append((path, root))
    for w, v in mem.find(start, end):
        if start <= w < end:
            continue    # the object points into itself
        where = mem.stack_word(w) or mem.static_word(w)
        if where:
            held[where[0]] = held.get(where[0], 0) + 1
            roots.append((path, where[1]))
        else:
            # The rest of the memory is heap
            holders.setdefault(mem.object_of(w), []).append(w)
    if not held and not holders:
        lines.append(f"{indent}(nothing points to it)")
    for text, n in held.items():
        lines.append(f"{indent}<- {text}" + (f" ({n} words)" if n > 1 else ""))
    for obj, words in holders.items():
        count = f" ({len(words)} words)" if len(words) > 1 else ""
        if obj is None:
            lines.append(f"{indent}<- free heap memory{count}")
            continue
        base, size, kind, marked, cls = obj
        lines.append(f"{indent}<- 0x{base:x}+{words[0] - base}{count}: {_describe_object(obj)}")
        # The collector does not scan pointer-free objects, and unmarked ones
        # were garbage at the last collection
        if kind in GC_POINTER_FREE or not marked:
            continue
        name = f"{cls}({size})"
        if kind == GC_UNCOLLECTABLE:
            roots.extend([(path, f"uncollectable {name}")] * len(words))
            continue
        if base in seen:
            lines.append(f"{indent}   (seen above)")
            continue
        seen.add(base)
        if depth >= max_depth:
            lines.append(f"{indent}   (depth limit)")
            roots.append((path + [name], "depth limit"))
            continue
        _referrers(mem, base, base + size, depth + 1, max_depth, path + [name], seen, lines, roots)


def cmd_refs(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:
    """acton refs: Show what keeps GC objects alive in a stopped program.

    Usage: acton refs [-s|--summary] [-d|--depth N] ADDRESS...

    Searches the memory that the collector scans for pointers (its heap, its
    static roots and each thread's stack from the stack pointer up) and the
    registers of the threads for words that point into the GC object at
    ADDRESS (a number or an expression, such as self). A word counts when it
    points anywhere into the object, since the collector recognizes interior
    pointers. Each word is shown with what it is in: a GC object (with its
    start, class, size, mark bit and kind), a stack frame or register of a
    thread, or a static root. For each marked GC object among them, the
    search repeats, up to N objects deep (default 10), so that each branch
    ends at a root.

    -s/--summary: instead of the tree, one line per root, which counts the
                  words of the root that lead to ADDRESS and lists the
                  objects in between

    The mark bits are those of the last collection.
    """
    try:
        parser = argparse.ArgumentParser(prog="acton refs", add_help=False)
        parser.add_argument("-s", "--summary", action="store_true")
        parser.add_argument("-d", "--depth", type=int, default=10)
        parser.add_argument("addresses", nargs="+")
        opts = parser.parse_args(shlex_split(command))
    except SystemExit:
        result.PutCString(cmd_refs.__doc__ or "")
        result.SetStatus(lldb.eReturnStatusFailed)
        return
    target = debugger.GetSelectedTarget()
    if not _is_stopped(target, result):
        return
    out: List[str] = []
    try:
        addrs = [_address(target, a) for a in opts.addresses]
        mem = _Memory(target)
        out.append(mem.header())
        for addr in addrs:
            obj = mem.object_of(addr)
            if obj is None:
                out.append(f"0x{addr:x}: not in a GC object")
                start, end = addr, addr + 1
            else:
                start, end = obj[0], obj[0] + obj[1]
                inside = f" (0x{addr:x} is at +{addr - start} in it)" if addr != start else ""
                out.append(f"0x{start:x}{inside}: {_describe_object(obj)}")
            lines: List[str] = []
            roots: List[Tuple[List[str], str]] = []
            _referrers(mem, start, end, 1, opts.depth, [], {start}, lines, roots)
            if not opts.summary:
                out.extend(lines)
                continue
            counts = Counter(" <- ".join(path + [root]) for path, root in roots)
            if not counts:
                out.append("   (no path to a root)")
            for chain, n in sorted(counts.items(), key=lambda kv: (-kv[1], kv[0])):
                out.append(f"   {n}x {chain}")
    except RuntimeError as e:
        out.append(str(e))
        result.SetStatus(lldb.eReturnStatusFailed)
    result.PutCString("\n".join(out))


def cmd_jump0(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:
    """acton jump0: Show the GC objects that the workers' jump0 frames point to.

    Usage: acton jump0

    jump0 in struct WorkerCtx is the worker loop's exception frame. The first
    continuation in each call of wt_work_cb() pushes it with a setjmp, which
    saves the worker's registers in it, and the collector scans it while it
    is pushed. For every worker, prints jump_top (jump0 while the frame is
    pushed, NULL when no frame is, else the address of a try frame) and each
    word of jump0 that points into a GC object, with the object's start and
    class.
    """
    target = debugger.GetSelectedTarget()
    if not _is_stopped(target, result):
        return
    process = target.GetProcess()
    wctxs = target.FindFirstGlobalVariable("wctxs")
    if not wctxs.IsValid() or not wctxs.GetType().IsArrayType():
        result.PutCString("No debug info for wctxs: is this a debug build of an Acton program?")
        result.SetStatus(lldb.eReturnStatusFailed)
        return
    out = []
    try:
        for i in range(wctxs.GetNumChildren()):
            ctx = wctxs.GetChildAtIndex(i)
            if not ctx.GetValueAsUnsigned():
                continue
            ctx = ctx.Dereference()
            jump0 = ctx.GetChildMemberWithName("jump0")
            start = jump0.GetLoadAddress()
            top = ctx.GetChildMemberWithName("jump_top").GetValueAsUnsigned()
            state = "jump0" if top == start else "NULL" if top == 0 else f"0x{top:x}"
            err = lldb.SBError()
            data = process.ReadMemory(start, jump0.GetByteSize(), err)
            if not err.Success():
                out.append(f"worker {i}: cannot read jump0 at 0x{start:x}: {err.GetCString()}")
                continue
            words = struct.unpack(f"<{len(data) // WORD}Q", data)
            out.append(f"worker {i}: jump_top {state}, {sum(1 for w in words if w)} of {len(words)} words of jump0 are not 0")
            for j, w in enumerate(words):
                base = w and _call(target, f"((void *(*)(void *))GC_base)((void *)0x{w:x})")
                if base:
                    where = f"0x{w:x}" if w == base else f"0x{w:x} in 0x{base:x}+{w - base}"
                    out.append(f"    {_field_at(jump0.GetType(), j * WORD)}: {where}, {_class_of(target, base)}")
    except RuntimeError as e:
        out.append(str(e))
        result.SetStatus(lldb.eReturnStatusFailed)
    result.PutCString("\n".join(out))


def cmd_outgoing(debugger: lldb.SBDebugger, command: str, exe_ctx: lldb.SBExecutionContext, result: lldb.SBCommandReturnObject, _internal_dict=None) -> None:
    """acton outgoing: Show the mailbox head and the $outgoing messages of actors.

    Usage: acton outgoing ACTOR...

    $outgoing holds the messages that the actor's running continuation has
    sent, newest first, until the worker delivers them after the
    continuation. For each ACTOR (an address or an expression, such as
    self), prints the head of its mailbox (the field B_Msg) and each message
    in $outgoing with its recipient ($to) and the recipient's class.
    """
    actors = shlex_split(command)
    if not actors:
        result.PutCString(cmd_outgoing.__doc__ or "")
        result.SetStatus(lldb.eReturnStatusFailed)
        return
    target = debugger.GetSelectedTarget()
    if not _is_stopped(target, result):
        return
    actor_type = target.FindFirstType("struct B_Actor")
    msg_type = target.FindFirstType("struct B_Msg")
    if not actor_type.IsValid() or not msg_type.IsValid():
        result.PutCString("No debug info for struct B_Actor and struct B_Msg: is this a debug build of an Acton program?")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    def value_at(name: str, addr: int, t: lldb.SBType) -> lldb.SBValue:
        return target.CreateValueFromAddress(name, lldb.SBAddress(addr, target), t)

    out = []
    try:
        for text in actors:
            a = _address(target, text)
            actor = value_at("actor", a, actor_type)
            mailbox = actor.GetChildMemberWithName("B_Msg").GetValueAsUnsigned()
            m = actor.GetChildMemberWithName("$outgoing").GetValueAsUnsigned()
            out.append(f"0x{a:x} {_class_of(target, a)}: mailbox 0x{mailbox:x}, $outgoing 0x{m:x}")
            seen = set()
            while m and m not in seen:
                seen.add(m)
                msg = value_at("msg", m, msg_type)
                to = msg.GetChildMemberWithName("$to").GetValueAsUnsigned()
                out.append(f"    message 0x{m:x} to 0x{to:x} {_class_of(target, to)}")
                m = msg.GetChildMemberWithName("$next").GetValueAsUnsigned()
    except RuntimeError as e:
        out.append(str(e))
        result.SetStatus(lldb.eReturnStatusFailed)
    result.PutCString("\n".join(out))
