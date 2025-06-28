# New interpolated strings

After the string parser rewrite, Acton now supports interpolation in normal
strings plus many previous issues with f-strings have been fixed. The idiomatic
Acton style is now to use regular strings with interpolation.


## basic concatenation
replace
```acton
"Hello " + name + ", you have " + str(count) + " messages"
```
with
```acton
"Hello {name}, you have {count} messages"
```

## % operator style
replace:
```acton
"Hello %s" % name
```

with:
```acton
"Hello {name}"
```

## f-strings

Remove the f prefix since it is optional and the idiomatic Acton style is to NOT use f-prefix.

replace:
```acton
f"Hello {name}"
```

with:
```acton
"Hello {name}"
```

## Slice expressions

There were previously issues with slice expressions inside f-strings. Those have
been addressed with the new parser, so we should stick to string interplation,
not concatenation.

replace:
```acton
"Hello " + name[:3]
```

with:
```acton
"Hello {name[:3]}"
```


## Escaped quotes

There were previously issues with escaping quotes inside f-strings. Those issues
have been addressed with the new parser, so we can use different quote chars:

replace:
```acton
f"""Hello \"world\"!"""
```

with:
```acton
"""Hello "world"!"""
```

## Escape curly braces

Since normal strings now offer interpolation, curly braces have changed
semantically from literal braces to mean an expression that should be
interpolated. Thus, for existing code, we need to convert the string to a raw
string or escape the brace.

replace:
```acton
'Some JSON {"a": 1}'
```

with:
```acton
r'Some JSON {"a": 1}'
```

if interpolation or string concatenation is used, then escape

replace:
```acton
'Name' + name + 'Some JSON {"a": 1}'
```

with:
```acton
"""Name {name} Some JSON {{"a": 1}}"""
```
