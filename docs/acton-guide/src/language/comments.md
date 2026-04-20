# Comments

Use `#` for comments.

Comments are for readers of the code. Use them to explain intent,
assumptions, or surprising choices.

```python
def area(width, height):
    # Width and height are measured in meters.
    return width * height

actor main(env):
    # Keep the greeting short because it is printed in a narrow terminal.
    message = "Hello"
    print(message)
    env.exit(0)
```

<div class="beginner-content">
<p>A comment is ignored by the compiler. It is there only for people
reading the source code. There is no separate block-comment syntax here;
use <code>#</code> on each line you want to comment.</p>
</div>

## What to comment

Use comments for things the code does not make obvious:

- why a check exists
- units or external constraints
- a temporary workaround
- a non-obvious invariant

## Practical guidance

- Prefer comments that explain <em>why</em>, not comments that only
  repeat what the code already says.
- Keep comments close to the code they describe.
- Update or remove comments when the code changes.
