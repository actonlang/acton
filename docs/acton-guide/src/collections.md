# Collections and everyday data

Acton has three built-in collection types for everyday data:

- [Lists](collections/lists.md) keep values in order and allow
  duplicates.
- [Dictionaries](collections/dicts.md) map keys to values.
- [Sets](collections/sets.md) keep unique values and make membership
  checks direct.

<div class="beginner-content">
<p>If you are unsure where to start, choose by the question you want to
answer. Use a list when order matters, a dictionary when you need to
look things up by key, and a set when you mainly care about uniqueness
or membership.</p>
</div>

Use this section when you need to keep many values together and work
with them as one logical value.

Collections pair naturally with [higher order
functions](functions/higher_order.md) and comprehensions. That is the
common Acton path for turning one collection into another without
spreading the transformation across several places.

All collection types are statically typed. A list has one element type,
a set has one element type, and a dictionary has one key type and one
value type.
