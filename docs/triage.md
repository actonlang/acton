# Bug triage and prioritization

This isn't some hard line policy but rather a softer guideline on how to think
with regards to triaging and prioritizing of bugs in Acton.

- silent errors
  - we prioritize silent errors over those that emit explicit error messages
  - for example, there was a bug that cause integer subtraction to give the
    wrong results; 5-1 = 3 and stuff like that
- seg faults
  - this causes a general feeling of brokenness and without an descriptive error
    message, it is rather difficult to work around
- things without workarounds
  - with type errors it is often possible to work around by adding type
    signatures or explicit type conversions, thus they have lower priority
    compared to bugs that are not as simple to work around
- other bugs

