# uroboro-transformations

## Usage
Defunctionalizing programs:
```
cat program.uro | ./defunc
```
Refunctionalizing programs:
```
cat program.uro | ./refunc
```

Works with all coverage complete Uroboro* programs without empty (co)data types.
No guarantees.

*Note: The order independent variant. In practice, this means that order dependent programs are disallowed.
