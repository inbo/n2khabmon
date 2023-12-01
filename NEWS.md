# n2khabmon 0.1.0 (2023-12-01)

- The data sources `schemes` and `scheme_types` that were distributed by package {[n2khab](https://inbo.github.io/n2khab)} have moved to {n2khabmon} (#1).
Also the code that generates both data sources has moved to {n2khabmon}.
Up to now these data sources have only been used as definitions of N2KHAB monitoring schemes and their target populations, hence it was better to put them in here.
- `read_schemes()` and `read_scheme_types()` moved from {n2khab} to {n2khabmon} (#1); they are deprecated in the former.
