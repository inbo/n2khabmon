# n2khabmon (development version)

- `namelist` data source: update names of some schemes (#13).

# n2khabmon 0.2.0 (2024-05-06)

- `schemes` & `scheme_types` data sources: drop scheme `HQ6120`; add type for scheme `HQ2330` (#10).
- `scheme_types` data source: define typegroups for schemes `SOIL_03.2` & `SURF_03.4_lotic` (#10).
- `namelist` data source: use longer names for MNE schemes (#10).

# n2khabmon 0.1.3 (2024-02-06)

- `read_scheme_types()`: fix the call to `n2khab::read_types()` when running `read_scheme_types(extended = TRUE)` ([995b32b](https://github.com/inbo/n2khabmon/commit/995b32b)).

# n2khabmon 0.1.2 (2024-01-18)

- A misspecification of MHQ monitoring scheme HQ3260 (#5) has been fixed in data sources `namelist`, `schemes` and `scheme_types`.

# n2khabmon 0.1.1 (2023-12-01)

- Set the minimum required version of {n2khab} (#3).

# n2khabmon 0.1.0 (2023-12-01)

- The data sources `schemes` and `scheme_types` that were distributed by package {[n2khab](https://inbo.github.io/n2khab)} have moved to {n2khabmon} (#1).
Also the code that generates both data sources has moved to {n2khabmon}.
Up to now these data sources have only been used as definitions of N2KHAB monitoring schemes and their target populations, hence it was better to put them in here.
- `read_schemes()` and `read_scheme_types()` moved from {n2khab} to {n2khabmon} (#1); they are deprecated in the former.
