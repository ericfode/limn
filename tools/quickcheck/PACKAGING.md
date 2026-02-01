# Scryer Prolog Package Management Research

> `pkg res | scr sta | dis opt`
> *(package research | Scryer state | distribution options)*

## Current State

**Scryer Prolog has no official package manager.** This is a known gap, with the 2025 goals explicitly mentioning "Work on at least a simple package system for Scryer Prolog."

### Available Options

#### 1. Bakage/pkg.pl (Experimental)

[GitHub: bakaq/pkg.pl](https://github.com/bakaq/pkg.pl)

An experimental package manager for Scryer Prolog. **NOT PRODUCTION READY.**

**Warnings:**
- Has "trivially exploitable arbitrary remote code execution"
- No transitive dependency support
- Early experimental stage

**How it works:**
1. Download `bakage.pl` into your project
2. Create `scryer-manifest.pl` with package metadata
3. Run `./bakage.pl install` to fetch dependencies to `scryer_libs/`
4. Load via `:- use_module(bakage).` and `:- use_module(pkg(package_name)).`

**Package manifest structure:**
```prolog
name(my_package).
main_file('src/main.pl').
license(mit, 'LICENSE').
dependencies([
    git('https://github.com/user/dep.git', main)
]).
```

#### 2. Logtalk Packs Tool

[Logtalk](https://logtalk.org/) includes a `packs` tool that works with multiple Prolog backends including Scryer.

**Advantages:**
- Cross-platform (works with SWI, Scryer, etc.)
- More mature tooling
- Active development

**Disadvantages:**
- Requires Logtalk installation
- Additional abstraction layer
- Different module system conventions

#### 3. Manual Distribution (Current Best Practice)

Most Scryer libraries are distributed as:
- Git repositories with `use_module(library(X))` or direct paths
- Copy/paste into project
- Git submodules

## Scryer Module System

Scryer supports ISO-standard modules:

```prolog
%% Declaring a module
:- module(my_module, [predicate1/2, predicate2/1]).

%% Using modules
:- use_module(library(lists)).           % Built-in library
:- use_module('path/to/file.pl').        % Local file
:- use_module(library(my_lib), [pred/1]). % Selective import
```

**Key points:**
- `library(X)` looks in Scryer's built-in library path
- Direct paths work for local modules
- No `pack_install/1` like SWI-Prolog

## Recommendation for Our QuickCheck

Given the current ecosystem state, I recommend a **layered approach**:

### Option A: Simple Distribution (Recommended Now)

Structure our QuickCheck as a standalone module that can be:
1. Copied directly into projects
2. Cloned as a git submodule
3. Used via direct path: `:- use_module('path/to/quickcheck.pl').`

**Proposed structure:**
```
quickcheck/
├── quickcheck.pl        # Main module (exports public API)
├── prng.pl              # PRNG module
├── gen.pl               # Generators module
├── shrink.pl            # Shrinking module
├── prop.pl              # Property testing core
├── README.md            # Usage documentation
└── test/                # Self-tests
    └── ...
```

**Single-file alternative:**
Concatenate all modules into one `quickcheck_standalone.pl` for easy copy-paste distribution.

### Option B: Bakage-Compatible (Future)

When bakage matures, add:
```prolog
%% scryer-manifest.pl
name(limn_quickcheck).
main_file('quickcheck.pl').
license(mit, 'LICENSE').
dependencies([]).
```

### Option C: Logtalk Pack (If Needed)

If cross-Prolog compatibility becomes important, port to Logtalk conventions.

## Next Steps

1. **Now:** Organize our QuickCheck with clean module boundaries
2. **Soon:** Create single-file standalone version
3. **Later:** Add bakage manifest when ecosystem matures
4. **Future:** Consider Logtalk port for wider adoption

## Sources

- [Scryer Prolog Package Manager Discussion](https://github.com/mthom/scryer-prolog/discussions/1041)
- [bakaq/pkg.pl - Experimental Package Manager](https://github.com/bakaq/pkg.pl)
- [2025 Goals for Scryer Prolog](https://github.com/mthom/scryer-prolog/discussions/2699)
- [Logtalk Packs Tool](https://logtalk.org/)

---

*pkg sta | opt cle | pat fwd*
*(package state | options clear | path forward)*
