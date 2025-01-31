Release Notes for KiCS2
=======================

Release notes for KiCS2 Version 3.3.0 (January 31, 2025)
--------------------------------------------------------

Changes to version 3.2.0:

  * Front-end updated to support multi-parameter type classes.
    For this purpose, the front-end supports the language extensions
    `MultiParamTypeClasses`, `FunctionalDependencies` and
    `FlexibleInstances` (similarly to Haskell).
    An example can be found in `testsuite/TypeclassTests/TestMPTCCoerce.curry`.
  * As a consequence of supporting multi-parameter type classes,
    the structure of Curry interface files (ending with `.icurry`)
    and AbstractCurry files (ending with `.acy`) has been slightly changed:
    type class constraints have now a list of type parameters
    instead of a single one. This can be seen in the new versions
    of the Curry packages `abstract-curry` (version 4.x) and
    `curry-interface` (version 4.x).
    Moreover, the names of internal operations generated for
    operations defined in type classes (e.g., instance operations)
    have been slightly changed (this is only visible in FlatCurry files).
  * `Prelude`: value generator for floats added


Release notes for KiCS2 Version 3.2.0 (July 8, 2024)
----------------------------------------------------

Changes to version 3.1.0:

  * Command `:interface` use the new interface pretty printer
    based on the Curry package `curry-interface` so that also
    information about type classes are shown
  * Update front end:
    - new option `--origin-pragmas` (only for use in the Curry Language Server)
    - incomplete cases are always extended with explicit calls to
      `Prelude.failed` in missing branches. For instance,

          head (x:_) = x

      is translated into the FlatCurry definition

          head xs = case xs of x:_ -> x
                               []  -> Prelude.failed


Release notes for KiCS2 Version 3.1.0 (April 4, 2024)
-----------------------------------------------------

Changes to version 3.0.0:

  * Changes in case mode: the case modes Haskell, Prolog, and Gödel
    are stronger so that they emit error messages instead of warnings,
    the default case mode is Curry, which is like Haskell but emit
    only warnings (see Section 3.7 of the PAKCS User Manual).
  * Front end does not include `Prelude` in imports of FlatCurry files
    when it is not necessary (e.g., if the language option `NoImplicitPrelude`
    is set).
  * Base libraries extended by including libraries for encapsulated search
    so that set functions can be used without installing packages.
    The new libraries are: `Control.Search.SetFunctions`(implementing
    set functions), `Control.Search.AllValues` (implementing a strong
    encapsulation as I/O operations), `Control.Search.Unsafe`
    (implementing strong encapsulation as non I/O operations, but this
    method has a non-declarative behavior), and `Control.Search.SearchTree`
    (implementing search trees).
  * Update CPM (modified options for command `upload`, support for
    automatic upload to Masala/CPM, add `--dependencies` option to clean command
    in order to clean all dependencies in the current package (useful to clean
    the standard homepackage).


Release notes for KiCS2 Version 3.0.0 (March 31, 2023)
------------------------------------------------------

This version is a major rewrite where the compiler and REPL
are written as Curry packages in order to re-use the current
infrastructure for Curry. Moreover, the implementation uses
[Haskell Stack](https://www.haskellstack.org/) to download
and install the required Haskell compiler required for KiCS2.
The other major changes concern the version of the base libraries
and support for the standard class `Data`, as listed in the following.

Changes to version 2.3.0:

  * Top-level expressions with `Monad` context are specialized to `IO`.
  * Top-level expressions with `Data` context are specialized to `Bool`.
  * Top-level expressions with `Floating` context are specialized to `Float`.
  * New compilation scheme for type classes to avoid problems
    with 0-ary non-deterministic definitions in instance declarations.
    Dictionaries are now represented as functions in order to
    enforce the evaluation of all instance operations.
  * Prelude: operation `(/==)` added.
  * The prelude operations `(=:=)` and `(=:<=)` changed from external
    to defined operations that call the external operations
    `constrEq` and `nonstrictEq`, respectively. This is meaningful
    to keep the `Data` constraint for `(=:=)` and `(=:<=)` whereas
    external operations have no class contexts.
  * Type class `Data` with operations `(===)` (equality) and
    `aValue` (non-deterministic value generator) added to the prelude.
    For each `data` declaration, `Data` instances for the defined type
    are automatically derived as long as the defined type is first-order
    (i.e., does not contain functional types).
    Free variable have type class constraint `Data`.
    The motivation for this design and its advantages are described in a
    [DECLARE/WFLP'19 paper](https://doi.org/10.1007/978-3-030-46714-2_15).
  * The standard libraries has been changed in order to keep the names
    and structure more closely with Haskell. Specialized functionality
    is moved separate packages. There is a separate
    [migration guide](https://git.ps.informatik.uni-kiel.de/curry/curry-libs/-/blob/master/MigrationGuide.md)
    describing the changes.
  * Libraries `FilePath`, `Directory`, `Distribution`, `Time`,
    `IOExts`, `ReadShowTerm` removed
    (now available in packages `filepath`, `directory`, `distribution`,
    `time`, `io-extra` and `read-legacy`).
  * Library `System` split into `System.Process`, `System.CPUTime`,
    `System.Environment`. 
    `System.Process` is available in package `process`.
    The rest remains in the library.
  * Implemented the "MonadFail-Proposal" for Curry
    (see <https://wiki.haskell.org/MonadFail_Proposal>).
  * Intermediate files are written into versioned directories, e.g.,
    the FlatCurry representation of `lib/Prelude.curry` is written
    to `lib/.curry/kics2-3.0.0/Prelude.fcy` (and similarly all
    other intermediate files). This avoids inconsistencies
    of intermediate files when different Curry systems are used.
  * Front end supports new language options
    `NoImplicitPrelude` and `NoDataDeriving`.
  * Front end has the old option `--extended` as default.
    In order to switch off the extensions for functional patterns
    and anonymous free variables, one can use the options
    `NoFunctionalPatterns` and `NoAnonFreeVars`, respectively.


Release notes for KiCS2 Version 2.3.0 (October 12, 2020)
--------------------------------------------------------

Changes to version 2.2.0:

* CPM updated (faster `update` command and support for caret
  and tilde comparison operators in dependency descriptions)


Release notes for KiCS2 Version 2.2.0 (October 30, 2019)
--------------------------------------------------------

Changes to version 2.0.0:

  * Prelude: `chr` defined as total operation
  * Libraries `AllSolutions`, `Findall`, `SearchTree`, `SearchTreeGenerators`,
    `SearchTreeTraversal`, `ValueSequence removed
    (available in package `searchtree` as `Control...`)
  * Library `AnsiCodes` removed (available in package `ansi-terminal`
    as `System.Console.ANSI.Codes`)
  * Library `Array` removed (available in package `array` as `Data.Array`)
  * Library `Combinatorial` removed (available in package `combinatorial`)
  * Library `CPNS` removed
    (available in package `cpns` as `Network.CPNS`)
  * Library `Dequeue` removed (available in package `queue` as `Data.Queue`)
  * Library `Distribtion`: `curryCompilerRevisionVersion` added,
    operations related to load paths removed (available in package
    `currypath` in library `System.CurryPath`),
    operations to call the front end removed (available in package
    `frontend-exec` in library `System.FrontendExec`)
  * Library `Format` removed (available in package `printf` as `Data.Format`)
  * Library `NamedSocket` removed
    (available in package `cpns` as `Network.NamedSocket`)
  * Library `Nat` removed (available in package `peano` as `Data.Nat`)
  * Library `Profile` removed
    (available in package `profiling` as `Debug.Profile`)
  * Library `PropertyFile` removed
    (available in package `propertyfile` as `Data.PropertyFile`)
  * Library `Random` removed (available in package `random` as `System.Random`)
  * Libraries `RedBlackTree`, `SetRBT`, and `TableRBT` removed
    (available in package `redblacktree` as `Data.RedBlackTree`,
    `Data.Set.RBTree`, and `Data.Table.RBTree` with slightly renamed API
    operations).
  * Library `SCC` removed (available in package `scc` as `Data.SCC`)
  * Library `Socket` removed
    (available in package `socket` as `Network.Socket`)
  * Library `SetFunctions` removed
    (available in package `setfunctions` as `Control.SetFunctions`)
  * Library `Traversal` removed
    (available in package `traversal` as `Data.Traversal`)
  * Library `Test.EasyCheck` removed (available in package `easycheck`).
    The import of this library should be replaced by `Test.Prop`.
  * Library `Test.Contract` removed
    (available in package `contracts`)
  * Front end updated in order to generate ASTs with source code
    position information (see package `curry-ast`).
  * Curry Port Name Server removed from `currytools`
    (available as `curry-cpnsd` in package `cpns`)
  * Registry for dynamic web pages removed from `currytools`
    (available as `curry-cgi` in package `html-cgi`)
  * CPM updated (e.g., faster `update` operation)


Release notes for KiCS2 Version 2.0.0 (November 23, 2018)
---------------------------------------------------------

This version has almost the same functionality as Version 0.6.0
but adds type classes similar to Haskell 98.
In addition to version 0.6.0 and type classes,
this version contains the following changes:

  * Base libraries are now versioned. The actual version of the base
    libraries can be queried by `kics2 --base-version` or inside
    Curry programs by the operation `Distribution.baseVersion`.
    The versioning of base libraries is intended to be used by CPM.
  * Library `Findall`: `oneValue` added
  * Library `SetFunctions`: `minValueBy` and `maxValueBy` added, `minValue`
    and `maxValue` depend on `Ord` context.
  * Some libraries removed since they are available as packages
    which can easily be installed via `cypm`:

      - `Assertion`
        (no longer used since `currytest` has been replaced by `currycheck`)
      - `CSV` (now available as `Text.CSV` in package `csv`)
      - `Parser` (now available in package `fl-parser`)
      - `RegExp` (now available in package `regexp`)
      - `UnsafeSearchTree` (now available in package `searchtree`)
  * Arbitrary precision integers and double-precision floating point numbers
    are now used for the implementation of Int and Float, respectively.


Release notes for KiCS2 Version 0.6.0
-------------------------------------

Changes to version 0.5.1:

  * Curry Package Manager added as tool `cypm`.
  * Various tools (e.g., addtypes, currybrowser, currycheck, currydoc,
    currypp, erd2curry, runcurry, spicey, verify, xmldata)
    have been removed from the distribution
    since they are not necessary for the basic use of KiCS2 and they can
    easily be installed (by a one-line command) locally via `cypm`.
    Instructions how to install these tools are included
    in the KiCS2 manual.
  * Operation `RegExp.match`: order of arguments swapped
  * Curry preprocessor does not generate implicit `match` for regexps.
  * Some libraries removed since they are available as packages
    which can easily be installed via `cpm`:

      - `AbstractCurry.*` (now in package `abstract-curry`)
      - `Bootstrap3Style` (now in package `html`)
      - `CHR` (now in package `chr-curry`)
      - `CategorizedHtmlList` (now in package `html`)
      - `CurryStringClassifier` (now in package `addtypes`)
      - `Database.ERDGoodies` (now in package `ertools`)
      - `Database.ERD` and `Database.CDBI.*` (now in package `cdbi`)
      - `FlatCurry.*` (now in package `flatcurry`)
      - `FlatCurry.Annotated.*` (now in package `flatcurry-annotated`)
      - `GraphInductive` (now in package `graph-inductive`)
      - `GUI` (now in package `gui`)
      - `HTML` (now in package `html` as library `HTML.Base`)
      - `HtmlParser` (now in package `html`)
      - `KeyDatabaseSQLite` (now in package `keydb`)
      - `JavaScript` (now in package `javascript`)
      - `Mail` (now in package `mail`)
      - `Markdown` (now in package `markdown`)
      - `Prolog` (now in package `prolog`)
      - `Rewriting.*` (now in package `rewriting`)
      - `URL` (now in package `url`)
      - `WUI` and `WUIjs` (now in package `wui`)
      - `XML` (now in package `xml`)
      - `XmlConv` (now in package `xml`)

Release notes for KiCS2 Version 0.5.1
-------------------------------------

Changes to version 0.5.0:

  * Makefiles changed so that parallel build (make -j) is possible.
  * Option `local` added to switch to local compilation mode (for
    locally installed systems). This replaces the old setting in the
    Makefile for global/local installations.
  * Library `Database.ERD...` added (formerly part of ER currytools).
  * Library `IOExts` uses system commands
    `lockfile-create` and `lockfile-remove` instead of `lockfile`
    for internal file synchronization in order to remove dependency
    on package `procmail`.
  * Library `Nat` for Peano numbers added.
  * Libraries `Rewriting.*` for term rewriting in Curry extended
    to deal with rewriting strategies, narrowing strategies,
    critical pairs, definitional trees.
  * Library `Sort`: ...Sort operations renamed to ...SortBy and
    ...Sort operations with standard ordering added.
  * Library `State` with an implementation of the state monad added.
  * Library `Test.EasyCheck` split into two modules to have less
    import dependencies when putting properties into a module.
  * Library `Test.Prop` added as a clone of `Test.EasyCheck` which defines
    the interface but no implementation so that it does not import
    any other library.
    import dependencies when putting properties into a module.
  * CurryDoc shows properties and contracts, if they are present
    in source files, in the HTML documentation.
  * The Curry Preprocessor supports a new option `contracts`
    to transform contracts (specifications, pre/postconditions)
    into run-time assertions.
  * New partial evaluator (command "peval") added to `currytools`.
  * New tool Curry2Verify (to translate Curry programs into Agda programs)
    added to `currytools`.
  * Names of tool executables changed to `kics2 toolname` or `curry toolname`.
  * Name of Curry parser `cymake` changed to `curry frontend`.
  * Tool `curry analysis` (CASS): option `--all` added.
  * Tool `curry analysis` (CASS): analysis `Functional` added and
    analysis `Deterministic` modified so that it considers encapsulated search.
  * Tool `curry analysis` (CASS): simple termination analysis `Terminating`
    added.
  * Tool `curry analysis` (CASS): analysis `TypesInValues` added.
  * Tool `curry check` supports also testing with float arguments.
  * Run-time parameters passed to KiCS2 must be separated by `--`.


Release notes for KiCS2 Version 0.5.0
-------------------------------------

Changes to version 0.4.1:

  * Type `Success` is now a type synonym for `Bool` and
    `success` is defined as `True` in the prelude.
  * Prelude: operations `(===)` and `(&&>)` removed
  * Library Distribution: some load path handling operations
    (`findFileInLoadPath`, `lookupFileInLoadPath`, `readFirstFileInLoadPath`,
    `getLoadPath`, `getLoadPathForFile`) removed since they are deprecated
    (use operations like `getLoadPathForModule` and
    `lookupModuleSourceInLoadPath` instead of the deprecated operations
    since they handle hierarchical module names better)
  * Libraries `List`: `diagonal` added
  * Libraries `meta/*` removed (since they have been replaced
    by libraries with hierachical names, see below)
  * Hierarchical libraries for FlatCurry added and extended:
    in order to to compatible with future versions, the following
    imports should be adapted in programs working with AbstractCurry:

    - replace `import FlatCurry` by

          import FlatCurry.Types
          import FlatCurry.Files

    - replace `import FlatCurryGoodies` by

          import FlatCurry.Goodies

    - replace `import FlatCurryPretty` by

          import FlatCurry.Pretty

    - replace `import FlatCurryRead` by

          import FlatCurry.Read

    - replace `import FlatCurryShow` by

          import FlatCurry.Show

    - replace `import FlatCurryXML` by

          import FlatCurry.XML

    - replace `import CompactFlatCurry` by

          import FlatCurry.Compact

    - replace `import AnnotatedFlatCurry` by

          import FlatCurry.Annotated.Types

    - replace `import AnnotatedFlatGoodies` by

          import FlatCurry.Annotated.Goodies

    - replace `import AnnotatedFlatCurryPretty` by

          import FlatCurry.Annotated.Pretty

    - replace `import FlexRigid` by

          import FlatCurry.FlexRigid

  * New libraries added: Bootstrap3Style, ErrorState, SCC, Rewriting.*,
    FlatCurry.Annotated.TypeInference
  * currytools: typeinference removed since it is now contained
    in the standard system libraries
  * currytools: new tool `currycheck` for automated test execution added
  * REPL command `:compile` added    



Release notes for KiCS2 Version 0.4.1
-------------------------------------

Changes to version 0.4.0:

  * Hierarchical libraries for AbstractCurry added and extended:
    in order to to compatible with future versions, the following
    imports should be adapted in programs working with AbstractCurry:

    - replace `import AbstractCurry` by

          import AbstractCurry.Types
          import AbstractCurry.Files

    - replace `import AbstractCurryGoodies` by

          import AbstractCurry.Select
          import AbstractCurry.Build

    - replace `import PrettyAbstract` by

          import AbstractCurry.Pretty



Release notes for KiCS2 Version 0.4.0
-------------------------------------

Changes to version 0.3.3:

  * The REPL and compiler accepts also property definitions of the form
    "-Dprop=val" as arguments which overrides the kics2rc definitions.
  * The extension for records (with special Curry syntax) was removed.
    Instead, KiCS2 now supports Haskell's record syntax.
    See the manual for a detailed description of the new record syntax.
  * A new representation of AbstractCurry was introduced:

    - AbstractCurry files now contain version information
    - support for new record syntax
    - support for newtype declarations
    - evaluation annotations removed
    - arity of constructor declarations removed
    - simplified representation of function rules
    - String literals added

  * Library `Pretty`: code and interface updated, e.g., some combinators
    renamed and new combinators added (e.g., to support ANSI formatting
    and colorisation of documents)
  * Library `FunctionInversion` added
  * Library `AnnotatedFlatCurryPretty` added
  * Library `Either` extended with functions `fromLeft` and `fromRight`
  * Library `ShowS` added
  * Library `AnsiCodes` added
  * Prelude operation `===` added


Release notes for KiCS2 Version 0.3.3
-------------------------------------

Changes to version 0.3.2:

  * The Curry syntax is extended to also support binary integer literals.
    For instance, `0b101010` or `0B101010` can now be lexed
    and are converted to the integer value `42`.
  * Changed target code representation of string literals. Before, string
    literals s were desugared into lists of characters by the frontend
    and then translated character-wise,
    resulting in large Haskell code that was hard to read.
    We now represent string literals s like `"Hello from Curry"` in target
    code by a call to a primitive function `toCurryString`. For instance,
    the above literal gets translated to `toCurryString "Hello from Curry"`.
    While adding some (small) runtime overhead, this improves the readability
    of the code and shortens the code size.
  * Curry programs compiled with `kics2` can now be profiled using the GHC's
    profiling capabilities. Please see the manual for additional information.
  * Library `AbstractCurryGoodies` added.
  * Tool `data2xml` for data conversion to XML and back added.


Release notes for KiCS2 Version 0.3.2
-------------------------------------

Changes to version 0.3.1:

  * KiCS2 can also be invoked via the command `curry` in the bin directory.
  * Library `Prelude`: `solve` and `&&>` added.
  * Library `Prolog` added.
  * Tool `CASS`: new analysis `RequiredValues` added.
  * Optimization tool `bindingopt` for transforming Boolean equalities
    into constraint equalities added.


Release notes for KiCS2 Version 0.3.1
-------------------------------------

Changes to version 0.3.0:

  * Changed representation of integer literals containing free variables.
    Such literals are now represented as arithmetic expressions well-known
    from (constraint-)logic programming, such as

        { x = (4 * _x4 + 1) }

    instead of the binary representation

        { x = (Pos (I (O _x4))) }

    Furthermore, the representation can be configured during installation
    and is now also documented in the manual as a language extension.
  * REPL: The prompt now shows the loaded module first instead of after
    the list of added modules.
  * REPL: The command `:add` now accepts a list of modules to be added
    instead of only a single module. The list of added modules is now
    sorted in ascending order and no longer contains duplicates.
  * REPL: new (experimental) options `prelude` (for defining the name
    of the standard prelude) and `parser` (for setting additional
    front-end options) included.
  * Library `Distribution`: front-end parameters extended by supporting
    "special" (i.e., aribrary) arguments.
  * Library `Distribution`: front-end paramter `outfile` removed and
    front-end parameter `htmldir` added.
  * Library `Distribution` and `FlatCurry`: after calling the front end,
    an exception is raised if the front end returns with an error
    (due to an illegal source program).
  * Libraries `Float` and `Integer`: Power operators added.
  * Library `Float`: hyperbolic/ arc sine/cosine/tangent operators added.
  * Library `HTML`: `formMetaInfo` added, HTML header changed to HTML5.
  * Library `List`: Improved version of `inits` added.
  * Library `Prelude`: Functions `forIO`, `forIO_`, `when`, `unless` added.
  * Library `SetFunctions`: `notEmpty` added.
  * Libraries `Format` and `RegExp` added.
  * Tool `erd2curry` updated to support easier installation.
    Bug fix in code generation w.r.t. checking of cardinality constraints.
  * Tool `spicey` (web framework) added and updated with a RESTful interface
    for entities.


Release notes for KiCS2 Version 0.3.0
-------------------------------------

Changes to version 0.2.4:

  * Front-end updated (it writes and reads new kinds of interface files
    with the suffix `.icurry`).
  * The default search strategy has been changed to *breadth-first search*.
  * Fixed a bug in `IO.hGetLine` which caused an end-of-file error when
    reading a non-empty line without a newline termination.
  * Compilation scheme changed so that each function has an additional
    argument indicating the encapsulation level in order to obtain
    a reasonable implementation of nested encapsulated set functions with
    possible higher-order functions.
  * Library `UnsafeSearchTree` added. This library is similarly to
    `SearchTree` but does not evaluate unbound local variables
    to their ground values. Thus, one can have results containing
    unbound variables (which can be checked by the operations
    `isVar` and `getVarId`).


Release notes for KiCS2 Version 0.2.4
-------------------------------------

Changes to version 0.2.3:

  * Fixed a long standing bug which occured in combination with locally
    polymorphic sub-expressions. For example, in the rule
    `f = fst (3, id)` the identifier `id` has type `forall a . a -> a`,
    but the type variable `a` does not occur in the type of `f`.
    This lead to a type error in the generated Haskell sources.
    The compiler now solves this problem by replacing such type variables
    with the unit type `()`, thus the above mentioned rule becomes
    `f = fst (3, id :: () -> ())`.
  * The compiler now uses type inference for FlatCurry programs to
    improve the generated Haskell files.
  * Library `Function` added.
  * The installation process now uses `cabal`'s capabilities to install
    all required and locally generated Haskell packages into the installation
    directory, inside a new directory `pkg`.
    As a consequence, the installation process no longer depends on packages
    to be installed for the user.
    Furthermore, different versions of the KiCS2 compiler can now
    be installed in parallel, even when they are configured
    to use different versions of GHC.
  * Library `Prelude`: operations for encapsulated search omitted
    since the base primitive `try` is not implemented.
    Use libraries `AllSolutions`, `SearchTree`, and `SetFunctions`
    for encapsulated search.
  * Library `SearchTree`: operation `getAllValuesWith` added,
    operation `someValueBy` slightly changed and renamed to `someValueWith`.
  * Internal constructor in library FiniteMap changed.
    As a consequence, compiler intermediate (analysis) files produced
    by previous KiCS2 versions are incompatible to this version.
  * CASS tool (Curry Analysis Server System) added and integrated
    into currydoc.
  * Library `HTML`: default (white) background for body of generated
    web pages removed (since this is usually defined in css files).
  * Type inference tool (see `currytools/typeinference`) added.
    This tool can be used to annotate expressions in FlatCurry programs
    with their type.


Release notes for KiCS2 Version 0.2.3
-------------------------------------

Changes to version 0.2.2:

  * The KiCS2 REPL now supports the configuration of the user prompt
    via `:set prompt <prompt>`.

  * Syntax highlighting for KDE's editor component `katepart` added,
    supporting Curry and Literate Curry.

  * A frontend bug leading to an internal erroe when using as patterns
    inside functional patterns is fixed.

  * Some bugs regarding the combination of functional patterns and
    nonlinear left-hand-sides are fixed. The parser also accepts
    as-patterns inside functional patterns.

  * The GHC and cabal binaries used for installation can now be
    explicitly set. See the installation instructions for details.

  * Windows support for kernel installation.

    The KiCS2 kernel system (`kics2c`, `kics2`, `cleancurry`, `cymake`) can
    now be installed under Windows using MinGW. See the installation
    instructions for details.

  * Library module `Directory` updated and extended.

    - Replacement of `String` by `FilePath`
    - Added new function `createDirectoryIfMissing :: Bool -> FilePath -> IO ()`
    - Added new function `getHomeDirectory :: IO FilePath`
    - Added new function `getTemporaryDirectory :: IO FilePath`
    - Added new function `copyFile :: FilePath -> FilePath -> IO ()`

  * Library `SetFunctions`: Operations `choose` and `select` added.

  * The front end accepts typed expressions of the form "Expr :: TypeExpr".
    Such type annotations are considered by the compiler.
    For instance, `(unknown :: Bool)` evaluates to `False` or `True`.

  * Library `FlatCurry` extended to represent typed expressions
    by a new constructor `Typed`.

-------------------------------------------------------------------------

