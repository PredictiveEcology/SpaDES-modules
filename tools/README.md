# Wiki tooling — the module list

Regenerates the wiki page
**[Modules list](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Modules-list)**,
which catalogues every publicly available SpaDES module.

**Refresh it about every 3 months.** The page states its own generation date, so a
stale one is visible to readers.

```bash
cd tools
python3 update-wiki.py --out /tmp/Modules-list.md      # ~10 min, mostly GitHub API
```

Then review, and publish:

```bash
git clone https://github.com/PredictiveEcology/SpaDES-modules.wiki.git /tmp/wiki
cp /tmp/Modules-list.md /tmp/wiki/Modules-list.md
cd /tmp/wiki && git diff && git commit -am "Refresh module list" && git push
```

Requires `gh` (authenticated) and `curl`. Python 3.9+, no third-party packages.

## The two files

| file | what it holds | who edits it |
|---|---|---|
| `modules-list.template.md` | **everything editorial** — which modules are listed, how they are grouped into sections, their one-line descriptions, author names, the legend, the intro prose | a human |
| `update-wiki.py` | **only the data** — status markers, usage counts, the scanned-accounts table, the generated date | nobody, normally |

The split is the whole point. Grouping a module under "Fire" rather than "Carbon", or
describing what it does, is a judgement no scan can make — so the script never invents
page content. It fills in placeholders (`%GENERATED%`, `%SCAN_TABLE%`, `%SCFM%`,
`%CASTOR_PARENT%`, `%CASTOR_PUSHED%`, `%N_PROJECT_REPOS%`) and appends a status marker
to each module bullet. Everything else passes through untouched.

**To add a module to the page, edit the template.** Add a bullet in the right section:

```markdown
- [myModule](https://github.com/owner/myModule) — what it does (_Author Name_)
```

Leave the marker off; the script adds it.

## What the markers mean

```
🟢 active   commits on its live branch in the last 12 months
🔵 stable   no recent commits, but a project active in the last 18 months still uses it
⚪ quiet    neither
🗄 archived repository is read-only
⚠ still calls the retired raster / sp packages
·N          number of public projects that list this module
```

🔵 versus ⚪ is the distinction worth protecting. A finished module that still works and
is still used is not the same thing as an abandoned one, and commit recency alone cannot
tell them apart — `fireSense_EscapeFit` had no commits for a year while 13 projects
depended on it. That is why the usage graph exists.

## How it runs

1. **Enumerate** — every public repo in `ACCOUNTS`. A repo is a module if it has
   `<name>.R` at its root.
2. **Usage graph** — fetch the driver scripts (`global.R` and friends) from every
   scanned repo that is *not* a module, and record which module names they mention.
   This is the slow part; `--skip-usage` reuses `.wiki-cache/usage.json`.
3. **Status** — for each module the template links, resolve its live branch, count
   commits in the last year, and check for retired spatial packages.
4. **Render** — substitute placeholders, annotate bullets, write the page.
5. **Drift report + link check** — see below.

## The drift report

Printed to stderr at the end. It is the part that stops the page rotting between runs.

- **"module(s) on GitHub but NOT on the page"** — new modules have appeared. Decide
  which section each belongs in and add it to the template. The script will not guess.
- **"template entr(y/ies) no longer resolve as modules"** — a repo was deleted, renamed,
  or was never a module. Fix or remove the template entry.

Recurring, intentional cases are allowlisted in the script so they do not reappear every
quarter: `NOT_MODULES` (project repos, multi-module collections, and the three repos
whose name differs from the module inside them) and `EXCLUDED_MODULES` (scratch repos
and forks of listed modules). Add to these rather than tolerating a noisy report.

## Things that are easy to get wrong

Each of these is load-bearing; they are in the code because getting them wrong produced
a visibly incorrect page.

- **Read activity from the live branch, not the default branch.** Most of these repos do
  their work on `development` while `master`/`main` sits years behind — `fireSense`'s
  default branch was last touched in 2022 while `development` was current. `live_branch()`
  prefers `development`.
- **Strip R comments before looking for `raster`/`sp`.** `Biomass_core` was once flagged
  on the strength of a single commented-out `expectsInput(..., "RasterBrick", ...)`.
- **A `reqdPkgs` entry is not usage.** Four modules declare `raster` or `sp` and never
  call them; four others call them without declaring. `detect_legacy()` excludes the
  `reqdPkgs` block so ⚠ means the code actually uses them.
- **Archived repos must never read 🟢.** Their most recent commit is often the archiving
  itself. They can still be 🔵 — `timeSinceFire` is archived and LandWeb still calls it.
- **`gh repo list` needs `--limit 1000`.** Three of the eight accounts have more than the
  default 100 repos, and the truncation is silent.
- **Multi-module repos inherit.** `PredictiveEcology/scfm` holds ten modules in folders.
  They are released together, so the repository carries the status and the folders show
  none. Bullets pointing at `/tree/.../modules/...` are skipped by the annotator.
- **⚪ means "no public user found", not "abandoned".** Private and personal project
  repos are invisible to the usage scan. Never reword this on the page to imply more
  than it does.
- **Don't use bare `.replace()` on template text.** A silent no-op once shipped a whole
  section unconverted. Assert first.

## Changing the scan

- **New account publishing modules** → add it to `ACCOUNTS`. The scanned-accounts table
  on the page regenerates from that list.
- **bcgov / CASTOR** is *not* covered by `ACCOUNTS`. Those twelve modules were found by
  searching bcgov for `castor` by name and are listed in the template by hand. If the
  set changes, update the template's CASTOR section manually.
- **Thresholds** — `ACTIVE_DAYS` (365) and `USER_ACTIVE_DAYS` (548) at the top of the
  script.

## A caution about re-running

Metadata-only commits count as activity. After a housekeeping pass across many modules
(a licence header, an author change), a refresh will flip those modules to 🟢 on the
strength of your own commits. If that is the only change a run produces, it is making
the page less informative, not more — skip it.
