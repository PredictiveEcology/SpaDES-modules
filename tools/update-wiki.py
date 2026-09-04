#!/usr/bin/env python3
"""Regenerate the SpaDES-modules wiki "Modules list" page.

Run this roughly every 3 months:

    python3 tools/update-wiki.py --out Modules-list.md

Editorial content -- which modules exist, how they are grouped, their
descriptions and authors -- lives in `modules-list.template.md` next to this
script, and is never invented here. This script only recomputes the data:
status markers, usage counts, the scanned-accounts table and the date.

It also reports **drift**: modules that exist on GitHub but are missing from
the template, and template links that no longer resolve. Those need a human to
place or remove; the script will not guess.

Requires: `gh` (authenticated) and `curl` on PATH.
"""

from __future__ import annotations

import argparse
import csv
import datetime
import json
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
TEMPLATE = os.path.join(HERE, "modules-list.template.md")

# Accounts scanned in full. Add to this list if a new group starts publishing
# modules; the scanned-accounts table on the page is generated from it.
ACCOUNTS = [
    "PredictiveEcology", "tati-micheletti", "FOR-CAST", "CeresBarros",
    "achubaty", "ianmseddy", "eliotmcintire", "cboisvenue",
]

# Template links that are deliberately not modules (project repos, collections,
# non-SpaDES tools) or whose repo name differs from the module name. Listed so
# the drift report does not re-flag them every quarter.
NOT_MODULES = {
    "PredictiveEcology/scfmModules",        # 2018 collection, superseded by scfm
    "PredictiveEcology/scfm",               # multi-module repo; folders inherit
    "PredictiveEcology/spadesCBM", "PredictiveEcology/LandRCBM",
    "achubaty/LandR_MPB", "FOR-CAST/SBW_EasternBoreal", "bcgov/castor",
    "tati-micheletti/anthropogenicDisturbance_Demo", "CeresBarros/SpaDES4Dummies",
    "gparadis/ws3", "UBC-FRESH/spades_ws3",
    "fRI-Research/timeSinceFire", "fRI-Research/LandWeb_preamble",
    "fRI-Research/LandWeb_output",
}

# Repos whose module file is not named after the repo. Without this they look
# like non-modules and silently lose their status marker.
REPO_MODULE_NAME = {
    "FOR-CAST/LandbirdNRV_Prepinput": "bird_dataPrep",
    "bcgov/caribouSurvivalCastor": "survivalCastor",
    "bcgov/volumeReportCastor": "volumebyareaReportCastor",
}

# Modules that exist but are intentionally left off the page.
EXCLUDED_MODULES = {
    "TEST",                                 # scratch repo
    "LandR_speciesParameters",              # fork of Biomass_speciesParameters
}

ACTIVE_DAYS = 365      # 🟢 threshold: commits on the live branch within this
USER_ACTIVE_DAYS = 548 # 🔵 threshold: referring project pushed within this
WORKERS = 8

# Retired spatial stack. A module is flagged ⚠ only if it *calls* these,
# not if it merely lists them in reqdPkgs (see detect_legacy).
LEGACY_RX = re.compile("|".join([
    r"\braster::", r"\bsp::", r"\brgdal::", r"\brgeos::", r"\bmaptools::",
    r"(?:library|require|requireNamespace)\(\s*[\"']?(?:raster|sp|rgdal|rgeos|maptools)[\"']?\s*[,)]",
    r"\bRasterLayer\b", r"\bRasterStack\b", r"\bRasterBrick\b",
    r"\bSpatialPolygons(?:DataFrame)?\b", r"\bSpatialPoints(?:DataFrame)?\b",
    r"\bSpatialLines(?:DataFrame)?\b", r"\bSpatialPixels(?:DataFrame)?\b",
]))

# Bullets like:  - [name](https://github.com/owner/repo) — description
BULLET_RX = re.compile(
    r"^(?P<pre>\s*-\s\[)(?P<label>[^\]]+)"
    r"(?P<mid>\]\(https://github\.com/(?P<owner>[^/]+)/(?P<repo>[^/)#]+)(?P<sub>(?:/tree/[^)#]+)?)\))"
    r"(?P<rest>.*)$", re.M)


# ---------------------------------------------------------------- helpers

def sh(cmd: list[str], timeout: int = 60) -> str:
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    except subprocess.TimeoutExpired:
        return ""
    return r.stdout if r.returncode == 0 else ""


def gh_json(path: str):
    out = sh(["gh", "api", path])
    if not out:
        return None
    try:
        return json.loads(out)
    except json.JSONDecodeError:
        return None


def raw(owner: str, repo: str, branch: str, path: str) -> str:
    return sh(["curl", "-sfL", "--max-time", "25",
               f"https://raw.githubusercontent.com/{owner}/{repo}/{branch}/{path}"])


def strip_r_comments(txt: str) -> str:
    """Drop R comments, respecting quoted strings.

    Without this, a commented-out `expectsInput(..., "RasterBrick", ...)` makes
    a fully modernised module look like it still depends on raster.
    """
    out = []
    for line in txt.split("\n"):
        res, quote, i = [], None, 0
        while i < len(line):
            c = line[i]
            if quote:
                if c == "\\":
                    res.append(line[i:i + 2]); i += 2; continue
                if c == quote:
                    quote = None
                res.append(c)
            elif c in "\"'":
                quote = c; res.append(c)
            elif c == "#":
                break
            else:
                res.append(c)
            i += 1
        out.append("".join(res))
    return "\n".join(out)


def balanced(txt: str, open_idx: int) -> int:
    depth = 0
    for j in range(open_idx, len(txt)):
        if txt[j] == "(":
            depth += 1
        elif txt[j] == ")":
            depth -= 1
            if depth == 0:
                return j
    return len(txt) - 1


def reqd_block(code: str) -> str:
    m = re.search(r"reqdPkgs\s*=\s*list\(", code)
    if not m:
        return ""
    i = code.index("(", m.end() - 1)
    return code[i:balanced(code, i) + 1]


def detect_legacy(source: str) -> bool:
    code = strip_r_comments(source)
    blk = reqd_block(code)
    body = code.replace(blk, "") if blk else code   # declaration alone is not use
    return bool(LEGACY_RX.search(body))


# ---------------------------------------------------------------- scanning

def list_repos(account: str) -> list[dict]:
    """Every public repo. --limit 1000 matters: several accounts exceed 100."""
    out = sh(["gh", "repo", "list", account, "--visibility", "public",
              "--limit", "1000", "--json",
              "name,isFork,isArchived,pushedAt,defaultBranchRef"], timeout=180)
    try:
        return json.loads(out) if out else []
    except json.JSONDecodeError:
        return []


def root_paths(owner: str, repo: str, branch: str) -> list[str]:
    """Top-level blobs only (used for the module test and driver-script scan)."""
    t = gh_json(f"repos/{owner}/{repo}/git/trees/{branch}") or {}
    return [e["path"] for e in t.get("tree", []) if e.get("type") == "blob"]


def all_paths(owner: str, repo: str, branch: str) -> list[str]:
    """Every blob. Needed for R/*.R: a non-recursive tree lists "R" as a tree
    entry, so the blob filter drops the helper sources and ⚠ is under-reported."""
    t = gh_json(f"repos/{owner}/{repo}/git/trees/{branch}?recursive=1") or {}
    return [e["path"] for e in t.get("tree", []) if e.get("type") == "blob"]


def live_branch(owner: str, repo: str, default: str | None) -> str | None:
    """development where it exists, else the default branch.

    Default branches in this ecosystem are routinely years behind; reading
    activity off them is the single easiest way to get this page wrong.
    """
    if gh_json(f"repos/{owner}/{repo}/commits/development"):
        return "development"
    if default:
        return default
    info = gh_json(f"repos/{owner}/{repo}")
    return info.get("default_branch") if info else None


def commits_since(owner: str, repo: str, branch: str, since: str, path: str = "") -> int:
    q = f"repos/{owner}/{repo}/commits?sha={branch}&since={since}&per_page=100"
    if path:
        q += f"&path={path}"
    c = gh_json(q)
    return len(c) if isinstance(c, list) else 0


# ---------------------------------------------------------------- usage graph

DRIVER_RX = re.compile(r"\.R$", re.I)


def build_usage(repos_by_account: dict[str, list[dict]], module_names: set[str],
                verbose=True) -> tuple[dict[str, tuple[int, str]], int]:
    """Which public projects reference which module?

    A "project" is any scanned repo that is not itself a module but has R
    scripts at its root (global.R and the numbered pipeline scripts). This is
    what separates 🔵 stable from ⚪ quiet, so it matters more than it looks.
    """
    jobs, pushed = [], {}
    for account, repos in repos_by_account.items():
        for r in repos:
            full = f"{account}/{r['name']}"
            pushed[full] = (r.get("pushedAt") or "")[:10]
            if r["name"] in module_names or not r.get("defaultBranchRef"):
                continue
            jobs.append((account, r["name"], r["defaultBranchRef"]["name"]))

    def fetch(job):
        owner, repo, br = job
        hits = set()
        for p in root_paths(owner, repo, br):
            if not DRIVER_RX.search(p):
                continue
            txt = raw(owner, repo, br, p)
            for name in module_names:
                if re.search(rf"\b{re.escape(name)}\b", txt):
                    hits.add(name)
        return f"{owner}/{repo}", hits

    usage: dict[str, set[str]] = {}
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for i, (proj, hits) in enumerate(ex.map(fetch, jobs), 1):
            if verbose and i % 25 == 0:
                print(f"    usage scan {i}/{len(jobs)}", file=sys.stderr)
            for name in hits:
                if name != proj.split("/")[1]:      # ignore self-reference
                    usage.setdefault(name, set()).add(proj)

    out = {}
    for name, projs in usage.items():
        newest = max((pushed.get(p, "") for p in projs), default="")
        out[name] = (len(projs), newest)
    return out, len(jobs)


# ---------------------------------------------------------------- rendering

def marker(n12: int, archived: bool, legacy: bool,
           n_users: int, newest_user: str, user_cutoff: str) -> str:
    bits = []
    if archived:
        bits.append("🗄")
    # An archived repo's final commit is often the archiving itself, so it must
    # never read as 🟢.
    if n12 >= 1 and not archived:
        bits.append("🟢")
    elif n_users >= 1 and newest_user >= user_cutoff:
        bits.append("🔵")
    else:
        bits.append("⚪")
    if legacy:
        bits.append("⚠")
    s = " ".join(bits)
    if n_users >= 2:
        s += f" ·{n_users}"
    return s


def scan_table(counts: list[tuple[str, int, int]]) -> str:
    rows = ["| account | public repos | modules found |", "|---|---|---|"]
    for acct, nrepo, nmod in sorted(counts, key=lambda x: -x[1]):
        rows.append(f"| [{acct}](https://github.com/{acct}) | {nrepo} | {nmod} |")
    rows.append(f"| **total** | **{sum(c[1] for c in counts)}** "
                f"| **{sum(c[2] for c in counts)}** |")
    return "\n".join(rows)


# ---------------------------------------------------------------- main

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", default="Modules-list.md", help="page to write")
    ap.add_argument("--template", default=TEMPLATE)
    ap.add_argument("--skip-usage", action="store_true",
                    help="reuse usage.json from a previous run (much faster)")
    ap.add_argument("--cache", default=".wiki-cache")
    ap.add_argument("--no-linkcheck", action="store_true")
    args = ap.parse_args()

    os.makedirs(args.cache, exist_ok=True)
    today = datetime.date.today()
    since = (today - datetime.timedelta(days=ACTIVE_DAYS)).isoformat()
    user_cutoff = (today - datetime.timedelta(days=USER_ACTIVE_DAYS)).isoformat()

    template = open(args.template, encoding="utf-8").read()

    # -- 1. enumerate ----------------------------------------------------
    print("1/5  listing public repositories", file=sys.stderr)
    repos_by_account, counts, meta, module_names = {}, [], {}, set()
    for acct in ACCOUNTS:
        repos = list_repos(acct)
        repos_by_account[acct] = repos
        nmod = 0
        for r in repos:
            full = f"{acct}/{r['name']}"
            meta[full] = r
            br = (r.get("defaultBranchRef") or {}).get("name")
            if br and f"{r['name']}.R" in root_paths(acct, r["name"], br):
                nmod += 1
                module_names.add(r["name"])
        counts.append((acct, len(repos), nmod))
        print(f"     {acct}: {len(repos)} repos, {nmod} modules", file=sys.stderr)

    # -- 2. usage graph --------------------------------------------------
    usage_path = os.path.join(args.cache, "usage.json")
    if args.skip_usage and os.path.exists(usage_path):
        blob = json.load(open(usage_path))
        usage = {k: tuple(v) for k, v in blob["usage"].items()}
        n_projects = blob["n_projects"]
        print("2/5  usage graph: reused cache", file=sys.stderr)
    else:
        print("2/5  building usage graph (slow)", file=sys.stderr)
        usage, n_projects = build_usage(repos_by_account, module_names)
        json.dump({"usage": {k: list(v) for k, v in usage.items()},
                   "n_projects": n_projects}, open(usage_path, "w"))

    # -- 3. status for every entry the template links to -----------------
    print("3/5  resolving status for template entries", file=sys.stderr)
    entries, seen = [], set()
    for m in BULLET_RX.finditer(template):
        owner, repo, sub = m["owner"], m["repo"], m["sub"]
        if sub:
            continue          # folder module: inherits its repository's status
        if (owner, repo) in seen:
            continue
        seen.add((owner, repo))
        entries.append((owner, repo))

    def status(pair):
        owner, repo = pair
        info = meta.get(f"{owner}/{repo}") or gh_json(f"repos/{owner}/{repo}") or {}
        default = ((info.get("defaultBranchRef") or {}).get("name")
                   or info.get("default_branch"))
        br = live_branch(owner, repo, default)
        if not br:
            return pair, None
        modname = REPO_MODULE_NAME.get(f"{owner}/{repo}", repo)
        src = raw(owner, repo, br, f"{modname}.R")
        for p in all_paths(owner, repo, br):
            if p.startswith("R/") and p.lower().endswith(".r"):
                src += raw(owner, repo, br, p)
        archived = bool(info.get("isArchived", info.get("archived", False)))
        return pair, dict(n12=commits_since(owner, repo, br, since),
                          archived=archived, legacy=detect_legacy(src) if src else False,
                          resolved=bool(src))

    st = {}
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for pair, d in ex.map(status, entries):
            st[pair] = d

    # -- 4. render -------------------------------------------------------
    print("4/5  rendering", file=sys.stderr)

    cinfo0 = gh_json("repos/bcgov/castor") or {}
    castor_live = bool(commits_since("bcgov", "castor",
                                     cinfo0.get("default_branch", "main"), since))

    def annotate(m: re.Match) -> str:
        head = m["pre"] + m["label"] + m["mid"]
        if m["sub"]:
            return m.group(0)                        # inherits
        d = st.get((m["owner"], m["repo"]))
        if not d or not d["resolved"]:
            return m.group(0)                        # not a module: leave alone
        if m["owner"] == "bcgov":
            # bcgov is not in ACCOUNTS, so the usage graph cannot see CASTOR.
            # A module with its own recent commits is still 🟢; the rest fall back
            # to the parent project's liveness rather than to ⚪. No ·N: it would
            # not be comparable with the rest of the page.
            mk = "🟢" if d["n12"] >= 1 else ("🔵" if castor_live else "⚪")
            if d["legacy"]:
                mk += " ⚠"
            return f"{head} {mk}{m['rest']}"
        n_users, newest = usage.get(m["repo"], (0, ""))
        return f"{head} {marker(d['n12'], d['archived'], d['legacy'], n_users, newest, user_cutoff)}{m['rest']}"

    page = BULLET_RX.sub(annotate, template)

    # scfm: the folders inherit, so the repository carries the status
    scfm = st.get(("PredictiveEcology", "scfm"))
    if scfm is None:
        br = live_branch("PredictiveEcology", "scfm", "master")
        scfm = dict(n12=commits_since("PredictiveEcology", "scfm", br, since),
                    archived=False, legacy=True, resolved=True)
    scfm_users, scfm_newest = usage.get("scfm", (0, ""))
    if scfm_users == 0:      # module names never include the repo itself
        scfm_users, scfm_newest = count_scfm_usage(repos_by_account, meta, module_names)
    page = page.replace("%SCFM%", marker(scfm["n12"], False, True,
                                         scfm_users, scfm_newest, user_cutoff))

    cinfo = gh_json("repos/bcgov/castor") or {}
    cbr = cinfo.get("default_branch", "main")
    page = page.replace("%CASTOR_PARENT%",
                        "🟢" if commits_since("bcgov", "castor", cbr, since) else "⚪")
    page = page.replace("%CASTOR_PUSHED%", (cinfo.get("pushed_at") or "")[:10])
    page = page.replace("%GENERATED%", today.isoformat())
    page = page.replace("%SCAN_TABLE%", scan_table(counts))
    page = page.replace("%N_PROJECT_REPOS%", str(n_projects))

    open(args.out, "w", encoding="utf-8").write(page)
    print(f"     wrote {args.out}", file=sys.stderr)

    # -- 5. drift + link check -------------------------------------------
    print("5/5  drift report", file=sys.stderr)
    linked = {r for _, r in seen}
    missing = sorted(module_names - linked - EXCLUDED_MODULES)
    if missing:
        print(f"\n  {len(missing)} module(s) on GitHub but NOT on the page — "
              f"add them to the template by hand:", file=sys.stderr)
        for n in missing:
            owner = next(a for a, rs in repos_by_account.items()
                         if any(r["name"] == n for r in rs))
            print(f"    {owner}/{n}", file=sys.stderr)
    gone = sorted(f"{o}/{r}" for (o, r), d in st.items()
                  if d and not d["resolved"] and f"{o}/{r}" not in NOT_MODULES)
    if gone:
        print(f"\n  {len(gone)} template entr(y/ies) no longer resolve as modules:",
              file=sys.stderr)
        for g in gone:
            print(f"    {g}", file=sys.stderr)

    if not args.no_linkcheck:
        urls = sorted(set(re.findall(r"https://github\.com/[^\s)]+", page)))
        def check(u):
            code = sh(["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}",
                       "-L", "--max-time", "25", u])
            return u, code
        bad = []
        with ThreadPoolExecutor(max_workers=WORKERS) as ex:
            for u, code in ex.map(check, urls):
                if code != "200":
                    bad.append((code, u))
        print(f"\n  link check: {len(urls)} links, {len(bad)} bad", file=sys.stderr)
        for code, u in bad:
            print(f"    {code}  {u}", file=sys.stderr)

    print("\nDone. Review the diff, then copy into the wiki clone and push:", file=sys.stderr)
    print("  git clone https://github.com/PredictiveEcology/SpaDES-modules.wiki.git",
          file=sys.stderr)
    return 0


def count_scfm_usage(repos_by_account, meta, module_names) -> tuple[int, str]:
    """scfm is referenced by repo path, not by a bare module name."""
    rx = re.compile(r"PredictiveEcology/scfm|\bscfm[A-Z]\w*")
    projs, newest = set(), ""
    for acct, repos in repos_by_account.items():
        for r in repos:
            if (r["name"] == "scfm" or r["name"] in module_names
                    or not r.get("defaultBranchRef")):
                continue
            br = r["defaultBranchRef"]["name"]
            for p in root_paths(acct, r["name"], br):
                if not DRIVER_RX.search(p):
                    continue
                if rx.search(raw(acct, r["name"], br, p)):
                    projs.add(f"{acct}/{r['name']}")
                    newest = max(newest, (r.get("pushedAt") or "")[:10])
                    break
    return len(projs), newest


if __name__ == "__main__":
    sys.exit(main())
