#!/usr/bin/env python3
"""Classify SpaDES modules into types, from source + the cross-module object graph.

Imported by update-wiki.py. See tools/README.md for the taxonomy.

Order of evidence: code signals first; the module's own description next; the
README only as a last resort, and only when it is not the unedited template.
Anything still undetermined is reported as "in development" rather than guessed.
"""
import re, os, glob, json, difflib

ICON={"Data":"📥","Parameterizing":"🎯","Prediction":"⏩","Simulation":"🔁",
      "Summary":"📋","Translator":"🔌","Library":"📚","Validation":"✅",
      "In development":"🚧"}

def strip_comments(txt):
    out=[]
    for line in txt.split("\n"):
        res,q,i=[],None,0
        while i<len(line):
            c=line[i]
            if q:
                if c=="\\": res.append(line[i:i+2]); i+=2; continue
                if c==q: q=None
                res.append(c)
            elif c in "\"'": q=c; res.append(c)
            elif c=="#": break
            else: res.append(c)
            i+=1
        out.append("".join(res))
    return "\n".join(out)

def bal(t,i):
    d=0
    for j in range(i,len(t)):
        if t[j]=="(": d+=1
        elif t[j]==")":
            d-=1
            if d==0: return j
    return len(t)-1

def drop_quoted(code):
    """quote(...) holds a model spec for ANOTHER module to fit; not our fitting."""
    out=code
    while True:
        m=re.search(r"\bquote\s*\(", out)
        if not m: return out
        out=out[:m.start()]+" "+out[bal(out,out.index("(",m.end()-1))+1:]

def objnames(code, fn):
    """expectsInput("x"…), expectsInput(objectName = "x"…), and single quotes."""
    return {m.group(1) for m in
            re.finditer(fn + r"""\(\s*(?:objectName\s*=\s*)?["']([^"']+)["']""", code)}

DATA=[r"\bprepInputs\w*\(",r"\bpreProcess\(",r"\bpostProcess\(",
      r"sourceURL\s*=\s*(?:\"http|extractURL)",r"googledrive::",r"drive_download",
      r"\bdownload\.file\(",r"\bdbGetQuery\(",r"\bdbReadTable\(",r"\bst_read\("]
STATS=[r"\bglm\(",r"\bglm\.nb\(",r"(?<![A-Za-z._$])lm\(",r"\boptim\(",r"\bDEoptim",
       r"\bnls\(",r"\bgam\(",r"\bbam\(",r"randomForest",r"\bgbm\(",r"\bglmer\(",
       r"\blmer\(",r"\bglmmTMB",r"\bnlminb\(",r"\bmle2?\(",r"\bbrms::",r"\brstan",
       r"\bcaret::",r"\bkeras",r"\btorch",r"\bfitdistr",r"\bnnet\(",r"\bneuralnet",
       r"\bcv\.glmnet",r"\bsurvreg\(",r"\bclogit\(",r"\bfit_issf\(",r"\bamt::"]
RECUR=r"scheduleEvent\(\s*sim\s*,\s*time\(sim\)\s*\+"
REPORT=[r"rmarkdown::render",r"\brender\(\s*input",r"\bknitr::knit"]
VAL_STRONG=[r"\bRMSE\b",r"\brmse\b",r"\bcross[-_]?valid",r"\bconfusionMatrix\b",
            r"\bgoodness",r"\bresiduals?\(",r"\bMAE\b"]
VAL_WEAK=[r"\bR2\b",r"\bRsq\b",r"\bobserved\b",r"\bbias\b"]
LIB=r"\bfactorial\b|yield ?tables?|\blibrar(y|ies)\b|look-?up|reference set"

STATIC={"studyArea","studyAreaLarge","studyAreaReporting","rasterToMatch",
 "rasterToMatchLarge","sppEquiv","sppEquivCol","sppColorVect","speciesTable",
 "species","ecoregion","ecoregionMap","ecoregionRst","rstLCC","cceArgs",
 "sppNameVector","studyAreaPSP","climateVariables","flammableRTM","rstFlammable",
 "standAgeMap","LCC","sppMultipliers","speciesEcoregion","minRelativeB",
 "speciesLayers","sufficientLight","factorialSpeciesTable","cohortDefinitionCols"}
# Declared but not actually produced -- would invent a feedback loop.
BOGUS={("caribou_SSUD","timeSinceFire")}

def load(srcdir="src"):
    mods={}
    for f in sorted(glob.glob(f"{srcdir}/*.R")):
        p=os.path.basename(f)[:-2].split("__")
        if len(p)<3: continue
        txt=open(f,encoding="utf-8",errors="replace").read()
        if not txt.strip(): continue
        code=strip_comments(txt)
        dm=re.search(r'description\s*=\s*',code); desc=""
        if dm:
            seg=code[dm.end():dm.end()+900]; cut=seg.find("keywords")
            desc=" ".join(re.findall(r'"([^"]+)"',seg[:cut if cut>0 else 900]))
        outs={o for o in objnames(code,"createsOutput")-STATIC if (p[2],o) not in BOGUS}
        mods[p[2]]=dict(owner=p[0],repo=p[1],code=code,fit=drop_quoted(code),desc=desc,
                        ins=objnames(code,"expectsInput")-STATIC,outs=outs,
                        recur=bool(re.search(RECUR,code)))
    return mods

def cycles(mods):
    nodes=[m for m,d in mods.items() if d["recur"]]
    prod={}
    for m in nodes:
        for o in mods[m]["outs"]: prod.setdefault(o,set()).add(m)
    edges={m:set() for m in nodes}
    for b in nodes:
        for o in mods[b]["ins"]:
            for a in prod.get(o,()):
                if a!=b: edges[a].add(b)
    index={};low={};on=[];stack=[];comps=[];n=[0]
    import sys; sys.setrecursionlimit(10000)
    def sc(v):
        index[v]=low[v]=n[0]; n[0]+=1; stack.append(v); on.append(v)
        for w in edges[v]:
            if w not in index: sc(w); low[v]=min(low[v],low[w])
            elif w in on: low[v]=min(low[v],index[w])
        if low[v]==index[v]:
            c=[]
            while True:
                w=stack.pop(); on.remove(w); c.append(w)
                if w==v: break
            comps.append(c)
    for v in nodes:
        if v not in index: sc(v)
    inc={m for c in comps if len(c)>1 for m in c}
    inc |= {m for m in nodes if mods[m]["ins"] & mods[m]["outs"]}
    return inc, [c for c in comps if len(c)>1]

def boilerplate(prose_dir="prose"):
    """READMEs that are the unedited template say nothing about the module."""
    docs={}
    for f in glob.glob(f"{prose_dir}/*.txt"):
        t=open(f,encoding="utf-8",errors="replace").read()
        if len(t)<50: continue
        name=os.path.basename(f)[:-4]
        t=re.sub(r"```.*?```","",t,flags=re.S)
        t=re.sub(r"[A-Za-z_]*"+re.escape(name)+r"[A-Za-z_]*","MOD",t)
        docs[name]=re.sub(r"\s+"," ",t).strip().lower()
    dup=set()
    keys=sorted(docs)
    for i,a in enumerate(keys):
        for b in keys[i+1:]:
            if difflib.SequenceMatcher(None,docs[a],docs[b]).ratio()>0.85:
                dup.add(a); dup.add(b)
    return dup, docs

def classify(mods, inc, dup, docs):
    res={}
    for m,d in mods.items():
        T=[]
        if any(re.search(p,d["code"]) for p in DATA) or re.search(r"studyarea|data",m.lower()):
            T.append("Data")
        if any(re.search(p,d["fit"]) for p in STATS) or re.search(r"Fit$|Parameters$",m):
            T.append("Parameterizing")
        if d["recur"] and m in inc:   T.append("Simulation")
        if d["recur"] and m not in inc: T.append("Prediction")
        if any(re.search(p,d["code"]) for p in REPORT) or re.search(r"summary|Summary|Report(?!ing)",m):
            T.append("Summary")
        if re.search(r"translat|connection module|harmoniz|interface between",(d["desc"]+" "+m).lower()):
            T.append("Translator")
        if re.search(LIB,(d["desc"]+" "+m).lower(),re.I): T.append("Library")
        strong=sum(len(re.findall(p,d["code"])) for p in VAL_STRONG)
        weak=sum(len(re.findall(p,d["code"])) for p in VAL_WEAK)
        if (re.search(r"valid|evaluat",m,re.I)
                or re.search(r"validat|evaluat|goodness.of.fit|compare[sd]? .{0,40}observ",d["desc"],re.I)
                or (strong>=1 and strong+weak>=3)):
            T.append("Validation")
        # last resort: a README that was actually written
        if not T and m in docs and m not in dup:
            low=docs[m]
            PROSE={"Data":[r"\bdownload",r"\bprepar",r"\bretriev",r"\bdata prep"],
                   "Parameterizing":[r"\bcalibrat",r"\bestimat",r"\bparameteri[sz]"],
                   "Prediction":[r"\bpredict",r"\bforecast"],
                   "Simulation":[r"\bsimulat",r"each (time ?)?step",r"\bdynamic"],
                   "Summary":[r"\bsummar",r"post-?hoc",r"\breport"],
                   "Library":[r"\bfactorial",r"\blibrar",r"look-?up"]}
            sc={k:sum(len(re.findall(p,low)) for p in v) for k,v in PROSE.items()}
            best=max(sc,key=sc.get)
            if sc[best]>=3: T.append(best+"*")     # * = from prose, lower confidence
        if not T: T.append("In development")
        res[m]=T
    return res


def from_sources(sources, prose=None):
    """Classify from in-memory text rather than a cache directory.

    sources: {module name -> concatenated .R source}
    prose:   {module name -> README/Rmd text}, optional, used only as a last resort
    Returns {module name -> [type, ...]}; a trailing "*" marks a type inferred
    from prose rather than code.
    """
    mods = {}
    for m, txt in sources.items():
        if not txt or not txt.strip():
            continue
        code = strip_comments(txt)
        dm = re.search(r'description\s*=\s*', code)
        desc = ""
        if dm:
            seg = code[dm.end():dm.end() + 900]
            cut = seg.find("keywords")
            desc = " ".join(re.findall(r'"([^"]+)"', seg[:cut if cut > 0 else 900]))
        outs = {o for o in objnames(code, "createsOutput") - STATIC
                if (m, o) not in BOGUS}
        mods[m] = dict(owner="", repo="", code=code, fit=drop_quoted(code), desc=desc,
                       ins=objnames(code, "expectsInput") - STATIC, outs=outs,
                       recur=bool(re.search(RECUR, code)))
    inc, comps = cycles(mods)

    docs, dup = {}, set()
    if prose:
        import difflib
        for m, t in prose.items():
            if not t or len(t) < 50:
                continue
            t = re.sub(r"```.*?```", "", t, flags=re.S)
            t = re.sub(r"[A-Za-z_]*" + re.escape(m) + r"[A-Za-z_]*", "MOD", t)
            docs[m] = re.sub(r"\s+", " ", t).strip().lower()
        keys = sorted(docs)
        for i, a in enumerate(keys):
            for b in keys[i + 1:]:
                if difflib.SequenceMatcher(None, docs[a], docs[b]).ratio() > 0.85:
                    dup.add(a); dup.add(b)
    return classify(mods, inc, dup, docs), comps
