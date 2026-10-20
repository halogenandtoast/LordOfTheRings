#!/usr/bin/env python3
"""Generate CardDef modules from a cards.json export (ringsdb format).

usage: tools/generate-card-defs.py path/to/cards.json

Writes src/LordOfTheRings/{Trait,EncounterSet}.hs and <Type>/CardDefs.hs.
Card behaviour lives elsewhere; these modules are data only.
"""
import json
import re
import sys
import unicodedata
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent / "src" / "LordOfTheRings"

# Same-named cards need distinct identifiers.
NAME_OVERRIDES = {
    "01121": "aChosenPathDontLeaveThePath",
    "01122": "aChosenPathBeornsPath",
}

TYPE_MODULES = {
    "hero": "Hero",
    "ally": "Ally",
    "attachment": "Attachment",
    "event": "Event",
    "enemy": "Enemy",
    "location": "Location",
    "treachery": "Treachery",
    "quest": "Quest",
    "objective": "Objective",
    "objective-ally": "Objective",
}

TEXT_KEYWORDS = ("Sentinel", "Ranged", "Restricted", "Surge", "Guarded")

# copies kept in easy mode, by pack name and card number; see tools/extract-easy-mode.py
EASY_MODE = json.loads((Path(__file__).resolve().parent / "easy-mode.json").read_text())


def ascii_fold(s):
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()


def words(s):
    return re.findall(r"[A-Za-z0-9]+", ascii_fold(s).replace("'", "").replace("`", ""))


def ident(card):
    if card["code"] in NAME_OVERRIDES:
        return NAME_OVERRIDES[card["code"]]
    ws = words(card["name"])
    name = ws[0].lower() + "".join(w[:1].upper() + w[1:] for w in ws[1:])
    return name if name[0].isalpha() else "card" + name


def con(s):
    return "".join(w[:1].upper() + w[1:] for w in words(s))


def hs_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def cost(card):
    c = card.get("cost")
    return "DynamicCost" if c == "X" else f"(StaticCost {int(c or 0)})"


def stat(card, key):
    v = card.get(key)
    return int(v) if v not in (None, "", "X") else 0


def keywords(card):
    found, victory = set(), None
    parts = re.split(r"[.;]", card.get("keywords") or "")
    first_line = (card.get("text") or "").split("\n")[0]
    parts += [p for p in re.split(r"[.;]", first_line) if p.strip() in TEXT_KEYWORDS]
    for part in (p.strip() for p in parts):
        if not part:
            continue
        if m := re.fullmatch(r"Victory (\d+)", part):
            victory = int(m.group(1))
        elif m := re.fullmatch(r"Doomed (\d+)", part):
            found.add(f"Doomed {m.group(1)}")
        elif part in TEXT_KEYWORDS:
            found.add(part)
    return sorted(found), victory


def traits(card):
    return sorted({con(t) for t in (card.get("traits") or "").split(".") if t.strip()})


def sphere(card):
    return (card.get("sphere_code") or "neutral").capitalize()


def encounter_set(card):
    return con(card["encounter_set"])


def stage(card):
    m = re.match(r"\d+", str(card.get("stage") or "1"))
    return int(m.group(0))


def base(card):
    t, code, name = card["type_code"], hs_string(card["code"]), hs_string(card["name"])
    if t == "hero":
        stats = " ".join(str(stat(card, k)) for k in ("threat", "willpower", "attack", "defense", "health"))
        return f"hero {code} {name} {sphere(card)} {stats}"
    if t == "ally":
        stats = " ".join(str(stat(card, k)) for k in ("willpower", "attack", "defense", "health"))
        return f"ally {code} {name} {sphere(card)} {cost(card)} {stats}"
    if t in ("attachment", "event"):
        return f"{t} {code} {name} {sphere(card)} {cost(card)}"
    qty = card.get("quantity") or 1
    es = encounter_set(card)
    if t == "enemy":
        stats = " ".join(str(stat(card, k)) for k in ("engagement_cost", "threat", "attack", "defense", "health"))
        return f"enemy {code} {name} {es} {qty} {stats}"
    if t == "location":
        return f"location {code} {name} {es} {qty} {stat(card, 'threat')} {stat(card, 'quest_points')}"
    if t == "treachery":
        return f"treachery {code} {name} {es} {qty}"
    if t == "quest":
        return f"quest {code} {name} {es} {stage(card)} {stat(card, 'quest_points')}"
    if t == "objective":
        return f"objective {code} {name} {es} {qty}"
    if t == "objective-ally":
        stats = " ".join(str(stat(card, k)) for k in ("willpower", "attack", "defense", "health"))
        return (
            f"(objective {code} {name} {es} {qty})\n      {{ cdCardType = ObjectiveAllyType"
            + "".join(f", cd{f} = Just {v}" for f, v in zip(("Willpower", "Attack", "Defense", "HitPoints"), stats.split()))
            + " }"
        )
    raise ValueError(t)


def definition(card):
    kws, victory = keywords(card)
    updates = []
    if card.get("is_unique") and card["type_code"] != "hero":
        updates.append("cdUnique = True")
    if ts := traits(card):
        updates.append(f"cdCardTraits = setFromList [{', '.join(ts)}]")
    if kws:
        updates.append(f"cdKeywords = setFromList [{', '.join(kws)}]")
    if victory:
        updates.append(f"cdVictoryPoints = Just {victory}")
    if card.get("shadow_text") or "Shadow:" in (card.get("text") or ""):
        updates.append("cdShadowEffect = True")
    if (kept := EASY_MODE.get(card.get("pack_name"), {}).get(str(card.get("position")))) is not None:
        updates.append(f"cdEasyModeQuantity = Just {kept}")
    expr = base(card)
    name = ident(card)
    if not updates:
        return f"{name} :: CardDef\n{name} = {expr}\n"
    body = "\n    , ".join(updates)
    return f"{name} :: CardDef\n{name} =\n  ({expr})\n    {{ {body}\n    }}\n"


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    print(f"wrote {path.relative_to(ROOT.parent.parent)}")


def enum_module(name, values):
    cons = "\n  | ".join(values)
    return (
        f"module LordOfTheRings.{name} (module LordOfTheRings.{name}) where\n\n"
        f"import LordOfTheRings.Prelude\n\n"
        f"data {name}\n  = {cons}\n"
        f"  deriving stock (Show, Eq, Ord, Generic)\n  deriving anyclass (ToJSON, FromJSON)\n"
    )


def main(path):
    cards = [c for c in json.loads(Path(path).read_text()) if c["type_code"] in TYPE_MODULES]
    write(ROOT / "Trait.hs", enum_module("Trait", sorted({t for c in cards for t in traits(c)})))
    sets = sorted({encounter_set(c) for c in cards if c.get("encounter_set")})
    write(ROOT / "EncounterSet.hs", enum_module("EncounterSet", sets))

    by_module = defaultdict(list)
    for card in cards:
        by_module[TYPE_MODULES[card["type_code"]]].append(card)

    for module, group in sorted(by_module.items()):
        group.sort(key=lambda c: c["code"])
        entries = "\n      , ".join(ident(c) for c in group)
        all_name = f"all{module}Cards"
        text = (
            f"module LordOfTheRings.{module}.CardDefs (module LordOfTheRings.{module}.CardDefs) where\n\n"
            "-- Generated by tools/generate-card-defs.py; edit the generator, not this file.\n\n"
            "import Data.Map.Strict qualified as Map\n"
            "import LordOfTheRings.Card.CardDef\n"
            "import LordOfTheRings.Card.CardType\n"
            "import LordOfTheRings.Card.Code\n"
            "import LordOfTheRings.EncounterSet\n"
            "import LordOfTheRings.Keyword\n"
            "import LordOfTheRings.Prelude\n"
            "import LordOfTheRings.Sphere\n"
            "import LordOfTheRings.Trait\n\n"
            f"{all_name} :: Map CardCode CardDef\n"
            f"{all_name} =\n  Map.fromList $\n    map\n      (\\def -> (cdCardCode def, def))\n      [ {entries}\n      ]\n\n"
            + "\n".join(definition(c) for c in group)
        )
        write(ROOT / module / "CardDefs.hs", text)


if __name__ == "__main__":
    main(sys.argv[1])
