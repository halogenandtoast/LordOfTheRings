#!/usr/bin/env python3
"""Name the card images by the engine's card codes, in one directory.

usage: tools/card-images.py [--dry-run]

The importer delivers frontend/public/data/cards.json and its renditions under
frontend/public/images/browse (and browse/thumb), named by the database's codes
(core-set-001.avif). The game names cards by RingsDB codes (01001), so this moves each
rendition to frontend/public/images/cards (and cards/thumb) under the engine's code and
rewrites cards.json to match. Run it again after each import.

A card's engine code is its RingsDB code, or for an encounter card its pack's RingsDB
prefix and its number (Flies and Spiders, core-set-119, is 01119). Other faces add
-b, -c, ...; printed variants add their name (-errata). The first printing of a code owns
it: later printings of the same card share its file, and a different card whose code is
taken keeps its database code.
"""
import json
import re
import shutil
import sys
from pathlib import Path

PUBLIC = Path(__file__).resolve().parent.parent / "frontend" / "public"
INDEX = PUBLIC / "data" / "cards.json"
BROWSE = PUBLIC / "images" / "browse"
CARDS = PUBLIC / "images" / "cards"


def slug(s):
    return re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")


def main():
    index = json.loads(INDEX.read_text())
    order = {p["code"]: (p.get("cycleOrder") or 0, p.get("order") or 0) for p in index["packs"]}
    prefix = {}
    for card in index["cards"]:
        code = card.get("ringsdbCode") or ""
        if code[-3:].isdigit():
            prefix.setdefault(card["packCode"], code[:-3])

    def engine(card):
        if card.get("ringsdbCode"):
            return card["ringsdbCode"]
        if card["section"] == "nightmare" or card["packCode"] not in prefix or card.get("number") is None:
            return card["code"]
        return prefix[card["packCode"]] + "%03d" % card["number"]

    def suffix(image):
        face = "" if image["face"] == "a" else "-" + image["face"]
        return face + ("-" + slug(image["variant"]) if image.get("variant") else "")

    # first printings first, cards with a RingsDB code before codes worked out from a number
    cards = sorted(index["cards"], key=lambda c: (order.get(c["packCode"], (99, 99)), not c.get("ringsdbCode"), c["code"]))
    owner = {}  # name -> the card name holding it
    renames = {}  # old file -> new file
    for card in cards:
        base = engine(card)
        if any(owner.get(base + suffix(i), card["name"]) != card["name"] for i in card["images"]):
            base = card["code"]
        for image in card["images"]:
            name = base + suffix(image)
            owner.setdefault(name, card["name"])
            renames.setdefault(image["file"], name + ".avif")

    for card in index["cards"]:
        for image in card["images"]:
            image["file"] = renames.get(image["file"], image["file"])

    if "--dry-run" in sys.argv:
        print(f"{len(set(renames.values()))} images from {len(renames)} renditions")
        return
    moved = 0
    for sub in ["", "thumb"]:
        (CARDS / sub).mkdir(parents=True, exist_ok=True)
        taken = set()
        for old, new in renames.items():
            src = BROWSE / sub / old
            # the first printing's rendition is the one kept
            if src.exists() and new not in taken:
                shutil.move(src, CARDS / sub / new)
                taken.add(new)
                moved += 1
    # renditions of later printings that share a file are not needed
    leftovers = [p for p in BROWSE.rglob("*.avif")]
    for p in leftovers:
        p.unlink()
    for d in sorted(BROWSE.rglob("*"), reverse=True) + [BROWSE]:
        if d.is_dir() and not any(d.iterdir()):
            d.rmdir()

    INDEX.write_text(json.dumps(index, ensure_ascii=False, separators=(",", ":")))
    print(f"moved {moved} files, dropped {len(leftovers)} duplicate printings, {len(set(renames.values()))} images")


if __name__ == "__main__":
    main()
