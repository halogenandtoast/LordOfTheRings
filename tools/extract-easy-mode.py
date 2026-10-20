#!/usr/bin/env python3
"""Extract easy mode quantities from a Hall of Beorn checkout.

usage: tools/extract-easy-mode.py path/to/hall-of-beorn > tools/easy-mode.json

git clone https://github.com/danpoage/hall-of-beorn gives the checkout. Its set
definitions mark each encounter card that easy mode thins with .WithEasyModeQuantity(n),
the copies kept in easy mode; cards without it keep every copy. (Its JSON export reads
both as 0, so use the source.) Writes {pack name: {card number: copies kept}}.
"""
import json
import re
import sys
from collections import defaultdict
from pathlib import Path


def main(hob):
    easy = defaultdict(dict)
    for f in sorted((Path(hob) / "src/HallOfBeorn/Models/LotR/Sets").rglob("*.cs")):
        src = f.read_text(encoding="utf-8-sig")
        name = re.search(r'(?:Name|setName)\s*=\s*"([^"]+)"', src)
        if not name or "WithEasyModeQuantity" not in src:
            continue
        # each card is one add... chain
        for card in re.split(r"\badd\w*\(", src)[1:]:
            kept = re.search(r"\.WithEasyModeQuantity\((\d+)\)", card)
            info = re.search(r"\.WithInfo\((\d+),", card)
            if kept and info:
                easy[name[1]][int(info[1])] = int(kept[1])
    json.dump(easy, sys.stdout, ensure_ascii=False, indent=1, sort_keys=True)
    print()


if __name__ == "__main__":
    main(sys.argv[1])
