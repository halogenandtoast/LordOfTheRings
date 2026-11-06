#!/usr/bin/env python3
"""Refresh standalone product metadata from Hall of Beorn and supplied printed scans.

Run with --local-dir pointing to the Print on Demand scan directory.
Existing artwork follows the repository local/ignored asset convention.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import html
import json
from pathlib import Path
import re
import subprocess
import time
import urllib.parse
import urllib.request

ROOT = Path(__file__).resolve().parent.parent / 'frontend/public'
PACK = ''
TITLE = ''
BASE = 'https://hallofbeorn.com'
CACHE = Path('/tmp/lotr-standalone-import')
CACHE.mkdir(exist_ok=True)


def fetch(url):
    for attempt in range(4):
        try:
            return urllib.request.urlopen(url, timeout=40).read()
        except Exception:
            if attempt == 3:
                raise
            time.sleep(0.5)


def icon(match):
    tag = match[0]
    source = re.search(r'src=[\'"]([^\'"]+)', tag)
    if not source:
        return ''
    name = Path(source[1]).stem.removesuffix('-med').removesuffix('-small')
    return '' if name == 'unique-card' else '[' + name + ']'


def normalize_text(value):
    value = value.replace('Randomly select 1 of the 5 remaining UNRECOGNIZED TYPE: trait-searchkeyword', 'Randomly select 1 of the 5 remaining Loot objectives and place it facedown under Stalking the Ruins (without looking). Shuffle the others into the encounter deck (without looking at them).')
    value = value.replace('Book of Aul‰', 'Book of Aulë').replace('advace to stage', 'advance to stage')
    value = value.replace('the fist player claims', 'the first player claims')
    value = value.replace('place 1 progress token on Stalking the Ruins', 'place 1 resource on Stalking the Ruins')
    value = value.replace('beginning of the ropund', 'beginning of the round')
    if 'Driven by Fear' in value:
        value = value.replace('first player attached', 'first player attaches').replace("Treat attacked character's", "Treat attached character's")
    value = value.replace('attacks and Oathbreaker that enemy', 'attacks an Oathbreaker, that enemy').replace('insread.', 'instead.')
    return value.replace('Nazg√l', 'Nazgûl').replace('Rozbójnik', 'Brigand').replace('Créature', 'Creature').replace('Mort-vivant', 'Undead').replace('[PerPlayer]', '[per-player]').replace('Wiched Willow', 'Wicked Willow').replace('2..', '2.').replace('Doomed 3. instead', 'Doomed 3 instead').replace("Tarlang's Next", "Tarlang's Neck").replace('must engaged all enemies', 'must engage all enemies').replace('Exhuast a hero', 'Exhaust a hero').replace('The Stone of Erech is that active location', 'The Stone of Erech is the active location')


def clean(value, rich=False):
    value = re.sub(r'<img\b[^>]*>', icon, value)
    value = re.sub(r'<br\s*/?>', '\n', value)
    value = re.sub(r'</?(?!b\b|i\b)[a-zA-Z][^>]*>' if rich else r'<[^>]+>', '', value)
    return normalize_text(html.unescape(value).replace('\xa0', ' ').strip())


def number(value):
    value = clean(value)
    return int(value) if value.isdigit() else 'X' if value in ('X', '*') else None


def printed_keywords(text):
    keywords = []
    pattern = r'^(Surge|Doomed \d+|Burn \d+|Spectral|Guarded|Indestructible|Battle|Siege|Archery \d+|Time \d+|Loot|Restricted|Discover \d+)\.\s*'
    while match := re.match(pattern, text, re.I):
        keywords.append(match[1])
        text = text[match.end():]
    return keywords


def import_page(link, old, local):
    slug = link.rsplit('/', 1)[-1]
    cached = CACHE / (urllib.parse.quote(slug, safe='') + '.html')
    if not cached.exists():
        cached.write_bytes(fetch(BASE + urllib.parse.quote(link, safe="/'-")))
    page = cached.read_text()
    printed, face, quantity = re.search(r'#(\d+)([ab]?) \(x(\d+)', page).groups()
    printed, quantity = int(printed), int(quantity)
    code = f'{PACK}-{printed:03}' + ('-b' if face == 'b' else '')
    base_code = f'{PACK}-{printed:03}'
    card = dict.fromkeys(('sphere', 'deckLimit', 'illustrator', 'octgnId', 'ringsdbCode', 'ringsdbUrl'))
    card.update(old.get(base_code, {}))
    card['hasErrata'] = False
    name = clean(re.search(r'font-size:1.75em;font-family:Verdana;[^>]*>(.*?)</div>', page, re.S)[1])
    type_name = clean(re.search(r'font-weight:bold;font-size:20pt;[^>]*>(.*?)</div>', page, re.S)[1])
    typ = type_name.lower().replace('ship-', '').replace(' ', '-')
    typ = {'lieu': 'location', 'ort': 'location', 'ennemi': 'enemy'}.get(typ, typ)
    if typ in ('location', 'enemy'):
        type_name = typ.title()
    if typ not in {'enemy', 'location', 'quest', 'treachery', 'objective', 'objective-ally', 'attachment', 'event', 'ally', 'campaign'}:
        typ = old[base_code]['type']
        type_name = old[base_code]['typeName']
    header = page[page.index('<div class="statHeaderBox">'):].split('<div class="extraBox">')[0]
    value_box = re.search(r'class="statValueBox"(.*?)class="statTextBox', header, re.S)[1]
    stats = {key: None for key in ('cost', 'threatCost', 'willpower', 'attack', 'defense', 'health', 'threat', 'engagementCost', 'questPoints', 'victoryPoints', 'stage')}
    for value, symbol in re.findall(r'<span[^>]*>([^<]+)</span><img[^>]*src="/Images/([^"/]+)-med.png"', value_box):
        key = {'willpower': 'willpower', 'attack': 'attack', 'defense': 'defense', 'heart': 'health', 'threat': 'threat'}.get(symbol)
        if key:
            stats[key] = number(value)
    parentheses = re.search(r'\(\s*([\dX*]+|-)\s*\)', clean(value_box))
    if parentheses and typ in ('enemy', 'location', 'quest'):
        stats[{'enemy': 'engagementCost', 'location': 'questPoints', 'quest': 'questPoints'}[typ]] = number(parentheses[1])
    if typ in ('attachment', 'event', 'ally'):
        cost = re.search(r'\[\s*([\dX]+)\s*\]', clean(value_box))
        stats['cost'] = number(cost[1]) if cost else old.get(base_code, {}).get('cost')
    if typ == 'quest':
        stage = re.search(r'(\d+)\s*[A-Z]-[A-Z]', clean(value_box))
        stats['stage'] = int(stage[1]) if stage else None
    victory = re.search(r"title=['\"]Victory: (\d+)", header)
    stats['victoryPoints'] = int(victory[1]) if victory else None
    traits = [clean(t).rstrip('.') for t in re.findall(r'<a title="Trait Search"[^>]*>(.*?)</a>', header, re.S)]
    blocks = re.split(r'<div class="statTextBox[^>]*>', header)[1:]

    def paragraphs(block, kind):
        if kind == 'main-text':
            block = re.sub(r'<blockquote>(.*?)</blockquote>', r"<p class='main-text'>\1</p>", block, flags=re.S)
        return '\n'.join(clean(p, rich=True) for p in re.findall(r'<p class=[\'"]' + kind + r'[\'"]>(.*?)</p>', block, re.S)) or None

    texts = [paragraphs(block, 'main-text') for block in blocks]
    shadow = next((paragraphs(block, 'shadow-text') for block in blocks if paragraphs(block, 'shadow-text')), None)
    main_text = ''.join(re.findall(r"<p class=['\"]main-text['\"]>(.*?)</p>", blocks[0], re.S)) if blocks else ''
    # Only unconditional printed keywords belong in this field. A keyword
    # mentioned inside a conditional effect is not active on the card itself.
    keywords = printed_keywords(clean(main_text))
    image_urls = list(dict.fromkeys(re.findall(r'data-src="([^"]+)"', page)))
    images = []
    for index, image_url in enumerate(image_urls):
        side = 'b' if face == 'b' or index else 'a'
        filename = base_code + ('-b' if side == 'b' else '') + '.avif'
        candidates = sorted(local.rglob(f'{printed:03}{side} - *.jpg')) if local else []
        if not candidates and local and typ == 'quest':
            candidates = sorted(local.rglob(f'{printed:03} - *{side.upper()} - *.jpg'))
            if not candidates and side == 'a' and 2 <= printed <= 5 and PACK == 'the-old-forest':
                candidates = sorted(local.rglob('002 - 2A - *.jpg'))
        if not candidates and local:
            candidates = sorted(p for p in local.rglob(f'{printed:03} - *.jpg') if 'Reverse' not in p.name)
            if side == 'b':
                candidates = sorted(local.rglob(f'{printed:03} - * (Reverse).jpg'))
        if not candidates:
            existing = ROOT / 'images/cards' / filename
            if existing.exists():
                candidates = [existing]
        source = candidates[0] if candidates else CACHE / (filename + '.jpg')
        if not source.exists():
            source.write_bytes(fetch(urllib.parse.urljoin(BASE, urllib.parse.quote(image_url, safe="/:'-"))))
        for thumb in (False, True):
            target = ROOT / 'images/cards' / ('thumb' if thumb else '') / filename
            target.parent.mkdir(parents=True, exist_ok=True)
            if not target.exists():
                subprocess.run(['magick', str(source), '-resize', '160x224>' if thumb else '1000x1000>', '-quality', '65', str(target)], check=True, stderr=subprocess.DEVNULL)
        images.append({'face': side, 'variant': None, 'stage': stats['stage'], 'orientation': 'landscape' if typ == 'quest' else 'portrait', 'source': urllib.parse.urljoin(BASE, image_url), 'file': filename})
    sphere_match = re.search(r'font-weight:bold;font-size:16pt;[^>]*>(.*?)</div>', page, re.S)
    if sphere_match:
        card['sphere'] = clean(sphere_match[1]).lower()
    card.update(code=code, name=name, number=printed, type=typ, typeName=type_name, traits=traits, keywords=keywords, unique='unique-card.png' in page, quantity=0 if face == 'b' else quantity, text=texts[0] if texts else None, shadow=shadow, setupText=None, flavor=paragraphs(blocks[0], 'flavor-text') if blocks else None, back=None, isBackOf=base_code if face == 'b' else None, images=images, orientation='landscape' if typ == 'quest' else 'portrait', **stats)
    card.update(section='player' if typ in ('attachment', 'event', 'ally') and card['sphere'] else 'quest' if typ in ('quest', 'setup') else 'encounter', setCode=PACK, setName=TITLE, packCode=PACK, packName=TITLE, cycleCode='print-on-demand', cycleName='Print on Demand', encounterSet=None if typ == 'contract' else TITLE)
    if len(texts) > 1:
        card['back'] = dict(name=name, type=typ, traits=[], text=texts[1], **stats)
    return card


# Product suffixes and quantities are checked against the product lists, not the
# pre-existing OCR records. Quest reverse faces are part of their front record.
PRODUCTS = [
    ('the-massing-at-osgiliath', 'The Massing at Osgiliath', 'The-Massing-at-Osgiliath-GenCon-2011', 'TMaO', '01', 19, 45),
    ('the-battle-of-lake-town', 'The Battle of Lake-town', 'The-Battle-of-Lake-town-GenCon-2012', 'TBoLT', '03', 24, 49),
    ('the-stone-of-erech', 'The Stone of Erech', 'The-Stone-of-Erech-GenCon-2013', 'TSoE', '02', 27, 48),
    ('the-old-forest', 'The Old Forest', 'The-Old-Forest', 'TOF', '05', 24, 48),
    ('the-ruins-of-belegost', 'The Ruins of Belegost', 'The-Ruins-of-Belegost', 'TRoB', '06', 30, 52),
    ('fog-on-the-barrow-downs', 'Fog on the Barrow-downs', 'Fog-on-the-Barrow-downs', 'FotBD', '04', 21, 50),
    ('murder-at-the-prancing-pony', 'Murder at the Prancing Pony', 'Murder-at-the-Prancing-Pony', 'MatPP', '07', 29, 53),
    ('the-siege-of-annuminas', 'The Siege of Annúminas', 'The-Siege-of-Annuminas', 'TSoA', '08', 32, 57),
    ('attack-on-dol-guldur', 'Attack on Dol Guldur', 'Attack-on-Dol-Guldur', 'AoDG', '09', 32, 55),
]


def ensure_product_assets(pack):
    # Enabled standalones use their printed symbol for both icons.
    enabled = {
        'the-massing-at-osgiliath': ('MEC15', 'the_massing_at_osgiliath'),
        'the-battle-of-lake-town': ('MEC35', 'the_battle_of_lake_town'),
        'the-stone-of-erech': ('MEC33', 'the_stone_of_erech'),
        'the-old-forest': ('MEC37', 'the_old_forest'),
        'the-ruins-of-belegost': ('OP014', 'the_ruins_of_belegost'),
    }
    if pack not in enabled:
        return
    product, icon = enabled[pack]
    cover = ROOT / f'images/products/{product}.jpg'
    if not cover.exists():
        cover.parent.mkdir(parents=True, exist_ok=True)
        cover.write_bytes(fetch(f'https://hallofbeorn-resources.s3.amazonaws.com/Images/Products/{product}.jpg'))
    symbol = 'https://raw.githubusercontent.com/KevBelisle/lotr-lcg-assets/HEAD/' + urllib.parse.quote(f'icons/expansion symbols/pod/{icon}.svg')
    svg = None
    for kind in ('expansions', 'scenarios'):
        target = ROOT / 'images/icons' / kind / (pack + '.svg')
        if not target.exists():
            if svg is None:
                svg = fetch(symbol)
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(svg)


def main():
    global PACK, TITLE
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--local-dir', type=Path, required=True)
    parser.add_argument('--pack', choices=[p[0] for p in PRODUCTS])
    args = parser.parse_args()
    index = json.loads((ROOT / 'data/cards.json').read_text())
    for pack, title, product, suffix, folder, count, physical in PRODUCTS:
        if args.pack and args.pack != pack:
            continue
        PACK, TITLE = pack, title
        ensure_product_assets(pack)
        local = next(args.local_dir.glob(folder + ' - *'))
        product_cache = CACHE / (product + '.product.html')
        if not product_cache.exists():
            product_cache.write_bytes(fetch(BASE + '/LotR/Products/' + product + '?View=Browse'))
        product_page = product_cache.read_text()
        links = list(dict.fromkeys(html.unescape(link) for link in re.findall(r'/LotR/Details/[^"\s<>]+', product_page)))
        # Product lists include promotional alternate-art core heroes. Their
        # printed numbers collide with the scenario cards and they already
        # exist in the core set; retain the standalone scenario contents here.
        promos = {html.unescape(link) for line in product_page.splitlines() if '(Hero)' in line for link in re.findall(r'/LotR/Details/[^"\s<>]+', line)}
        links = [link for link in links if link not in promos]
        old = {c['code']: c for c in index['cards'] if c['packCode'] == pack}
        with ThreadPoolExecutor(max_workers=4) as pool:
            cards = sorted(pool.map(lambda link: import_page(link, old, local), links), key=lambda c: (c['number'], bool(c['isBackOf'])))
        if count is not None:
            assert len(cards) == count, (pack, len(cards), count)
        if physical is not None:
            assert sum(c['quantity'] for c in cards) == physical, (pack, sum(c['quantity'] for c in cards))
        assert cards and len({c['code'] for c in cards}) == len(cards), pack
        index['cards'] = [c for c in index['cards'] if c['packCode'] != pack] + cards
        (ROOT / 'data/cards.json').write_text(json.dumps(index, ensure_ascii=False, separators=(',', ':')))
        print(f"{pack}: {len(cards)} records, {sum(c['quantity'] for c in cards)} physical cards", flush=True)


if __name__ == '__main__':
    main()
