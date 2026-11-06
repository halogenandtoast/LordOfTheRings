#!/usr/bin/env python3
"""Import complete Dreadnaught metadata and artwork, including upgraded ship faces.

Requires Python 3 and ImageMagick. Optional --local-dir uses the supplied card scans;
otherwise artwork is downloaded from Hall of Beorn. Images follow the repository's
existing local/ignored asset convention.
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
PACK = 'the-hunt-for-the-dreadnaught'
TITLE = 'The Hunt for the Dreadnaught'
BASE = 'https://hallofbeorn.com'
CACHE = Path('/tmp/lotr-dreadnaught-import')
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
    # Correct transcription errors against the supplied printed cards.
    for old, new in (
        ('[PerPlayer]', '[per-player]'), ('\x07', '•'),
        ('loction', 'location'), ('hit pints', 'hit points'),
        ('eithe raise', 'either raise'), ('Bording', 'Boarding'),
        ('Boading', 'Boarding'), ('gaines', 'gains'), ('Tofalas', 'Tolfalas'),
        ('is gains:', 'it gains:'), ('sieze', 'seize'),
        ('player cards effects', 'player card effects'),
        ('gains +2 hit points', 'gets +2 hit points'),
        ('Each player assigns X among', 'Each player assigns X damage among'),
    ):
        value = value.replace(old, new)
    return value


def printed_text(printed, reverse, value):
    if not value:
        return value
    if printed == 2 and reverse:
        value = value.replace('group. (Group limit', 'group. Reveal the top card of the encounter deck. (Group limit')
    if printed == 7 and not reverse:
        value = value.replace('\nReduce your threat', '\n<b>Setup:</b> Reduce your threat')
    if printed == 13 and not reverse and 'Massive.' not in value:
        value = value.replace('Immune to player card effects.', 'Massive. Immune to player card effects.')
    return value


def clean(value, rich=False):
    value = re.sub(r'<img\b[^>]*>', icon, value)
    value = re.sub(r'<br\s*/?>', '\n', value)
    value = re.sub(r'</?(?!b\b|i\b)[a-zA-Z][^>]*>' if rich else r'<[^>]+>', '', value)
    return normalize_text(html.unescape(value).replace('\xa0', ' ').strip())


def number(value):
    value = clean(value)
    return int(value) if value.isdigit() else 'X' if value in ('X', '*') else None


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
    typ = type_name.lower().replace('ship-', '')
    if printed <= 3:
        typ, type_name = 'setup', 'Setup'
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
    if typ == 'quest':
        stage = re.search(r'(\d+)\s*A-B', clean(value_box))
        stats['stage'] = int(stage[1]) if stage else None
    victory = re.search(r"title=['\"]Victory: (\d+)", header)
    stats['victoryPoints'] = int(victory[1]) if victory else None
    traits = [clean(t).rstrip('.') for t in re.findall(r'<a title="Trait Search"[^>]*>(.*?)</a>', header, re.S)]
    blocks = re.split(r'<div class="statTextBox[^>]*>', header)[1:]

    def paragraphs(block, kind):
        return '\n'.join(clean(p, rich=True) for p in re.findall(r'<p class=[\'"]' + kind + r'[\'"]>(.*?)</p>', block, re.S)) or None

    texts = [printed_text(printed, face == 'b' or i > 0, paragraphs(block, 'main-text')) for i, block in enumerate(blocks)]
    shadow = next((paragraphs(block, 'shadow-text') for block in blocks if paragraphs(block, 'shadow-text')), None)
    main_text = ''.join(re.findall(r"<p class=['\"]main-text['\"]>(.*?)</p>", blocks[0], re.S)) if blocks else ''
    keywords = list(dict.fromkeys(html.unescape(k).rstrip('.') for k in re.findall(r"title='Search: ([^']+) Keyword'", main_text)))
    # The printed Elite face also has Massive; Hall's transcription omits it.
    if printed == 13 and 'Massive' not in keywords:
        keywords.append('Massive')
    image_urls = list(dict.fromkeys(re.findall(r'data-src="([^"]+)"', page)))
    images = []
    for index, image_url in enumerate(image_urls):
        side = 'b' if face == 'b' or index else 'a'
        filename = base_code + ('-b' if side == 'b' else '') + '.avif'
        candidates = sorted(local.rglob(f'{printed:03}{side} - *.jpg')) if local else []
        if not candidates and local:
            candidates = sorted(p for p in local.rglob(f'{printed:03} - *.jpg') if 'Reverse' not in p.name)
            if side == 'b':
                candidates = sorted(local.rglob(f'{printed:03} - * (Reverse).jpg'))
        source = candidates[0] if candidates else CACHE / (filename + '.jpg')
        if not source.exists():
            source.write_bytes(fetch(BASE + urllib.parse.quote(image_url, safe="/'-")))
        for thumb in (False, True):
            target = ROOT / 'images/cards' / ('thumb' if thumb else '') / filename
            target.parent.mkdir(parents=True, exist_ok=True)
            if not target.exists():
                subprocess.run(['magick', str(source), '-resize', '160x224>' if thumb else '1000x1000>', '-quality', '65', str(target)], check=True, stderr=subprocess.DEVNULL)
        images.append({'face': side, 'variant': None, 'stage': stats['stage'], 'orientation': 'landscape' if typ == 'quest' else 'portrait', 'source': BASE + image_url, 'file': filename})
    card.update(code=code, name=name, number=printed, type=typ, typeName=type_name, traits=traits, keywords=keywords, unique='unique-card.png' in page, quantity=0 if face == 'b' else quantity, text=texts[0] if texts else None, shadow=shadow, setupText=None, flavor=paragraphs(blocks[0], 'flavor-text') if blocks else None, back=None, isBackOf=base_code if face == 'b' else None, images=images, orientation='landscape' if typ == 'quest' else 'portrait', **stats)
    card.update(section='player' if typ == 'contract' else 'quest' if typ in ('quest', 'setup') else 'encounter', setCode=PACK, setName=TITLE, packCode=PACK, packName=TITLE, cycleCode=PACK, cycleName=TITLE, encounterSet=None if typ == 'contract' else TITLE)
    if len(texts) > 1:
        card['back'] = dict(name=name, type=typ, traits=[], text=texts[1], **stats)
    return card


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--local-dir', type=Path)
    args = parser.parse_args()
    cover = ROOT / 'images/products/MEC86.jpg'
    if not cover.exists():
        cover.parent.mkdir(parents=True, exist_ok=True)
        cover.write_bytes(fetch('https://hallofbeorn-resources.s3.amazonaws.com/Images/Products/MEC86.jpg'))
    index = json.loads((ROOT / 'data/cards.json').read_text())
    old = {c['code']: c for c in index['cards'] if c['packCode'] == PACK}
    scenario = fetch(BASE + '/LotR/Scenarios/The-Hunt-for-the-Dreadnaught').decode()
    links = list(dict.fromkeys(html.unescape(link) for link in re.findall(r'/LotR/Details/[^"\s]+-THftD', scenario)))
    links.append('/LotR/Details/A-Perilous-Voyage-THftD')
    with ThreadPoolExecutor(max_workers=4) as pool:
        cards = sorted(pool.map(lambda link: import_page(link, old, args.local_dir), links), key=lambda c: (c['number'], bool(c['isBackOf'])))
    assert len(cards) == 54, len(cards)
    assert sum(c['quantity'] for c in cards) == 79
    by_code = {c['code']: c for c in cards}
    for card in cards:
        if card['isBackOf']:
            parent = by_code[card['isBackOf']]
            parent['back'] = {key: value for key, value in card.items() if key not in ('images', 'back')}
            parent['images'] += card['images']
    index['cards'] = [c for c in index['cards'] if c['packCode'] != PACK] + cards
    for pack in index['packs']:
        if pack['code'] == PACK:
            pack['cardCount'] = 47
    (ROOT / 'data/cards.json').write_text(json.dumps(index, ensure_ascii=False, separators=(',', ':')))
    print(f'Imported {len(cards)} records, 79 physical cards, including all upgraded ships and setup cards')


if __name__ == '__main__':
    main()
