#!/usr/bin/env python3
"""Import The Dark of Mirkwood card metadata and scans from Hall of Beorn.

Run from any directory with Python 3 and ImageMagick's `magick` on PATH.
The images follow the existing convention: retained locally, excluded from Git.
Hall's detail records for 002/007/035 carry localized type labels and scan paths;
use the printed English types and canonical English scans for those records.
"""
import re,json,html,urllib.request,urllib.parse,concurrent.futures,subprocess,os,time,unicodedata
from pathlib import Path
BASE='https://hallofbeorn.com'
ROOT=Path(__file__).resolve().parent.parent/'frontend/public'
CACHE=Path('/tmp/dark-mirkwood-import');CACHE.mkdir(exist_ok=True)
def fetch(url):
 for attempt in range(4):
  try:return urllib.request.urlopen(url,timeout=40).read()
  except Exception:
   if attempt==3:raise RuntimeError(url)
   time.sleep(0.4)
def clean(s):
 s=re.sub(r'<img\b[^>]*class=[\'\"]icon-([^\'\"]+)[\'\"][^>]*>',lambda m:'['+m[1].lower()+']',s)
 s=re.sub(r'<img\b[^>]*title=[\'\"]([^\'\"]+)[\'\"][^>]*>',lambda m:'['+m[1].lower()+']',s)
 return html.unescape(re.sub('<[^>]+>','',s)).strip().replace('\xa0',' ')
def rich(s):
 s=re.sub(r'<img\b[^>]*class=[\'\"]icon-([^\'\"]+)[\'\"][^>]*>',lambda m:'['+m[1].lower()+']',s)
 s=re.sub(r'<img\b[^>]*title=[\'\"]([^\'\"]+)[\'\"][^>]*>',lambda m:'['+m[1].lower()+']',s)
 return html.unescape(re.sub(r'</?(?!b\b|i\b)[a-zA-Z][^>]*>','',s)).strip().replace('Gret Spider','Great Spider').replace('vitory display','victory display')
def val(s):
 s=clean(s)
 return int(s) if s.isdigit() else 'X' if s=='X' else None
product=fetch(BASE+'/LotR/Products/The-Dark-of-Mirkwood?View=Browse').decode()
links=list(dict.fromkeys(re.findall(r'/LotR/Details/[^\" ]+-TDoM',product)))
index=json.loads((ROOT/'data/cards.json').read_text())
def task(link):
 slug=link.split('/')[-1];cp=CACHE/(slug+'.html')
 if not cp.exists():cp.write_bytes(fetch(BASE+link))
 s=cp.read_text();number,quantity=map(int,re.search(r'#(\d+) \(x(\d+)',s).groups());code=f'dark-of-mirkwood-{number:03}'
 name=clean(re.search(r'font-size:1.75em;font-family:Verdana;[^>]*>(.*?)</div>',s,re.S)[1])
 typename=clean(re.search(r'font-weight:bold;font-size:20pt;[^>]*>(.*?)</div>',s,re.S)[1]);typ=typename.lower().replace(' ','-')
 if number in [2,7]:typ='quest';typename='Quest'
 if number==35:typ='location';typename='Location'
 header=s[s.index('<div class="statHeaderBox">'):].split('<div class="extraBox">')[0];statbox=header.split('class="statTextBox')[0]
 sphereMatch=re.search('font-weight:bold;font-size:16pt;[^>]*>(.*?)</div>',s,re.S)
 sphere=clean(sphereMatch[1]).lower() if sphereMatch else None
 encounter=re.search(r'<a title="([^"]+)" href="/LotR\?EncounterSet=',statbox)
 encounter=html.unescape(encounter[1]) if encounter else None
 traits=[clean(x).rstrip('.') for x in re.findall(r'<a title="Trait Search"[^>]*>(.*?)</a>',header,re.S)]
 if number==35:traits=['Underground','Dark']
 stats={k:None for k in ['cost','threatCost','willpower','attack','defense','health','threat','engagementCost','questPoints','victoryPoints','stage']}
 for value,label in re.findall(r'<span[^>]*>([^<]+)</span><img title="([^"]+)"',statbox):
  key={'Threat':'threat','Attack':'attack','Defense':'defense','Hit Points':'health','Willpower':'willpower'}.get(label)
  if key:stats[key]=val(value)
 valuebox=re.search(r'class="statValueBox"(.*?)class="statTextBox',header,re.S)[1]
 textval=clean(valuebox)
 if typ=='enemy':
  m=re.search(r'\(\s*(\d+|X|-)\s*\)',textval);stats['engagementCost']=val(m[1]) if m else None
 if typ in ['attachment','event']:
  m=re.search(r'\[\s*(\d+|X|-)\s*\]',textval);stats['cost']=val(m[1]) if m else None
 if typ=='location':
  m=re.search(r'\(\s*(\d+|X|-)\s*\)',textval);stats['questPoints']=val(m[1]) if m else None
 if typ=='quest':
  m=re.search(r'(\d+)\s*A-B',textval);stats['stage']=int(m[1]) if m else None
  m=re.search(r'\(\s*(\d+|X|-)\s*\)',textval);stats['questPoints']=val(m[1]) if m else None
 victory=re.search(r"title='Victory: (\d+)'",header)
 if victory:stats['victoryPoints']=int(victory[1])
 textblocks=re.split(r'<div class="statTextBox[^>]*>',header)[1:]
 def paras(block,kind):return chr(10).join(rich(x) for x in re.findall(r'<p class=[\'\"]'+kind+'[\'\"]>(.*?)</p>',block,re.S)) or None
 texts=[paras(b,'main-text') for b in textblocks];flavors=[paras(b,'flavor-text') for b in textblocks]
 shadow=next((paras(b,'shadow-text') for b in textblocks if paras(b,'shadow-text')),None)
 keywordtexts=re.findall(r"title='Search: ([^']+) Keyword'",header)
 keywords=[html.unescape(k).rstrip('.') for k in keywordtexts]
 if sphere in ['boon','burden']:traits.append(sphere.capitalize())
 imageurls=list(dict.fromkeys('/Images/Cards/The-Dark-of-Mirkwood/'+p.split('/')[-1] for p in re.findall(r'data-src="([^"]+)"',s)))
 images=[]
 for i,path in enumerate(imageurls):
  face=chr(97+i);dest=f'{code}{"" if i==0 else "-"+face}.avif'
  jpg=CACHE/(dest+'.jpg')
  if not jpg.exists():jpg.write_bytes(fetch(BASE+urllib.parse.quote(unicodedata.normalize('NFKD',path).encode('ascii','ignore').decode(),safe='/')))
  for thumb in [False,True]:
   out=ROOT/'images/cards'/('thumb' if thumb else '')/dest
   if not out.exists():
    args=['magick',str(jpg)]+(['-resize','160x224>'] if thumb else ['-resize','1000x1000>'])+['-quality','65',str(out)]
    subprocess.run(args,check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
  images.append({'face':face,'variant':None,'stage':stats['stage'],'orientation':'landscape' if typ in ['quest','campaign'] else 'portrait','source':BASE+path,'file':dest})
 back=None
 if len(texts)>1:
  back={'name':name,'type':typ,'traits':[],'text':texts[1],**stats}
 card={'code':code,'name':name,'section':'player' if typ in ['attachment','event'] else 'quest' if typ in ['quest','campaign'] else 'encounter','number':number,'setCode':'dark-of-mirkwood','setName':'The Dark of Mirkwood','packCode':'dark-of-mirkwood','packName':'The Dark of Mirkwood','cycleCode':'dark-of-mirkwood','cycleName':'The Dark of Mirkwood','encounterSet':encounter,'type':typ,'typeName':typename,'sphere':sphere,'traits':list(dict.fromkeys(traits)),'keywords':list(dict.fromkeys(keywords)),'unique':'unique-card.png' in s,**stats,'text':texts[0] if texts else None,'shadow':shadow,'setupText':None,'flavor':flavors[0] if flavors else None,'quantity':quantity,'deckLimit':1 if typ in ['attachment','event'] else None,'illustrator':clean(re.search(r'>Illus\.</span>\s*<a[^>]*>(.*?)</a>',s,re.S)[1]) if re.search(r'>Illus\.</span>\s*<a[^>]*>(.*?)</a>',s,re.S) else None,'hasErrata':False,'back':back,'isBackOf':None,'octgnId':None,'ringsdbCode':None,'ringsdbUrl':None,'images':images,'orientation':'landscape' if typ in ['quest','campaign'] else 'portrait'}

 return card
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
 cards=sorted(list(pool.map(task,links)),key=lambda c:c['number'])
assert len(cards)==51,(len(cards),links)
assert sum(c['quantity'] for c in cards)==80
index['cards']=[c for c in index['cards'] if c['packCode']!='dark-of-mirkwood']+cards
index['cycles']=[c for c in index['cycles'] if c['code']!='dark-of-mirkwood']+[{'code':'dark-of-mirkwood','name':'The Dark of Mirkwood','order':24,'packCodes':['dark-of-mirkwood']}]
index['packs']=[c for c in index['packs'] if c['code']!='dark-of-mirkwood']+[{'code':'dark-of-mirkwood','name':'The Dark of Mirkwood','order':1,'cycleCode':'dark-of-mirkwood','cycleName':'The Dark of Mirkwood','cycleOrder':24,'cardCount':51}]
(ROOT/'data/cards.json').write_text(json.dumps(index,ensure_ascii=False,separators=(',',':')))
print('Imported',len(cards),'cards;',sum(len(c['images']) for c in cards),'image faces')
