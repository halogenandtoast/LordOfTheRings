// The official campaigns: each deluxe box with its cycle of adventure packs, and the two
// Sagas. Scenario lists follow Hall of Beorn; box art lives in /images/products by code.
// Expansion symbols and encounter set icons come from KevBelisle/lotr-lcg-assets.

import type { Gateable } from './displayRules'

export interface ScenarioOption extends Gateable {
  id: string
  name: string
  image: string
  /** The scenario's encounter set icon. */
  icon: string
  /** The engine builds its Nightmare Deck, so it may be played in nightmare mode. */
  nightmare?: boolean
}

export interface Campaign {
  id: string
  name: string
  image: string
  /** The expansion symbol printed on its cards. */
  symbol: string
  scenarios: ScenarioOption[]
}

const product = (code: string) => `/images/products/${code}.jpg`

export const slug = (name: string) =>
  name
    .normalize('NFKD')
    .replace(/[^\w\s-]/g, '')
    .trim()
    .toLowerCase()
    .replace(/\s+/g, '-')

const scenario = (code: string, name: string): ScenarioOption => ({
  id: slug(name),
  name,
  image: product(code),
  icon: `/images/icons/scenarios/${slug(name)}.svg`,
})

// A box's own scenarios share its art; each adventure pack has its own.
const fromBox = (code: string, names: string[]): ScenarioOption[] => names.map((name) => scenario(code, name))

const fromPacks = (packs: [string, string][]): ScenarioOption[] => packs.map(([code, name]) => scenario(code, name))

// The scenarios the engine scripts, with how far along each is (see displayRules.ts):
// all but the core box's three are beta. The rest are unimplemented. `nightmare: true` once its Nightmare Deck is scripted.
const implemented: Record<string, Gateable & { nightmare?: boolean }> = {
  'passage-through-mirkwood': { nightmare: true },
  'journey-along-the-anduin': { nightmare: true },
  'escape-from-dol-guldur': { nightmare: true },
  'the-hunt-for-gollum': { beta: true, nightmare: true },
  'conflict-at-the-carrock': { beta: true, nightmare: true },
  'a-journey-to-rhosgobel': { beta: true, nightmare: true },
  'the-hills-of-emyn-muil': { beta: true, nightmare: true },
  'the-dead-marshes': { beta: true, nightmare: true },
  'return-to-mirkwood': { beta: true, nightmare: true },
  'into-the-pit': { beta: true, nightmare: true },
  'flight-from-moria': { beta: true, nightmare: true },
  'the-seventh-level': { beta: true, nightmare: true },
  'the-long-dark': { beta: true, nightmare: true },
  'road-to-rivendell': { beta: true, nightmare: true },
  'the-redhorn-gate': { beta: true, nightmare: true },
  'foundations-of-stone': { beta: true, nightmare: true },
  'shadow-and-flame': { beta: true, nightmare: true },
  'the-watcher-in-the-water': { beta: true, nightmare: true },
  'the-siege-of-cair-andros': { beta: true, nightmare: true },
  'into-ithilien': { beta: true, nightmare: true },
  'peril-in-pelargir': { beta: true, nightmare: true },
  'encounter-at-amon-din': { beta: true, nightmare: true },
  'the-druadan-forest': { beta: true, nightmare: true },
  'the-morgul-vale': { beta: true, nightmare: true },
  'the-stewards-fear': { beta: true, nightmare: true },
  'assault-on-osgiliath': { beta: true, nightmare: true },
  'the-blood-of-gondor': { beta: true, nightmare: true },
  'to-catch-an-orc': { beta: true, nightmare: true },
  'into-fangorn': { beta: true, nightmare: true },
  'the-fords-of-isen': { beta: true, nightmare: true },
  'the-antlered-crown': { beta: true, nightmare: true },
  'the-dread-realm': { beta: true, nightmare: true },
  'the-dunland-trap': { beta: true, nightmare: true },
  'trouble-in-tharbad': { beta: true, nightmare: true },
  'celebrimbors-secret': { beta: true, nightmare: true },
  'the-nin-in-eilph': { beta: true, nightmare: true },
  'the-three-trials': { beta: true, nightmare: true },
  'the-battle-of-carn-dum': { beta: true, nightmare: true },
  'the-wastes-of-eriador': { beta: true, nightmare: true },
  'deadmens-dike': { beta: true, nightmare: true },
  'intruders-in-chetwood': { beta: true, nightmare: true },
  'the-weather-hills': { beta: true, nightmare: true },
  'across-the-ettenmoors': { beta: true, nightmare: true },
  'the-treachery-of-rhudaur': { beta: true, nightmare: true },
  'escape-from-mount-gram': { beta: true, nightmare: true },
  'voyage-across-belegaer': { beta: true, nightmare: true },
  'the-fate-of-numenor': { beta: true, nightmare: true },
  'raid-on-the-grey-havens': { beta: true, nightmare: true },
  'flight-of-the-stormcaller': { beta: true, nightmare: true },
  'the-thing-in-the-depths': { beta: true, nightmare: true },
  'the-drowned-ruins': { beta: true, nightmare: true },
  'temple-of-the-deceived': { beta: true, nightmare: true },
  'a-storm-on-cobas-haven': { beta: true, nightmare: true },
  'the-city-of-corsairs': { beta: true, nightmare: true },
  'the-long-arm-of-mordor': { beta: true },
  'journey-up-the-anduin': { beta: true },
  'beneath-the-sands': { beta: true },
  'desert-crossing': { beta: true },
  'escape-from-umbar': { beta: true },
  'race-across-harad': { beta: true },
  'the-black-serpent': { beta: true },
  'the-dungeons-of-cirith-gurat': { beta: true },
  'the-mumakil': { beta: true },
  'the-crossings-of-poros': { beta: true },
  'the-fate-of-wilderland': { beta: true },
  'fire-in-the-night': { beta: true },
  'the-kings-quest': { beta: true },
  'the-ghost-of-framsburg': { beta: true },
  'mount-gundabad': { beta: true },
  'the-withered-heath': { beta: true },
  'roam-across-rhovanion': { beta: true },
  'lost-in-mirkwood': { beta: true },
  'the-river-running': { beta: true },
  'the-temple-of-doom': { beta: true },
  'the-city-of-ulfast': { beta: true },
  'under-the-ash-mountains': { beta: true },
  'danger-in-dorwinion': { beta: true },
  'wrath-and-ruin': { beta: true },
  'challenge-of-the-wainriders': { beta: true },
  'the-fortress-of-nurn': { beta: true },
  'the-land-of-sorrow': { beta: true },
  'the-passing-of-the-grey-company': { beta: true },
  'the-ring-goes-south': { beta: true },
  'the-passage-of-the-marshes': { beta: true },
  'flies-and-spiders': { beta: true },
  'the-battle-of-five-armies': { beta: true },
  'we-must-away-ere-break-of-day': { beta: true },
  'over-the-misty-mountains-grim': { beta: true },
  'dungeons-deep-and-caverns-dim': { beta: true },
  'the-lonely-mountain': { beta: true },
  'a-shadow-of-the-past': { beta: true },
  'flight-to-the-ford': { beta: true },
  'a-knife-in-the-dark': { beta: true },
  'the-battle-of-the-pelennor-fields': { beta: true },
  'the-siege-of-gondor': { beta: true },
  'the-tower-of-cirith-ungol': { beta: true },
  'the-black-gate-opens': { beta: true },
  'mount-doom': { beta: true },
  'journey-in-the-dark': { beta: true },
  'breaking-of-the-fellowship': { beta: true },
}

const campaign = (id: string, name: string, code: string, scenarios: ScenarioOption[]): Campaign => ({
  id,
  name,
  image: product(code),
  symbol: `/images/icons/expansions/${id}.svg`,
  scenarios: scenarios.map((s) => ({ ...s, ...(implemented[s.id] ?? { unimplemented: true }) })),
})

// The boxes the engine plays in campaign mode, by the engine's campaign id (see
// LordOfTheRings.Campaign): the Revised Core Set's Mirkwood Paths, Angmar Awakened, the
// Lost Realm's scenarios with its cycle's, and the Dream-chaser Campaign Expansion's nine
// scenarios, The Grey Havens' and the cycle's, one campaign.
export const campaignModes: Record<string, string> = {
  'core-set': 'revised-core',
  'angmar-awakened': 'angmar-awakened',
  'dream-chaser': 'dream-chaser',
  'the-hobbit': 'the-hobbit',
  'the-lord-of-the-rings': 'the-lord-of-the-rings',
}

export const campaigns: Campaign[] = [
  // the core set's cycle is Shadows of Mirkwood
  campaign('core-set', 'Core Set', 'MEC01', [
    ...fromBox('MEC01', ['Passage Through Mirkwood', 'Journey Along the Anduin', 'Escape from Dol Guldur']),
    ...fromPacks([
      ['MEC02', 'The Hunt for Gollum'],
      ['MEC03', 'Conflict at the Carrock'],
      ['MEC04', 'A Journey to Rhosgobel'],
      ['MEC05', 'The Hills of Emyn Muil'],
      ['MEC06', 'The Dead Marshes'],
      ['MEC07', 'Return to Mirkwood'],
    ]),
  ]),
  campaign('dwarrowdelf', 'Dwarrowdelf', 'MEC08', [
    ...fromBox('MEC08', ['Into the Pit', 'The Seventh Level', 'Flight from Moria']),
    ...fromPacks([
      ['MEC09', 'The Redhorn Gate'],
      ['MEC10', 'Road to Rivendell'],
      ['MEC11', 'The Watcher in the Water'],
      ['MEC12', 'The Long Dark'],
      ['MEC13', 'Foundations of Stone'],
      ['MEC14', 'Shadow and Flame'],
    ]),
  ]),
  campaign('against-the-shadow', 'Against the Shadow', 'MEC17', [
    ...fromBox('MEC17', ['Peril in Pelargir', 'Into Ithilien', 'The Siege of Cair Andros']),
    ...fromPacks([
      ['MEC18', "The Steward's Fear"],
      ['MEC19', 'The Drúadan Forest'],
      ['MEC20', 'Encounter at Amon Dîn'],
      ['MEC21', 'Assault on Osgiliath'],
      ['MEC22', 'The Blood of Gondor'],
      ['MEC23', 'The Morgul Vale'],
    ]),
  ]),
  campaign('ring-maker', 'The Ring-maker', 'MEC25', [
    ...fromBox('MEC25', ['The Fords of Isen', 'To Catch an Orc', 'Into Fangorn']),
    ...fromPacks([
      ['MEC26', 'The Dunland Trap'],
      ['MEC27', 'The Three Trials'],
      ['MEC28', 'Trouble in Tharbad'],
      ['MEC29', 'The Nîn-in-Eilph'],
      ['MEC30', "Celebrimbor's Secret"],
      ['MEC31', 'The Antlered Crown'],
    ]),
  ]),
  campaign('angmar-awakened', 'Angmar Awakened', 'MEC38', [
    ...fromBox('MEC38', ['Intruders in Chetwood', 'The Weather Hills', "Deadmen's Dike"]),
    ...fromPacks([
      ['MEC39', 'The Wastes of Eriador'],
      ['MEC40', 'Escape from Mount Gram'],
      ['MEC41', 'Across the Ettenmoors'],
      ['MEC42', 'The Treachery of Rhudaur'],
      ['MEC43', 'The Battle of Carn Dûm'],
      ['MEC44', 'The Dread Realm'],
    ]),
  ]),
  campaign('dream-chaser', 'Dream-chaser', 'MEC47', [
    ...fromBox('MEC47', ['Voyage Across Belegaer', 'The Fate of Númenor', 'Raid on the Grey Havens']),
    ...fromPacks([
      ['MEC48', 'Flight of the Stormcaller'],
      ['MEC49', 'The Thing in the Depths'],
      ['MEC50', 'Temple of the Deceived'],
      ['MEC51', 'The Drowned Ruins'],
      ['MEC52', 'A Storm on Cobas Haven'],
      ['MEC53', 'The City of Corsairs'],
    ]),
  ]),
  campaign('haradrim', 'Haradrim', 'MEC55', [
    ...fromBox('MEC55', ['Escape from Umbar', 'Desert Crossing', 'The Long Arm of Mordor']),
    ...fromPacks([
      ['MEC56', 'The Mûmakil'],
      ['MEC57', 'Race Across Harad'],
      ['MEC58', 'Beneath the Sands'],
      ['MEC59', 'The Black Serpent'],
      ['MEC60', 'The Dungeons of Cirith Gurat'],
      ['MEC61', 'The Crossings of Poros'],
    ]),
  ]),
  campaign('ered-mithrin', 'Ered Mithrin', 'MEC65', [
    ...fromBox('MEC65', ['Journey Up the Anduin', 'Lost in Mirkwood', "The King's Quest"]),
    ...fromPacks([
      ['MEC66', 'The Withered Heath'],
      ['MEC67', 'Roam Across Rhovanion'],
      ['MEC68', 'Fire in the Night'],
      ['MEC69', 'The Ghost of Framsburg'],
      ['MEC70', 'Mount Gundabad'],
      ['MEC71', 'The Fate of Wilderland'],
    ]),
  ]),
  campaign('vengeance-of-mordor', 'Vengeance of Mordor', 'MEC77', [
    ...fromBox('MEC77', ['The River Running', 'Danger in Dorwinion', 'The Temple of Doom']),
    ...fromPacks([
      ['MEC78', 'Wrath and Ruin'],
      ['MEC79', 'The City of Ulfast'],
      ['MEC80', 'Challenge of the Wainriders'],
      ['MEC81', 'Under the Ash Mountains'],
      ['MEC82', 'The Land of Sorrow'],
      ['MEC83', 'The Fortress of Nurn'],
    ]),
  ]),
  campaign('the-hobbit', 'The Hobbit', 'MEC16', [
    ...fromBox('MEC16', ['We Must Away, Ere Break of Day', 'Over the Misty Mountains Grim', 'Dungeons Deep and Caverns Dim']),
    ...fromBox('MEC24', ['Flies and Spiders', 'The Lonely Mountain', 'The Battle of Five Armies']),
  ]),
  campaign('the-lord-of-the-rings', 'The Lord of the Rings', 'MEC32', [
    ...fromBox('MEC32', ['A Shadow of the Past', 'A Knife in the Dark', 'Flight to the Ford']),
    ...fromBox('MEC34', ['The Ring Goes South', 'Journey in the Dark', 'Breaking of the Fellowship']),
    ...fromBox('MEC45', ['The Uruk-hai', "Helm's Deep", 'The Road to Isengard']),
    ...fromBox('MEC46', ['The Passage of the Marshes', 'Journey to the Cross-roads', "Shelob's Lair"]),
    ...fromBox('MEC54', ['The Passing of the Grey Company', 'The Siege of Gondor', 'The Battle of the Pelennor Fields']),
    ...fromBox('MEC62', ['The Tower of Cirith Ungol', 'The Black Gate Opens', 'Mount Doom']),
  ]),
]
