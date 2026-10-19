// The official campaigns: each deluxe box with its cycle of adventure packs, and the two
// Sagas. Scenario lists follow Hall of Beorn; box art lives in /images/products by code.

export interface ScenarioOption {
  id: string
  name: string
  image: string
  /** Only scenarios the engine scripts can be started. */
  available: boolean
}

export interface Campaign {
  id: string
  name: string
  image: string
  scenarios: ScenarioOption[]
}

const product = (code: string) => `/images/products/${code}.jpg`

const slug = (name: string) =>
  name
    .normalize('NFKD')
    .replace(/[^\w\s-]/g, '')
    .trim()
    .toLowerCase()
    .replace(/\s+/g, '-')

// A box's own scenarios share its art; each adventure pack has its own.
const fromBox = (code: string, names: string[]): ScenarioOption[] =>
  names.map((name) => ({ id: slug(name), name, image: product(code), available: false }))

const fromPacks = (packs: [string, string][]): ScenarioOption[] =>
  packs.map(([code, name]) => ({ id: slug(name), name, image: product(code), available: false }))

const playable = new Set(['passage-through-mirkwood'])

const campaign = (id: string, name: string, code: string, scenarios: ScenarioOption[]): Campaign => ({
  id,
  name,
  image: product(code),
  scenarios: scenarios.map((s) => ({ ...s, available: playable.has(s.id) })),
})

export const campaigns: Campaign[] = [
  campaign('core-set', 'Core Set', 'MEC01', fromBox('MEC01', ['Passage Through Mirkwood', 'Journey Along the Anduin', 'Escape from Dol Guldur'])),
  campaign(
    'shadows-of-mirkwood',
    'Shadows of Mirkwood',
    'MEC02',
    fromPacks([
      ['MEC02', 'The Hunt for Gollum'],
      ['MEC03', 'Conflict at the Carrock'],
      ['MEC04', 'A Journey to Rhosgobel'],
      ['MEC05', 'The Hills of Emyn Muil'],
      ['MEC06', 'The Dead Marshes'],
      ['MEC07', 'Return to Mirkwood'],
    ]),
  ),
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
