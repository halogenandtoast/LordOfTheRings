module LordOfTheRings.Trait (module LordOfTheRings.Trait) where

import Data.Char (isUpper)
import LordOfTheRings.Prelude

data Trait
  = Basic
  | Damaged
  | Pillager
  | Accolade
  | Archer
  | Assault
  | Attack
  | Armor
  | Arnor
  | Artifact
  | Balrog
  | Barrow
  | Battleground
  | Beorning
  | Besieged
  | Besieger
  | Blight
  | Boar
  | BoarClan
  | Bree
  | Body
  | Adaptation
  | Boon
  | Bridge
  | Brigand
  | Broken
  | Burden
  | Burglar
  | CairAndros
  | Camp
  | Captain
  | Captive
  | Captured
  | -- | the Capture trait, named apart from the keyword (The Mûmakil)
    CaptureTrait
  | CarnDum
  | Castle
  | Cave
  | Champion
  | City
  | LakeTown
  | Pier
  | Clue
  | Coastland
  | Condition
  | Corrupt
  | Corsair
  | Craftsman
  | Corruption
  | Creature
  | Cultist
  | Cursed
  | Dale
  | Dark
  | Defense
  | Desert
  | Deck
  | DeadMarshes
  | Disaster
  | DolAmroth
  | DolGuldur
  | Dragon
  | Dunedain
  | Dungeon
  | Dunland
  | Dwarf
  | Eagle
  | AllyTrait
  | EastBank
  | WestBank
  | Polluted
  | Easterling
  | Elite
  | EmynMuil
  | Enedwaith
  | Ent
  | Escape
  | Esgaroth
  | Ettenmoors
  | Flame
  | Forest
  | Fornost
  | Fortification
  | Gate
  | Giant
  | Goblin
  | Gollum
  | Gondor
  | Gossip
  | GreyHavens
  | Grotto
  | Guardian
  | Harad
  | Hazard
  | Healer
  | Highlands
  | Hills
  | Hobbit
  | Huorn
  | Inn
  | Insect
  | Instrument
  | Isengard
  | Istari
  | Item
  | Ithilien
  | Key
  | Lair
  | Legend
  | Light
  | Marsh
  | Marshland
  | Master
  | Mathom
  | Minstrel
  | Mirkwood
  | Mission
  | Modification
  | Mordor
  | Mount
  | MountGram
  | Mountain
  | Mumak
  | Nameless
  | Nazgul
  | Noble
  | Noldor
  | Ocean
  | Orc
  | Osgiliath
  | OstInEdhil
  | Outlands
  | Panic
  | Pipe
  | Pipeweed
  | Plains
  | Plot
  | Raider
  | Ranger
  | Rat
  | Raven
  | RavenClan
  | Record
  | Reforged
  | Ring
  | River
  | Riverland
  | Road
  | Rohan
  | Ruins
  | Scheme
  | Scout
  | Scroll
  | -- | the Search trait, named apart from the message (Beneath the Sands)
    SearchTrait
  | Shadow
  | Ship
  | Shirriff
  | Signal
  | Silvan
  | Skill
  | Snow
  | Song
  | Sorcerer
  | Sorcery
  | Spell
  | Spider
  | -- | the Spirit trait, named apart from the sphere
    SpiritTrait
  | Snaga
  | Spy
  | Staff
  | Stair
  | Steward
  | Stronghold
  | Summoner
  | Swamp
  | Tactic
  | Tantrum
  | Tentacle
  | Thug
  | -- | the Time trait, named apart from the keyword
    TimeTrait
  | Title
  | Tools
  | Town
  | Traitor
  | Trap
  | Troll
  | Umbar
  | Undead
  | Underground
  | Underwater
  | Underworld
  | Upgraded
  | Uruk
  | Valley
  | Village
  | Villain
  | Warden
  | Warg
  | Warrior
  | Wasteland
  | Weapon
  | Weather
  | Werewolf
  | Wight
  | -- | the Wilderlands trait, named apart from the encounter set
    WilderlandsTrait
  | Wolf
  | Woodman
  | Wose
  | Wraith
  -- the Saga Expansions'
  | CirithUngol
  | Despair
  | Doom
  | DoomedTrait
  | Enchantment
  | Erebor
  | Favor
  | Fear
  | FellowshipTrait
  | Gift
  | HelmsDeepTrait
  | Hill
  | Information
  | Lawn
  | Mearas
  | MinasTirith
  | Morgul
  | Oathbreaker
  | Pelennor
  | PerilTrait
  | Pony
  | Refuge
  | RingBearer
  | Sack
  | Shire
  | SiegeTrait
  | Stream
  | SurgeTrait
  | TreasureTrait
  | Trollshaws
  | UrukHai
  | Vale
  | WesternLandsTrait
  | Wound
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The trait as a player reads it, its words apart: "Carn Dûm"'s accents aside.
traitName :: Trait -> Text
traitName = \case
  LakeTown -> "Lake-town"
  AllyTrait -> "Ally"
  TreasureTrait -> "Treasure"
  WilderlandsTrait -> "Wilderlands"
  DoomedTrait -> "Doomed"
  FellowshipTrait -> "Fellowship"
  HelmsDeepTrait -> "Helm's Deep"
  PerilTrait -> "Peril"
  RingBearer -> "Ring-bearer"
  SiegeTrait -> "Siege"
  SurgeTrait -> "Surge"
  UrukHai -> "Uruk-hai"
  WesternLandsTrait -> "Western Lands"
  t -> pack $ unwords $ words $ concatMap (\c -> if isUpper c then [' ', c] else [c]) (show t)
