{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH (deriveJSON, deriveToJSON, mkParseJSON)
import Data.Aeson.Types (Parser)
import LordOfTheRings.Campaign (CampaignLogKey)
import LordOfTheRings.Card.CardType (CardType)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id (CardId, EnemyId, PlayerId)
import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Matcher.Types (CardMatcher, EnemyMatcher, HeroMatcher, LocationMatcher, PlayerMatcher)
import LordOfTheRings.Outcome (Outcome)
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Placement (Placement)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere
import LordOfTheRings.Target (Target)
import LordOfTheRings.Token (Token)
import LordOfTheRings.Trait

data ModifierType
  = WillpowerModifier Int
  | AttackModifier Int
  | -- | on an enemy: its attack is raised by this much when undefended, as Goblin
    -- Swordsman's
    AttackModifierIfUndefended Int
  | DefenseModifier Int
  | HitPointModifier Int
  | ThreatModifier Int
  | -- | on a location: its threat, every other modifier applied, is reduced by half,
    -- rounding up, as Secret Entrance reduces The Lonely Mountain's
    ThreatHalved
  | EngagementCostModifier Int
  | -- | on an enemy: its engagement cost is this much higher as the player makes an
    -- engagement check against it, as Hunting Pack's is lower
    EngagementCostAgainst PlayerId Int
  | -- | on a player: he is considered to have this much more threat for engagement checks
    -- only, as Vicious Trackers reads; read with 'getEngagementThreat'
    ThreatForEngagementChecks Int
  | GainTrait Trait
  | GainResourceIcon Sphere
  | ReturnToHandAtEndOfPhase
  | -- | on an ally, or its card id: it is discarded at the end of the phase, as Command's
    DiscardAtEndOfPhase
  | -- | on a character, until the end of the phase: the modifier applies to its next attack
    -- only, as Tides of Fate's "+3 attack for its next attack this phase"
    ForNextAttack ModifierType
  | -- | on an enemy, until the end of the phase: once its next attack against the player
    -- resolves it makes an additional attack against him, as Wolf of Isengard's Forced
    -- effect has it as it is dealt its shadow card, before that attack begins
    AdditionalAttackAfterNextAttack PlayerId
  | -- | on the game: if the players win the scenario, the card is recorded under the key in
    -- the campaign log, as Determination marks its character "determined"
    RecordedIfWon CampaignLogKey CardCode
  | ShuffleIntoDeckAtEndOfPhase
  | -- | a card in the staging area adds nothing to the staging total
    DoesNotContributeThreat
  | -- | on a card in play: player card effects do not touch it
    ImmuneToPlayerCardEffects
  | -- | on a card in play: player events do not touch it, as The Shattered Monument reads
    ImmuneToPlayerEvents
  | -- | on a card in play: only Master cards' effects touch it, as The One Ring reads
    ImmuneToNonMasterCardEffects
  | -- | on a card in play: no player card's effect touches it but a Fellowship sphere
    -- card's, as Gollum reads in The Land of Shadow
    ImmuneToNonFellowshipPlayerCardEffects
  | -- | on a location: no card effect, player or encounter, places or removes its tokens
    ImmuneToCardEffects
  | -- | on a card in play: no encounter card's effect touches it but one with the trait, as
    -- Gollum is immune to non-Gollum card effects ('ImmuneToPlayerCardEffects' for the
    -- player cards' effects)
    ImmuneToEncounterCardEffectsWithout Trait
  | -- | on a location: player card effects cannot place progress on it, as Retake the City
    -- reads for the staging area
    NoProgressFromPlayerCards
  | -- | on the game: each racing test requires this many more matching results, as Ravine
    -- Ledge reads while in the staging area
    RacingTestAdditionalResults Int
  | -- | on a location: its quest points are raised by this much, as an X reads
    QuestPointModifier Int
  | -- | on a location: its quest points are at most this, after the others, as Woodmen's
    -- Path reduces them to 1
    QuestPointsReducedTo Int
  | -- | on an enemy, or a character; on a player, none of their characters may attack,
    -- as Hobbit-sense reads
    CannotAttack
  | -- | on a character: it does not exhaust, so cannot commit, attack or defend
    CannotExhaust
  | -- | on a character; on the game, no character readies, as Enfeebled reads
    CannotReady
  | -- | on an enemy: characters with ranged may attack it in the staging area
    AttackableInStagingAreaByRanged
  | -- | on an enemy: it cannot attack that player, though it may attack others
    CannotAttackPlayer PlayerId
  | CannotAttackOrDefend
  | -- | on an enemy: allies cannot defend while it attacks, as Snow Warg's
    CannotBeDefendedByAllies
  | CannotHaveAttachments
  | -- | on a character: no treachery card can be attached to it, as Beyond All Hope reads
    CannotHaveTreacheryAttachments
  | -- | no player attachment without the trait can be attached to it, as a Nazgûl's
    -- "Cannot have non-Morgul attachments" reads
    CannotHaveAttachmentsWithoutTrait Trait
  | -- | on a player: he keeps the first player token as the refresh phase would pass it, as
    -- Last of the Company reads
    CannotLoseFirstPlayerToken
  | -- | no attachment with the restricted keyword can be attached to it
    CannotHaveRestrictedAttachments
  | -- | on a character: it can have this many more restricted attachments than 2, as
    -- Forth, The Three Hunters! gives each hero 1 and Golden Belt its hero
    AdditionalRestrictedSlots Int
  | -- | on the game: the total threat in the staging area is raised by this much
    StagingThreatBonus Int
  | -- | on the game: the total threat in the staging area is raised by 1 for each ally in
    -- the player's discard pile, as Dread and Despair reads
    StagingThreatPerAllyInDiscardOf PlayerId
  | -- | on the game: the archery total is raised by this much, as Pinned Down reads
    ArcheryBonus Int
  | -- | on the game: archery damage must be assigned to allies, if able, as An Untimely
    -- End reads
    ArcheryDamageToAllies
  | -- | on the game, or a character: characters use their willpower instead of defense,
    -- as Against the Shadow reads
    WillpowerForDefense
  | -- | on the game: characters use their willpower instead of attack when attacking
    -- enemies, as The Passage Out reads
    WillpowerForAttack
  | -- | on the game: attacks on enemies place progress on them instead of damage, as The
    -- Passage Out reads
    ProgressInsteadOfAttackDamage
  | -- | on an enemy: its attack that would damage a character captures that character
    -- underneath it instead, as Cruel Torturer reads (Escape from Mount Gram)
    CaptureInsteadOfAttackDamage
  | -- | on a hero: resources from card effects are not placed in its pool, as Garden of
    -- Poisons reads
    CannotGainResourcesFromCardEffects
  | -- | on the game: reveal this many more encounter cards during staging
    AdditionalStagingReveals Int
  | -- | on the game: reveal fewer encounter cards, allowing staging to reveal none; on a
    -- player, where each player reveals his own (The Lord of the Rings saga), he reveals
    -- fewer, as Palantír of Orthanc reads
    FewerStagingReveals Int
  | NoEngagementChecks
  | -- | on an enemy: it makes no engagement checks against the player, though he may still
    -- engage it, as Grey Cloak reads
    NoEngagementChecksAgainst PlayerId
  | -- | on the game: enemies with ambush make no engagement check as they enter play
    IgnoreAmbush
  | -- | on the game: in the staging step the first player alone reveals this many
    -- encounter cards, not one for each player, as The Battle of the Morannon reads
    FirstPlayerRevealsForStaging Int
  | SkipStagingStep
  | -- | on the game: the players, as a group, cannot play more than 1 ally each round
    OneAllyPerRound
  | -- | on the game or a card: a once-per-round effect keyed by the code was used, any
    -- copy counting, as Steward of Orthanc reads. A card played is 'PlayedCardThisRound'
    UsedThisRound CardCode
  | -- | on an ally: it entered play this round
    EnteredPlayThisRound
  | -- | damage an attack deals past the character's remaining hit points raises threat
    ExcessDamageToThreat
  | -- | on an enemy: for each point of damage its attack deals past the character's remaining
    -- hit points, the player damages another character they control
    ExcessDamageToOtherCharacters
  | -- | on an enemy: for each point of damage its attack deals past the character's remaining
    -- hit points, the player discards the top card of their deck, as Undead Horde reads
    ExcessDamageDiscardsFromDeck
  | CannotBeOptionallyEngagedWhileOthersInStaging
  | -- | on an enemy, or on the game for every enemy: matching players cannot optionally
    -- engage it
    CannotBeOptionallyEngagedBy PlayerMatcher
  | -- | on an enemy in the staging area: it counts as engaged with each matching player while
    -- it stays there, as Durin's Bane, and is dealt a new shadow card for each attack
    EngagedWithEachPlayer PlayerMatcher
  | DoesNotCountDefense
  | -- | on a character: its attack counts as 0
    DoesNotCountAttack
  | -- | on an enemy: its attack is undefended, no character may defend against it
    CannotBeDefended
  | -- | on an enemy: characters defending against it do not count their defense, as Fortify
    -- the Defense reads of Dagnir
    DefendersDoNotCountDefense
  | -- | on a location or quest card: at most this much more progress can be placed on it
    -- now, as a stage that "cannot have more than 15 progress tokens on it" gives itself
    ProgressLimit Int
  | -- | on a quest card: it is not defeated, whatever its progress; once nothing gives it
    -- this, it is checked again, as "the players cannot defeat this stage while..." reads
    CannotBeDefeated
  | -- | on a character committed to the quest: its willpower does not count toward it
    DoesNotCountWillpower
  | -- | on a character: its willpower is reduced to 0, whatever else modifies it, as A Fell
    -- Dread reads; one whose willpower cannot be reduced keeps it
    WillpowerReducedToZero
  | -- | on an enemy: it gets +1 threat for each token of the kind on it, counted as they
    -- change, as Foes of Ecthelion reads for resources
    ThreatPerToken Token
  | -- | on a card: it gets this much threat while it has a token of the kind on it, counted
    -- as they change, as Vastness of Mirkwood's "+2 instead if it has at least 1 progress"
    ThreatWithToken Token Int
  | -- | on an enemy, for an attack: it is the attack its own ability made, as Urdug's
    -- "If this attack deals damage" reads of the immediate attack Tiny gives him
    AttackMadeByOwnAbility
  | -- | on the game: the total threat in the staging area is raised by 1 for each token of
    -- the kind on the card, counted as they change, as The Tower Beckons reads
    StagingThreatPerToken Target Token
  | -- | on the game: each ally in play takes the modifier, one that enters play later too,
    -- for as long as the effect lasts, as Pit into the Depths reads
    EachAlly ModifierType
  | -- | on the game: each character takes the modifier while it is exhausted, as Weighed
    -- Down's "until the end of the round, exhausted characters get -1 willpower" reads
    EachExhaustedCharacter ModifierType
  | -- | on the game: each character takes the modifier while it is damaged, as Poisoned
    -- Vapour's "until the end of the combat phase, treat each damaged character's text box
    -- as if it was blank" reads, recalculated as the game state changes (FAQ 1.55)
    EachDamagedCharacter ModifierType
  | -- | on an enemy: it has attacked this phase, as Howling of Wargs reads
    AttackedThisPhase
  | -- | on the game: no more than this much progress may be placed on locations with the
    -- trait this phase, all of them together, as Sulfur Cloud reads for Underwater
    PhaseProgressLimitOnTrait Trait Int
  | -- | on the game, by an effect until the end of the phase: this much progress was placed
    -- on locations with the trait (see 'PhaseProgressLimitOnTrait')
    ProgressPlacedOnTraitThisPhase Trait Int
  | -- | on an enemy: each character its excess damage goes to, as
    -- 'ExcessDamageToOtherCharacters' sends it, is exhausted, as Terrible Strength reads
    ExhaustsCharactersDamagedByExcess
  | -- | on a character: it is discarded from play once its willpower is 0, as The
    -- Mountains' Peaks reads
    DiscardedAtNoWillpower
  | -- | on a location: when the players may travel and there is no active location, they
    -- must travel here
    MustBeTraveledTo
  | LeavesPlayToTopOfEncounterDeck
  | -- | on an objective: discarded, it goes to the encounter discard pile, not back to the
    -- staging area, as Wormsbane
    LeavesPlayToEncounterDiscard
  | -- | on a Sack objective: leaving play, it is shuffled back into the sack deck (We Must
    -- Away, Ere Break of Day rules)
    LeavesPlayToSackDeck
  | ShuffledIntoEncounterDeckWhenDefeated
  | -- | on an encounter card: as it leaves play the first player shuffles it into any
    -- player's deck, as Recurring Nightmare reads
    LeavesPlayToAnyPlayersDeck
  | ReturnToTopOfEncounterDeckAfterCombat
  | -- | on a location: the players cannot travel there now (its travel cost cannot be paid)
    CannotBeTraveledTo
  | -- | on a location: the players may travel there while 1 other location is active,
    -- making 2 active locations, as Ruined Tower reads
    TravelableWithActiveLocation
  | -- | on a player
    CannotDrawCards
  | DoesNotReadyDuringRefresh
  | DoesNotExhaustToAttack
  | DoesNotExhaustToDefend
  | -- | on a character: matching attachments played onto it cost this much less
    ReduceCostToAttach CardMatcher Int
  | -- | on an enemy: any player's characters may attack it, wherever it is
    AttackableByAnyCharacter
  | -- | on an enemy: the player attacks it as if it were engaged with him, wherever it is,
    -- as Charge into Battle reads
    AttackedAsIfEngagedWith PlayerId
  | -- | when attacking an enemy with the trait, attack is raised by this much
    AttackBonusAgainst Trait Int
  | -- | the hero may attack an enemy in the staging area when attacking alone
    CanAttackStagingAreaAlone
  | -- | the character may be declared as an attacker against enemies in the staging area
    CanAttackStagingArea
  | -- | when attacking a matching enemy, attack is raised by this much
    AttackBonusAgainstMatching EnemyMatcher Int
  | -- | the character is discarded once the attack it makes is over
    DiscardAfterAttack
  | -- | on an ally, or its card: it is discarded at the end of the round
    DiscardAtEndOfRound
  | -- | on a hero, or its card: it is removed from the game at the end of the round
    RemoveFromGameAtEndOfRound
  | -- | on a character, or its card: leaving play by any means, it is removed from the
    -- game instead, as Arkham's RemoveFromGameInsteadOfDiscard
    RemoveFromGameIfLeavesPlay
  | -- | on a card whose effect lets its controller choose 1 of its options: they choose up
    -- to this many instead, as Gandalf put into play by an upgraded Send for Help
    ChoosesUpToOptions Int
  | -- | on a hero: while paying for a matching card it has the sphere's resource icon
    GainResourceIconFor CardMatcher Sphere
  | -- | on a hero: while paying for a matching card it is considered to have the sphere's
    -- printed resource icon, which gives a 0-cost card its resource match too, as Bard son
    -- of Brand has for an Item attachment
    PrintedResourceIconFor CardMatcher Sphere
  | -- | on a card, as its card id: it needs no resource match, its cost paid with
    -- resources of any sphere, as Thranduil plays a Silvan ally
    NoResourceMatchNeeded
  | -- | on a player: they may declare any number of defenders against each attack
    MayDeclareMultipleDefenders
  | -- | on a player
    CannotCommitCharacters
  | GainKeyword Keyword
  | -- | the card loses a printed keyword, as The Last Battle loses siege
    LoseKeyword Keyword
  | -- | on a character: it defends against enemies with the trait without exhausting
    DoesNotExhaustToDefendAgainst Trait
  | CannotCommitToQuest
  | -- | on a character: during the quest phase it must commit to the quest, if able, as A
    -- Fell Dread reads
    MustCommitToQuest
  | -- | on a card in play: its abilities cannot be used, nor its Forced effects trigger
    CannotTriggerAbilities
  | -- | on a character: it cannot be given poison, as an unconscious character in Flies and
    -- Spiders
    CannotBePoisoned
  | -- | on a card in play: its printed text box is treated as blank, as Arkham's: it has no
    -- abilities or keywords, and its constant effects give no modifiers
    Blank
  | -- | on a hero
    CannotCollectResources
  | -- | on a hero: no resources can be spent from its pool, as Orc Vanguard's
    CannotSpendResources
  | -- | on a hero: it collects this many more resources in the resource phase
    CollectsAdditionalResources Int
  | -- | on the game: resources of any sphere may be spent as this sphere's
    ResourcesSpendableAs Sphere
  | CannotBeHealed
  | -- | on an enemy: no character may attack it
    CannotBeAttacked
  | -- | on an enemy: it can only be attacked by one character at a time, as Tom makes the
    -- Troll enemies
    AttackedByOneCharacterAtATime
  | -- | on an enemy: it has dealt damage this round, as Rock Adder reads
    DealtDamageThisRound
  | -- | on a character or enemy: damage dealt to it is canceled
    CannotTakeDamage
  | -- | on an enemy: the damage of attacks on it is canceled
    CannotTakeCombatDamage
  | -- | on an enemy: only a quest card's effects can damage it, as Dagnir's Slave gives Dagnir
    CannotTakeNonQuestDamage
  | -- | on an enemy: characters attacking it with ranged, engaged with another player,
    -- add nothing to the attack
    ImmuneToRangedDamage
  | -- | on a character: no single effect heals it of more than this
    HealLimit Int
  | -- | on a character: no more than this much damage can be healed from it each round, as
    -- Port of Pelargir reads
    HealLimitEachRound Int
  | -- | on a character: the damage healed from it this round, counted against a
    -- 'HealLimitEachRound'
    HealedThisRound Int
  | -- | on an enemy: at most this much more damage can be placed on it now, as Mumak's
    -- 3 a round
    DamageLimit Int
  | -- | on an enemy: the damage placed on it this round that counts against a limit of its
    -- own, as Dagnir's "cannot take more than X damage from non-quest effects each round"
    DamageTakenThisRound Int
  | -- | on an enemy: it cannot have more than this much damage on it; damage past that is not
    -- dealt, as The City of Ulfast's stages read of City Guard and The Final Confrontation of Thane Ulchor
    MaxDamage Int
  | -- | on an enemy: it cannot take more than this much damage each round, counted against
    -- 'DamageTakenThisRound' and read as the damage is placed, so the limit ends with the card
    -- that gives it, as A Haunt for Beasts reads, and follows the board, as Leader of the
    -- Cult's "X is the printed quest points of the active location"
    DamageLimitEachRound Int
  | -- | on an objective ally in the staging area: it is committed to the current quest, as
    -- Celador reads
    CountsAsCommitted
  | -- | on an enemy: only characters with the trait or the keyword can attack it or defend
    -- against it
    OnlyFoughtBy Trait Keyword
  | -- | on an enemy: only characters with the keyword can attack it or defend against it,
    -- as Moria Bats reads for ranged
    OnlyFoughtByKeyword Keyword
  | -- | on an enemy: only the player it is engaged with declares attackers or defenders
    -- against it, no other player's ranged or sentinel character, as Khamûl the Easterling
    -- reads
    OnlyEngagedPlayerFights
  | -- | on an enemy: allies cannot be declared as attackers or defenders against it, as
    -- Ulwarth reads
    NotFoughtByAllies
  | -- | on an enemy: damage from its undefended attacks is dealt to this character
    UndefendedDamageTo Target
  | -- | on an ally: the resources on it can pay for matching cards its controller plays
    ResourcesPayFor CardMatcher
  | -- | on a card: matching heroes' resources can pay for it, even those of a hero its
    -- player does not control, as Burglar Baggins's from Bilbo Baggins' pool
    PayableFromHeroes HeroMatcher
  | -- | on the game: players cannot play matching cards
    CannotPlay CardMatcher
  | -- | on a player: no matching card can be put into play under his control, as
    -- Fellowship reads "put non-unique allies into play"
    CannotPutIntoPlay CardMatcher
  | -- | on a hero: it gains the game text of the hero card with this code, as Thorongil
    -- gives it; that card, built as the hero, gives its constant abilities and resolves the
    -- abilities the hero gains (see 'LordOfTheRings.Hero')
    GainGameTextOf CardCode
  | -- | on a card in hand, as its card id: it cannot be played from there, as Misty
    -- Mountain Journeyman reads "unless you are engaged with an enemy"
    CannotBePlayedFromHand
  | -- | on the game: attachments' printed text boxes are treated as blank
    BlankAttachments
  | -- | on the game: the Deep keyword is ignored, as The Fire Worm reads
    IgnoreDeep
  | -- | on the game: the Discover keyword is ignored, as Mount Gundabad's campaign card reads
    IgnoreDiscover
  | -- | on the game: a delayed effect, spent at the next quest resolution, adding a copy of
    -- the card to the campaign pool, if able, as A Growing Threat reads
    AddCopyToCampaignPoolAtQuestResolution CardCode
  | -- | on a location: encounter card effects cannot discard it from play, as Mount
    -- Gundabad's campaign card reads for unique locations
    CannotBeDiscardedByEncounterCards
  | -- | on a location: its Discover value moves by this much, as Hidden Staircase's X and
    -- Crypt Key's 1 read
    DiscoverValueModifier Int
  | -- | on an enemy with no victory points printed: it has this many, and goes to the
    -- victory display as it is defeated, as The Cursed Shade gives Fram's Shade victory 6
    GainVictory Int
  | -- | on an enemy: it is dealt this many shadow cards whenever it is dealt one, as Fram's
    -- Shade reads
    ShadowCardsDealt Int
  | -- | on a player, by an effect until the end of the round: player card effects reduced
    -- his threat by this much this round, for Moonlit Passage's limit to count
    ThreatReducedByPlayerCardsThisRound Int
  | -- | on a hero: it cannot commit to Fortitude tests, as Heavy and Tired's host
    CannotCommitToFortitudeTests
  | -- | on the game: Eye icons added to each Fortitude test, as Brake of Thorns adds
    FortitudeTestEyes Int
  | -- | on a player, by an effect until the end of the round: he failed a Fortitude test
    -- this round
    FailedFortitudeTestThisRound
  | -- | on the game: every character must commit to the quest
    MustCommitAll
  | -- | on the game: each card dealt for an escape test adds this much more
    EscapeBonus Int
  | -- | on a character: damage dealt to it is dealt to the given character instead
    DamageRedirectedTo Target
  | -- | on a card: damage that would be placed on it is placed on the given card first, as
    -- much as that card still takes, the rest on this one, as City Wall takes Minas
    -- Tirith's
    DamagePlacedFirstOn Target Int
  | -- | on a character: it commits to the quest without exhausting
    DoesNotExhaustToCommit
  | -- | on the game: no player card may cancel an encounter card's effects
    CannotCancelEncounterEffects
  | -- | on an encounter card: its When Revealed effect cannot be canceled; on the game:
    -- no encounter card's can be
    WhenRevealedCannotBeCanceled
  | -- | on a shadow card: its shadow effect cannot be canceled, as Deadly Waters makes a
    -- treachery's; on the game: no shadow card's can be, as Stronghold Tower reads. A card
    -- whose own text says so has 'cdShadowCannotBeCanceled', as Blazing Grip
    ShadowCannotBeCanceled
  | -- | on the game: matching cards cost this much more to play from hand
    IncreaseCost CardMatcher Int
  | -- | on a player: matching cards they play from hand cost this much less
    ReduceCostOf CardMatcher Int
  | -- | as 'ReduceCostOf', to no less than 1, as Master of Lore's
    ReduceCostOfToMinimumOne CardMatcher Int
  | -- | on a card in hand, as its card id: it costs this much more to play (less, when
    -- negative)
    CostModifier Int
  | -- | on a player, or on the game for every player: the next matching card played
    -- costs this much less, to no less than 1
    ReduceNextCostOf CardMatcher Int
  | -- | as 'ReduceNextCostOf', to no less than 0
    LowerNextCostOf CardMatcher Int
  | -- | on a player: the next matching card he plays does not require a resource match,
    -- as King of Dale's ally; spent with it
    NextIgnoresResourceMatch CardMatcher
  | -- | on a character: it is defending the attack being resolved
    Defending
  | -- | on a card in hand, as its card id: its player may discard this many cards from
    -- the top of their deck to lower its cost by as much
    ReducedByDiscardingFromDeck Int
  | -- | on a character: the enemy it attacks alone does not count its defense
    IgnoresDefenseWhenAttackingAlone
  | -- | on a character: a matching enemy it attacks alone does not count its defense, as
    -- Skinbark an Orc
    IgnoresDefenseOfWhenAttackingAlone EnemyMatcher
  | -- | on a player: an enemy engaged them this round
    EngagedAnEnemyThisRound
  | -- | on a player: his threat was raised this round, by anything, as Darkness in the
    -- East's shadow reads "if you raised your threat this round"
    RaisedThreatThisRound
  | -- | on an enemy: it engaged a player this round, as Nowhere to Hide's shadow reads
    EngagedThisRound
  | -- | on a player: they played a copy of the card this phase
    PlayedThisPhase CardCode
  | -- | on a player: the printed card and type played this phase
    PlayedCardThisPhase CardCode CardType
  | -- | on a card: the card type its controller named
    NamedCardType CardType
  | -- | on a card: an effect chose it, its source reading it back later, as the enemy Bilbo
    -- Baggins chose to damage if the players quest successfully
    ChosenByEffect
  | -- | on an ally, as its card id: it returns to its owner's hand once the players quest
    -- successfully this phase, as Bofur's
    ReturnToHandIfQuestedSuccessfully
  | -- | on a side quest: the players quest against it this phase, as the current quest
    CurrentQuest
  | -- | on a quest card: the players cannot choose it as the current quest in the quest
    -- phase, as Secrets of Rhudaur reads
    CannotBeCurrentQuest
  | -- | on a quest card: at most this much progress can be placed on it each round, as
    -- Orc Rearguard gives the current quest
    ProgressPerRoundLimit Int
  | -- | on a quest card, by an effect until the end of the round: this much progress was
    -- placed on it this round (see 'progressPlacedThisRound')
    ProgressPlacedThisRound Int
  | -- | on a quest card, by an effect until the end of the phase: this much progress was
    -- placed on it this phase (see 'progressPlacedThisPhase')
    ProgressPlacedThisPhase Int
  | -- | on a quest card, by an effect until the end of the phase: progress from questing
    -- successfully was placed on it, as The Lower Depths' "after placing progress here
    -- from questing successfully" reads
    ProgressFromQuestingThisPhase
  | -- | on the game: the next enemy to enter the staging area takes the modifier instead,
    -- for as long as the effect lasts
    NextEnemyInStaging ModifierType
  | -- | on an enemy: it cannot leave play unless destroyed; an effect that would discard
    -- it, return it to the encounter deck or remove it from the game does nothing
    CannotLeavePlay
  | -- | on a character or enemy: any amount of damage dealt to it is reduced to this much
    DamageReducedTo Int
  | -- | on a character, as a lasting effect: the next time it would be dealt damage, so
    -- much of that damage is canceled, and the effect ends, as Turayn's "The next time
    -- that character would be dealt damage this round, cancel 1 of that damage"
    CancelNextDamage Int
  | -- | the card is considered to be there too, wherever it is, as Tide-runner off-course is
    -- in the staging area and the Stormcaller's area: what reads where it is reads that, but
    -- it does not move there
    ConsideredIn Placement
  | -- | on the game: matching enemies engaged with players add their threat to the staging
    -- area too, as Conflict at the Crossroads reads
    EngagedEnemiesAddThreat EnemyMatcher
  | -- | on a player: matching cards in their discard pile may be played as if from hand,
    -- by discarding the modifier's source, then go to the bottom of their deck, as a
    -- Record's Action reads
    MayPlayFromDiscard CardMatcher
  | -- | on a player: he plays with the top card of his deck faceup, as Gandalf (The Road
    -- Darkens) reads
    TopOfDeckFaceup
  | -- | on a player: he may play the top card of his deck as if it was in his hand, the
    -- modifier's source then given 'PlayedTopOfDeckThisPhase', as Gandalf (The Road Darkens)
    -- lets him once per phase
    MayPlayTopOfDeck
  | -- | on a card: a card was played from the top of a deck by the 'MayPlayTopOfDeck' it
    -- gives, this phase
    PlayedTopOfDeckThisPhase
  | -- | on a hero: it is considered to have the sphere's printed resource icon while paying
    -- for this card, as Gandalf (The Road Darkens) for the top card of the deck
    PrintedResourceIconForCard CardId Sphere
  | -- | on an enemy: it cannot engage the player, by engagement check or choice, as
    -- Pippin's
    CannotEngagePlayer PlayerId
  | -- | on a player: their undefended attacks' damage may go to this character instead of
    -- a hero, as White Tower Watchman's
    UndefendedDamageMayGoTo Target
  | -- | on the game: the next encounter card revealed, if of this type, is discarded
    -- without resolving, as Minas Tirith Lampwright's
    DiscardNextRevealed CardType
  | -- | on the game: the next enemy revealed from the encounter deck is dealt this much
    -- damage by the effect's source, as Expecting Mischief reads
    DamageNextRevealedEnemy Int
  | -- | on the game: all damage from the enemy's attack is canceled, as Bilbo's Magic Ring
    -- cancels it, whatever the enemy is immune to
    AttackDamageCanceled EnemyId
  | -- | on a card: no time counter can be removed from it, as Tharbad Hideout reads for
    -- the current quest
    CannotRemoveTimeCounters
  | -- | on a player, or on the game for every player: the next card played from hand
    -- gains Doomed N, as Gríma's Action reads; spent with it
    NextCardGainsDoomed Int
  | -- | on a player: the X of the next Doomed X event they play is lowered by this much, as
    -- Saruman's Staff reads; spent with it
    LowerNextDoomed Int
  | -- | alongside a reduction of the next matching card's cost, on the same effect: the
    -- card that spends it gets these modifiers until the end of the round, as the allies
    -- Man the Walls lowers cannot quest
    SpentOnCardGives [ModifierType]
  | -- | on a card in hand, as its card id: its player may give it Doomed N as they play
    -- it from hand, as Greyflood Wanderer reads; it then gains 'GainKeyword' of it
    MayGainDoomed Int
  | -- | on an enemy or location: it is considered out of play, while the card giving it this
    -- is in play, as Saruman's chosen card; no matcher finds it
    ConsideredOutOfPlay
  | -- | on an ally, or its card: it returns to its owner's hand at the end of the round, as
    -- the Eagle Gwaihir puts into play does
    ReturnToHandAtEndOfRound
  | -- | on a card in hand or in play, as its card id: at the end of the round it is shuffled
    -- into its owner's deck, as the card Message from Elrond passes is
    ShuffleIntoOwnersDeckAtEndOfRound
  | -- | on a hero: a player spent resources from its pool this round
    SpentResourcesThisRound
  | -- | on an enemy: defeated, it stays where it is for its own Forced effect to place it,
    -- as Mugash is attached to a hero
    StaysInPlayWhenDefeated
  | -- | on a player: they refresh no more than this many characters in the refresh phase,
    -- as Heart of Fangorn reads
    RefreshAtMost Int
  | -- | on a player: they draw this many cards in the resource phase instead of 1, as
    -- Hithaeglir Stream reads (0: they draw none, as Plains of Enedwaith)
    ResourcePhaseDraws Int
  | -- | on a player: their threat cannot be reduced, as The Empty Mug reads
    CannotReduceThreat
  | -- | on a player: their threat cannot be reduced below this, as The Paths of the Dead
    -- keeps it at the initial threat level
    CannotReduceThreatBelow Int
  | -- | on a player: a player card reduces their threat by at most this much, as Mines of
    -- Cirith Gurat's "reduce that amount to 1" reads
    PlayerCardThreatReductionLimit Int
  | -- | on a player: the threat at which they are eliminated, 50, moves by this much, as
    -- Double Dealings lowers it by 10
    EliminationThreatModifier Int
  | -- | on an enemy: the damage its attack deals raises the player's threat as much, as
    -- Seedy Inn's shadow reads
    AttackDamageRaisesThreatOf PlayerId
  | -- | on the game: each enemy gets +1 threat for each damage on it, as Orcish Howls reads
    EnemiesGetThreatPerDamage
  | -- | on an enemy: the damage its attack would deal is canceled, and the defending
    -- player's threat raised as much instead, as The Two Watchers reads
    AttackDamageToThreatInstead
  | -- | on an enemy: for each point of damage its attack deals, the player discards a card at
    -- random from their hand, as Northern Chetwood's shadow reads
    AttackDamageDiscardsRandomFromHandOf PlayerId
  | -- | on an enemy: each point of damage its attack deals removes 1 progress from each
    -- quest card in play, as Traitorous Wight's shadow reads
    AttackDamageRemovesProgress
  | -- | on a location in the staging area: the first this much progress that would be placed
    -- on a quest card each round is placed on it instead, as Forbidden Descent reads
    TakesQuestProgressFirst Int
  | -- | on a character: while defending against an enemy with the trait, its defense is
    -- raised by this much, as Orders from Angmar reads
    DefenseBonusAgainst Trait Int
  | -- | on a character: while defending against a matching enemy, its defense is raised by
    -- this much, as Silverthorn reads
    DefenseBonusAgainstMatching EnemyMatcher Int
  | -- | on a player: in the refresh phase they raise their threat by this much instead of 1,
    -- as Escape from Mount Gram's campaign card reads
    RefreshThreatRaise Int
  | -- | on the game: as the encounter discard pile is reset into the empty deck, its topmost
    -- matching card goes to the staging area first, as Ghostly Ruins reads
    ReturnBeforeEncounterDeckResets CardMatcher
  | -- | on an enemy: still in play as the phase ends, it raises the player's threat by this
    -- much, as Crazed Captive's shadow reads
    RaisesThreatAtEndOfPhase PlayerId Int
  | -- | on an enemy: as the quest phase ends, it engages the player with the highest threat,
    -- as Guffawing of Giants has its Stone-giant do
    EngagesHighestThreatAtEndOfQuestPhase
  | -- | on the game: as the phase ends, each enemy in the staging area engages the first
    -- player, as The Ring Draws Them reads
    StagingEnemiesEngageFirstPlayerAtEndOfPhase
  | -- | on an enemy: each hero its attack damages but does not destroy is taken prisoner, as
    -- Torture Master's shadow reads
    AttackDamageTakesPrisoner
  | -- | on the game: the encounter discard pile is reset into the deck as soon as the deck
    -- is empty, in any phase, as Journey Along the Anduin's nightmare setup card reads
    EncounterDeckResetsWheneverEmpty
  | -- | on the game: an empty encounter deck is not reset from the discard pile, even in the
    -- quest phase, as Roast Mutton advances once no cards are left in it
    EncounterDeckDoesNotReset
  | -- | on an enemy: the damage its attack deals past the character's remaining hit points
    -- is placed on the active location, as Collapsed Tower's shadow reads
    ExcessDamageToActiveLocation
  | -- | on an enemy: for each point of damage its attack deals past the character's
    -- remaining hit points, 1 progress is removed from each quest card in play, as Cruel
    -- Mountain-troll reads
    ExcessDamageRemovesQuestProgress
  | -- | on an enemy: damaged characters cannot attack it or defend against it, as Goblin
    -- Pursuer reads
    NotFoughtByDamaged
  | -- | on a player: before he commits his first character to the quest he discards a
    -- card at random from his hand, and with none he cannot commit, as A Weary Passage reads
    DiscardRandomToCommit
  | -- | on a player: player card effects cannot make him draw cards, as No End in Sight
    -- reads; the draw of the resource phase is not one, nor is an encounter card's
    CannotDrawCardsFromPlayerCardEffects
  | -- | on a location in the staging area: progress is placed on it before it can be placed
    -- on the active location, as Grim Foothills reads
    TakesProgressBeforeActiveLocation
  | -- | on a location in the staging area: progress is placed on it before it can be placed
    -- on the active location or the current quest, as Mines of Moria reads
    TakesProgressBeforeQuest
  | -- | on an active location: progress is placed on each other active location before it
    -- can be placed here, as Doors of Durin reads; questing progress fills the others first
    TakesProgressAfterOtherActiveLocations
  | -- | on the game: progress questing successfully would place on the quest is dealt as
    -- damage to the enemy instead, as Ride Them Down reads
    ProgressAsDamageTo Target
  | -- | on a character: it was dealt damage while defending the attack being resolved
    DamagedWhileDefending
  | -- | on an enemy: whatever would have it engage another player, it engages this one
    -- instead, as Chief Turch engages the first player
    EngagesOnly PlayerId
  | -- | on the game: progress questing successfully would place on the quest card that
    -- gives this is canceled instead, as Searching for Mugash reads; the card is told how
    -- much by 'ProgressCanceled'
    QuestProgressCanceledBy Source
  | -- | on a location in the staging area: nothing moves it out, to be active, to a deck or
    -- out of the game, as The Crossing at Tharbad reads; explored, it still leaves play. On
    -- an enemy there: it is not engaged, moved or discarded, as The Black Serpent reads;
    -- give it 'CannotBeOptionallyEngagedBy' too, for the players not to be offered it.
    -- On a card in the Orcs' area, the same holds of that area (Race Across Harad)
    CannotLeaveStagingArea
  | -- | on a hero: player card effects cannot give it resources, as No End in Sight reads;
    -- encounter card effects still may
    CannotGainResourcesFromPlayerCardEffects
  | -- | on a hero: only treasure cards' effects can give it resources, as On the Doorstep's
    -- Bilbo Baggins reads "cannot gain resources from non-treasure cards"
    CannotGainResourcesFromNonTreasureCards
  | -- | on a character: it counts as two when its controller's Dwarf characters are
    -- counted, as Bombur reads
    CountsAsTwoDwarves
  | -- | on a hero: control of it goes back to the player at the end of the phase, as
    -- Desperate Alliance gives it away until then
    ControlReturnsAtEndOfPhase PlayerId
  | -- | on an ally's card: it goes to the bottom of its owner's deck at the end of the
    -- phase, as To me! O my kinsfolk! puts one into play
    BottomOfDeckAtEndOfPhase
  | -- | on a player: he played the card this round, from wherever, paid for or not; one
    -- per card played. Read them with 'cardsPlayedThisRound'
    PlayedCardThisRound CardCode CardType
  | -- | on a player: he played a card this phase, as "the first card each player plays each
    -- phase" counts (The Long Arm of Mordor)
    PlayedACardThisPhase
  | -- | on a player: they draw this many more cards in the resource phase, in the same
    -- draw, as Bilbo Baggins reads
    AdditionalResourcePhaseDraws Int
  | -- | on the game: each location in the staging area takes the modifier, one that
    -- enters it later too, for as long as the effect lasts, as Treacherous Fog reads
    EachLocationInStagingArea ModifierType
  | -- | on the game: as 'EachLocationInStagingArea', each matching one, as Vastness of
    -- Mirkwood's "each non-objective location in the staging area"
    EachMatchingLocationInStagingArea CardMatcher ModifierType
  | -- | on the game: a hero committed to the quest this round, as The Rear Guard reads
    HeroCommittedThisRound
  | -- | on the game: the players quested successfully this phase, as Bofur reads
    QuestedSuccessfullyThisPhase
  | -- | on the game: the players quested unsuccessfully this round, as Melanwar reads
    QuestedUnsuccessfullyThisRound
  | -- | on a card, a lasting effect of its own: while it lasts the card gives each
    -- encounter card in play immunity to player card effects, as The Threat to
    -- Wilderland's "all encounter cards are immune to player card effects"
    EncounterCardsImmune
  | -- | on the game, until the end of the phase: if the players quest unsuccessfully, the
    -- effect's source places this much progress on the target, as Dark Clouds reads
    ProgressIfQuestUnsuccessful Target Int
  | -- | on an enemy, until the end of the phase: if the players quest successfully, the
    -- effect's source deals it the first amount of damage, the second instead if it is
    -- guarding a card, as Bilbo Baggins's (Tactics) reads
    DamageIfQuestSuccessful Int Int
  | -- | on a player, until the end of the phase: if the players quest successfully, the
    -- effect's source reduces his threat by this much, as Frodo Baggins's (Leadership) reads
    LowerThreatIfQuestSuccessful Int
  | -- | on the game: it cannot end, a win or loss waiting until nothing gives this, as
    -- Ghulat reads
    GameCannotEnd
  | -- | on the game: the outcome the game ends with once it can end
    GameEndsWith Outcome
  | -- | on a quest card or a location: progress placed on it is reduced by this much each
    -- time, as Orc Grunts reduces it on the current quest
    ProgressReducedBy Int
  | -- | on the game: shadow cards still unresolved are not discarded at the end of the
    -- combat phase, as The Clutches of Carn Dûm reads
    KeepUnresolvedShadowCards
  | -- | on the game: each shadow card with no shadow effect has one, which the modifier's
    -- source resolves as 'ResolveShadowCard', as Mountains of Angmar gives
    ShadowlessCardsGainEffect
  | -- | on the game: questing unsuccessfully raises no player's threat, as Doom Hangs
    -- Still reads
    NoThreatFromQuestingUnsuccessfully
  | -- | on the game: the quest phase is skipped, as Doom Hangs Still reads
    SkipQuestPhase
  | -- | on the game: questing unsuccessfully deals the difference as damage to the card
    -- instead of raising each player's threat, as the Assault keyword reads (MEC54 rules)
    QuestFailureDamagesInstead Target
  | -- | on the game: the round goes on from the phase before to the one after, which does
    -- not begin or end, as The Great River skips the encounter phase, Henneth Annûn the
    -- encounter and combat phases, and The Garrison of Gondor the combat phase
    SkipPhase Phase
  | -- | on the game: a player with no cards in his deck is eliminated, as Deadmen's Dike's
    -- stages read
    EliminatedWithEmptyDeck
  | -- | on the game: player card effects cannot take a card out of a player's discard
    -- pile, as The Power of Angmar reads
    DiscardPilesLocked
  | -- | on the game: event cards cannot shuffle cards from a player's discard pile into his
    -- deck, as Passage into Mordor reads
    EventsCannotShuffleDiscardIntoDeck
  | -- | on a player: he may optionally engage 1 additional enemy in the encounter phase,
    -- as Halbarad reads
    OptionallyEngagesAdditionalEnemy
  | -- | on a character: its willpower cannot be reduced, as Star Brooch reads
    CannotHaveWillpowerReduced
  | -- | on the game: the resolving enemy attacks step of the combat phase has begun, as
    -- Tireless Hunters reads
    ResolvingEnemyAttacks
  | -- | on the game: the resolving enemy attacks step of the combat phase is complete, as
    -- Helm! Helm! reads
    EnemyAttacksStepComplete
  | -- | on the game: each ally that leaves play goes to its owner's hand instead of the
    -- discard pile, as Rallying Cry's Valour Action reads
    AlliesLeavePlayToHand
  | -- | on a character: it quests with its attack instead of its willpower, or instead of
    -- its defense under siege, as Grappling Hook reads
    QuestsWithAttack
  | -- | on a character: it can commit to quests while exhausted, as Lindon Navigator reads
    CanCommitWhileExhausted
  | -- | on a location in the staging area: progress from questing successfully may be
    -- placed on it before it is placed on the current quest, as Explorer's Almanac reads
    MayTakeQuestingProgress
  | -- | on the game: each Sailing test looks at this many more cards, as Calm Waters reads
    SailingTestAdditionalCards Int
  | -- | on the game: the first sailing success icon each Sailing test finds is canceled,
    -- as Rough Waters reads
    CancelFirstSailingIcon
  | -- | on a character: it can be committed to a Sailing test whoever controls it, as the
    -- Dream-chaser
    CommittableToAnySailingTest
  | -- | on a character: committed to a Sailing test it counts as this many characters, as
    -- the Dream-chaser counts as 2
    CountsAsSailingCharacters Int
  | -- | on a character: committed to a Sailing test it counts as this many characters more,
    -- as Enlarged Quarters' "1 additional character"
    AdditionalSailingCharacters Int
  | -- | on a Ship enemy: its Boarding X is raised (lowered, when negative) by this much,
    -- as Cobas Haven reduces it
    BoardingModifier Int
  | -- | on an enemy: the damage its attack deals past the character's remaining hit points
    -- is dealt to a Ship objective the defending player controls, as Sudden Storms' shadow
    ExcessDamageToShipObjective
  | -- | on an enemy: the damage its attack deals past the character's remaining hit points
    -- is placed on an Aflame location in the staging area, as Elite Marauder reads
    ExcessDamageToAflameLocation
  | -- | on an enemy: the player's characters cannot attack it, as Umbar Captain reads for
    -- Ship enemies
    CannotBeAttackedByCharactersOf PlayerId
  | -- | on an enemy: only Ship objectives can be declared as attackers against it, as
    -- Raider Flagship reads
    OnlyAttackedByShipObjectives
  | -- | on an enemy: each damage its attack deals adds 1 to the total threat in the
    -- staging area until the end of the phase, as Ramming Speed! reads
    AttackDamageAddsStagingThreat
  | -- | on a player: his deck cannot be searched, as Soulless Cadaver reads: a search of it
    -- finds nothing
    CannotSearchDeck
  | -- | on a location: it cannot be flipped, as Powerful Undertow reads for the active one
    CannotBeFlipped
  | -- | on a player: his threat counts as no more than this, as Vanish from Sight reads
    ThreatCountsAsAtMost Int
  | -- | on the game: the quest phase's staging step is over, as Elevenses reads
    AfterStagingStep
  | -- | on the game: the encounter phase's engagement checks are made, as Arrows from the
    -- Trees reads
    AfterEngagementChecks
  | -- | on the game: an enemy was engaged this phase, as Arrows from the Trees reads
    EnemyEngagedThisPhase
  | -- | on a hero: its resources cannot pay for matching cards, as Na'asiyah's for allies
    CannotPayFor CardMatcher
  | -- | on a player: he may declare up to this many defenders against the attack, as Hold
    -- the Line reads
    MayDeclareUpToDefenders Int
  | -- | on the game: each character defending the attack that takes no damage from it
    -- readies, as Hold the Line reads
    ReadyUndamagedDefenders
  | -- | on an enemy, for an attack: the attack deals no damage, though it resolves, its
    -- shadow cards too, as Frodo Baggins (The Road Darkens) makes it
    AttackDealsNoDamage
  | -- | on a player: he is eliminated at the end of the round, as Justice Shall Be Done reads
    EliminatedAtEndOfRound
  | -- | on a player: he draws 1 card each time he attacks and destroys an enemy, as
    -- Knife-work reads
    DrawsOnDestroyingEnemy
  | -- | on a card in hand, as its card id: its player may exhaust ready characters he
    -- controls with the trait as he plays it, each lowering its cost by 1, as The Houses of
    -- Healing reads for Healers
    ReducedByExhausting Trait
  | -- | on the game: the staging step of this quest phase has begun
    StagingStepBegun
  | -- | on a location on the Island Map adjacent to the active location: it is not
    -- considered to be in the staging area, as Jagged Cliffs reads
    NotInStagingArea
  | -- | on the active location on the Island Map: the players may travel from it to a
    -- location up to so many locations away, as Winding Caverns reads
    TravelRange Int
  | -- | on the game: archery damage assigned to a character keeps going to it until it is
    -- destroyed, as Ithilien Overlook reads
    ArcheryDamageStaysOnCharacter
  | -- | on the game: archery damage may be assigned to this card as well as to characters,
    -- as Southron Mounted Archer gives Battleground locations
    ArcheryDamageMayGoTo Target
  | -- | on an enemy: the damage its attack deals past the character's remaining hit points
    -- is dealt to this card, as Dense Thickets' shadow sends it to a Ranger objective
    ExcessDamageTo Target
  | -- | on a player: they cannot discard cards from their hand, as Isen River reads
    CannotDiscardFromHand
  | -- | on a player: he cannot have more than this many cards in hand, discarding down to
    -- it at once, as In Enemy Territory reads
    HandSizeLimit Int
  | -- | on an objective: the first player claims it as soon as nothing guards it, attaching
    -- it to a hero he controls, as The Black Serpent's stages read of each Harad objective
    -- that is not attached
    FirstPlayerClaimsWhenFree
  | -- | on the game, or a character: player card effects cannot ready it, as Find Water
    -- reads; the refresh phase still does
    CannotBeReadiedByPlayerCards
  | -- | on a character: it cannot defend against matching enemies, as Uruk of Mordor reads
    -- for allies with less willpower than its threat
    CannotDefendAgainst EnemyMatcher
  | -- | on a character: it cannot attack or defend against matching enemies, as Cursed
    -- Burden reads
    CannotFight EnemyMatcher
  | -- | on a character: it cannot attack matching enemies, as Trial of Ancestors reads
    CannotAttackMatching EnemyMatcher
  | -- | on a card: its effect on "the first" card each phase has happened, as Dark Pathway reads
    FirstEffectUsed
  | -- | on a location: progress placed on it at once is reduced to this much, as Celebrimbor's
    -- Secret's nightmare setup card reads for the staging area
    ProgressReducedTo Int
  | -- | on a location: player card effects cannot discard it from play, as The Antlered Crown's
    -- nightmare setup card reads for locations with time counters
    CannotBeDiscardedByPlayerCards
  | -- | on an enemy: damage dealt to it other than by an attack is canceled, as Dunlending Horde
    -- reads
    CannotTakeNonCombatDamage
  | -- | on an enemy: its defense cannot be reduced, as Rupert's in nightmare mode
    CannotHaveDefenseReduced
  | -- | on a character: it loses the trait, as Shot from the Sky takes the Eagle trait
    LoseTrait Trait
  | -- | on a character: an effect of any card but the one with this code heals it of no
    -- more than this, as Noxious Supplies reads for Athelas
    HealLimitExceptFrom Int CardCode
  | -- | on an enemy: characters with the keyword cannot attack it, as Pitch-dark
    -- Thicket's shadow reads for ranged
    CannotBeAttackedByKeyword Keyword
  | -- | on the game: each escape test's dealt cards come to this much more, once, as
    -- Stagnant Quagmire adds its Escape value
    EscapeTestBonus Int
  | -- | on a player: he commits no more than this many characters to each escape test,
    -- as Ill-fated Guard reads
    EscapeTestCommitLimit Int
  | -- | on the game, or a player for his own tests: each Hide X value is this much more, as
    -- Woody End lowers it and Evil Crow raises it
    HideValue Int
  | -- | on the game or player: at most this many characters may commit to a Hide test
    HideCommitLimit Int
  | -- | on the game, or a player for his own tests: the discarded cards of each Hide test come
    -- to this much more threat, as Stock Road adds 2
    HideTestThreat Int
  | -- | on the game or player: each character committed to a Hide test gets this much more
    -- willpower for it, as Green Hill Country gives
    HideTestWillpower Int
  | -- | on a player: he has failed a Hide test this phase, as Crawling Towards Him reads
    FailedHideTestThisPhase
  | -- | on a player: he has failed a Hide test this round, as Green Hill Country's shadow
    -- reads
    FailedHideTestThisRound
  | -- | on the game: each player draws this many more cards in his starting hand, as
    -- Gandalf's Delay has each draw 1 less
    StartingHandSize Int
  | -- | on the game: the players cannot win, a win waiting until nothing gives this, as
    -- The Blood of Gondor's Nightmare setup card reads
    PlayersCannotWin
  | -- | on an enemy: a hero with no resources in its pool can neither attack nor defend
    -- against it, as Drúadan Warrior reads
    NotFoughtByHeroesWithoutResources
  | -- | on an enemy: to optionally engage it, the player spends this many resources, as
    -- Drúadan Stalker reads
    OptionalEngagementCost Int
  | -- | on the game: the progress questing makes beyond the active location's quest points
    -- goes to this location, not the quest, as Assault on Osgiliath's Nightmare setup card
    -- reads
    ProgressBeyondActiveTo Target
  | -- | on a card: progress that would be placed on it is placed on the target instead, as
    -- Blinding Blizzard takes the quest's and Snow locations'
    ProgressPlacedOnInstead Target
  | -- | on the game: characters cannot gain willpower from player card effects, as Frozen
    -- Silverlode reads
    NoWillpowerFromPlayerCards
  | -- | on an enemy: for each point of damage its attack deals, the player discards a random
    -- card from his hand, as Banks of Sirannon's shadow reads
    AttackDamageDiscardsRandomFrom PlayerId
  | -- | on an enemy: each point of damage its attack deals heals 1 damage from the target, as
    -- The Dark Fire heals Durin's Bane
    AttackDamageHeals Target
  | -- | on an enemy: each point of its attack's damage discards the top card of the player's
    -- captured deck, as Orc of Thaurdir's shadow reads (Escape from Mount Gram)
    AttackDamageDiscardsCaptured PlayerId
  | -- | on a player: card effects draw him no cards, the resource phase's draw still does, as
    -- Isolated reads
    CannotDrawCardsFromCardEffects
  | -- | on a card: it is considered to have no resources, as Captain's Quarters reads of
    -- Raider characters
    HasNoResources
  | -- | on the game: archery damage must be dealt to Ship objectives, as City Under Siege
    -- reads
    ArcheryDamageToShipObjectives
  | -- | on the game: no more than 1 ally can enter play each round, however it enters, as
    -- Corsair Waters reads; on a player, no more than 1 under him, as The Spider's Pass
    OneAllyEntersPlayPerRound
  | -- | on the game: no ally can enter play, however it enters, as The Crack of Doom reads
    AlliesCannotEnterPlay
  | -- | on the game: the matching active location adds its threat to the staging area, as
    -- Flooded Ruins reads
    ActiveLocationsAddThreat LocationMatcher
  | -- | on the game: a treachery card has been revealed this round
    TreacheryRevealedThisRound
  | -- | on the game, by an effect until the end of the phase: the treachery was revealed
    -- this phase, as "the first treachery revealed each quest phase" counts them
    TreacheryRevealedThisPhase
  | -- | on the game: as an additional cost to bypass a quest card, each player raises his
    -- threat by this much, as Shadowed Corridor reads
    BypassCostRaisesThreat Int
  | -- | on a location: it cannot enter the staging area, as Shrine to Morgoth reads
    CannotEnterStagingArea
  | -- | on a character: card effects cannot ready it, the refresh phase still does, as
    -- Twisting Hollow reads
    CannotReadyFromCardEffects
  | -- | on a character: the refresh phase is readying it, not a card effect
    ReadiedByRefresh
  | -- | on a quest card: this much progress that would have been placed on it was canceled
    -- this round
    ProgressCanceledThisRound Int
  | -- | on a card in play: player card effects cannot damage it, as The Watcher's in the
    -- staging area; its other effects still reach it
    CannotBeDamagedByPlayerCardEffects
  | -- | on an enemy: the player's card effects cannot damage it, as Marshland Outlaws bars
    -- them from other enemies
    CannotBeDamagedByEffectsOf PlayerId
  | -- | on an enemy: the damage its attacks deal goes to one of these instead, as Orc
    -- Saboteur's goes to a Battleground location
    AttackDamageRedirectedTo Target
  | -- | on a card in play: no player attachment can be attached to it; encounter cards
    -- still may, as Stuart's Trolls
    CannotHavePlayerAttachments
  | -- | on an enemy in the staging area: it attacks matching players during the combat
    -- phase, as if engaged with them, without counting as engaged, as Snow-Giant
    AttacksAsIfEngagedWith PlayerMatcher
  | -- | on a player: no card he controls can ready, as Ungoliant's Brood reads
    ControlledCardsCannotReady
  | -- | on an enemy: no more than this many characters can attack it each round, as Graxar
    AttackersPerRoundLimit Int
  | -- | on an enemy: no more than this many characters can attack it at a time, as The
    -- Black Serpent of The Battle of the Pelennor Fields
    AttackersPerAttackLimit Int
  | -- | on an enemy, by an effect until the end of the round: a character was declared as
    -- an attacker against it, for 'AttackersPerRoundLimit' to count
    DeclaredAttackerThisRound
  | -- | on an enemy: if its attack deals damage, the target gets the modifier until the end
    -- of the round, as Giant Scorpion's shadow bars the defender from readying
    AttackDamageModifiesUntilEndOfRound Target ModifierType
  | -- | on an enemy: each character its attack damages gets the modifier until the end of
    -- the round, as Port of Pelargir's shadow bars it from readying
    AttackDamageModifiesDamagedUntilEndOfRound ModifierType
  | -- | on the game: allies enter play exhausted, as Flight to Gondor reads
    AlliesEnterPlayExhausted
  | -- | in a lasting effect: its modifiers apply only while the card shows the side of this
    -- code, as Lost Underground blanks On Track's Forced effects and not Off Track's
    WhileShowing CardCode
  | -- | on the game: progress cannot be placed on quest cards, as Chaos in the Night reads
    NoProgressOnQuestCards
  | -- | on the game: an instruction to remove quest progress places 1 progress on
    -- this target instead (The Battle of Five Armies Nightmare, stage 5B)
    QuestProgressRemovalBecomes Target
  | -- | on the game: the total threat of the cards in the Orcs' area is added to the staging
    -- area's, as Howling at their Heels reads (Race Across Harad)
    OrcsAreaThreatAddsToStaging
  | -- | on a character: it takes no damage but from the source, as Bilbo Baggins in the
    -- riddle area takes none from cards other than Gollum (Dungeons Deep and Caverns Dim)
    CannotTakeDamageExceptFrom Source
  | -- | on the game: the first player is answering a riddle, past its discards and before
    -- its matches are counted, when Bilbo's Magic Ring's first Action may be taken
    AnsweringRiddle
  | -- | on the game: at the end of the combat phase the matching enemies return to the
    -- staging area, as Great Gray Wolf's Forced effect reads
    ReturnToStagingAtEndOfCombat EnemyMatcher
  | -- | One by One returns the destroyed character's Sack to the top, rather than
    -- shuffling it into the sack deck as it normally would when leaving play.
    LeavesPlayToTopOfSackDeck
  deriving stock (Show, Eq, Generic)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)

-- | How many characters a character committed to a Sailing test counts as, by its
-- modifiers: 1, or as many as it counts as, plus any additional.
sailingCharacters :: [ModifierType] -> Int
sailingCharacters ms = fromMaybe 1 (headMay [n | CountsAsSailingCharacters n <- ms]) + sum [n | AdditionalSailingCharacters n <- ms]

{- | A saved game from before one 'PlayedCardThisRound' marker per card played holds the
old markers, in its effects and in its modifiers. 'PlayedThisRound' was the once-per-round
flag 'UsedThisRound' is now, and 'PlayedCardTypeThisRound' loads as a card of the type
played, its code unknown. The rest were always saved beside a 'PlayedCardTypeThisRound'
and say nothing more: each loads as a use keyed by no card, which nothing reads. Anything
else is read by the generated parser.
-}
parseLegacyModifierType :: (Value -> Parser ModifierType) -> Value -> Parser ModifierType
parseLegacyModifierType generated v = case v of
  Object o -> case (KeyMap.lookup "tag" o, KeyMap.lookup "contents" o) of
    (Just (String "PlayedThisRound"), Just contents) -> UsedThisRound <$> parseJSON contents
    (Just (String "PlayedCardTypeThisRound"), Just contents) -> PlayedCardThisRound "" <$> parseJSON contents
    (Just (String tag), _)
      | tag `elem` ["PlayedACardThisRound", "PlayedEventThisRound", "PlayedAnAllyThisRound", "AllyPlayedThisRound" :: Text] ->
          pure $ UsedThisRound ""
    _ -> generated v
  _ -> generated v

-- Direct codecs avoid specializing Generic traversal for this large sum.
$(do
  toModifierType <- deriveToJSON defaultOptions ''ModifierType
  parser <- mkParseJSON defaultOptions ''ModifierType
  fromModifierType <- [d|instance FromJSON ModifierType where parseJSON = parseLegacyModifierType $(pure parser)|]
  modifier <- deriveJSON defaultOptions ''Modifier
  pure $ toModifierType <> fromModifierType <> modifier)
