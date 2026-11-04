module LordOfTheRings.Matcher.Window (module LordOfTheRings.Matcher.Window) where

import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types (AllyMatcher, CardMatcher, EnemyMatcher, HeroMatcher, LocationMatcher, PlayerMatcher, cardMatches)
import LordOfTheRings.Modifier (Modifier)
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Timing
import LordOfTheRings.Trait
import LordOfTheRings.Window

data WindowMatcher
  = EntersPlayWindow Timing Target
  | PhaseBeginsWindow Timing Phase
  | CommittedToQuestWindow Timing Target
  | -- | any character committed to the quest, as The Water's Edge reads
    AnyCommittedToQuestWindow Timing
  | EnemyEngagedWindow Timing EnemyId
  | -- | any enemy engaged with the player
    PlayerEngagedEnemyWindow Timing PlayerId
  | OptionallyEngagedWindow Timing PlayerId
  | EnemyEntersStagingAreaWindow Timing
  | -- | an enemy with the trait added to the staging area, as Amon Forn reads
    EnemyWithTraitEntersStagingAreaWindow Timing Trait
  | GainedResourcesWindow Timing Target
  | DealtDamageWindow Timing Target
  | TraveledWindow Timing LocationId
  | -- | the card exhausted
    ExhaustedWindow Timing Target
  | AnyTraveledWindow Timing
  | -- | an enemy destroyed by an attack, by any attackers or with the given one among them
    EnemyDestroyedByAttackWindow Timing (Maybe Target)
  | -- | an enemy with the trait destroyed by an attack the character was among
    EnemyWithTraitDestroyedByAttackWindow Timing Trait Target
  | {- | an enemy engaged with a player other than the given one, destroyed by an attack
    the character was among
    -}
    EnemyOfOtherPlayerDestroyedByAttackWindow Timing PlayerId Target
  | CharacterDestroyedWindow Timing
  | HeroDestroyedWindow Timing
  | AllyDestroyedWindow Timing
  | EnemyRevealedWindow Timing
  | -- | an enemy with the trait revealed from the encounter deck, as Goblin Tunnels reads
    EnemyWithTraitRevealedWindow Timing Trait
  | -- | any enemy engaged with any player, as Knees of the Mountain reads
    AnyEnemyEngagedWindow Timing
  | -- | an enemy with the modifier, from that source, engaged a player, as Goblin Gate's
    -- ambusher does
    EnemyWithModifierEngagedWindow Timing Modifier
  | TreacheryRevealedWindow Timing
  | -- | a treachery with the trait revealed from the encounter deck, as Thaurdir answers a
    -- Sorcery one
    TreacheryWithTraitRevealedWindow Timing Trait
  | -- | an encounter side quest revealed from the encounter deck, as Iârion answers
    SideQuestRevealedWindow Timing
  | ShadowEffectWindow Timing
  | ExploredWindow Timing LocationId
  | -- | any location worth no victory points explored
    ExploredWithoutVictoryWindow Timing
  | PhaseEndsWindow Timing Phase
  | -- | at the end of the round
    RoundEndsWindow
  | -- | at the beginning of the staging step, as Search for an Exit flips
    StagingStepBeginsWindow
  | -- | at the end of the staging step, as The Fate of Balin reads
    StagingStepEndsWindow
  | ProgressPlacedWindow Timing Target
  | QuestedUnsuccessfullyWindow Timing
  | QuestedSuccessfullyWindow Timing
  | -- | after an attack the character, or any character, defended
    DefendedAttackWindow Timing (Maybe Target)
  | PlayedFromHandWindow Timing Target
  | LocationRevealedWindow Timing
  | ShadowEffectAgainstWindow Timing Target
  | AnyEnemyAttacksWindow Timing
  | -- | a character whose printed card matches left play
    CharacterLeftPlayWindow Timing CardMatcher
  | -- | after an attack the character made resolved
    AttackedWindow Timing Target
  | ShadowEffectResolvedWindow Timing EnemyId
  | -- | a shadow card dealt to an enemy engaged with the player
    ShadowCardDealtWindow Timing PlayerId
  | EnemyAttacksWindow Timing EnemyId
  | -- | characters attack the enemy, as "when it is attacked" reads
    EnemyAttackedWindow Timing EnemyId
  | -- | an encounter card revealed from the encounter deck with a When Revealed effect
    -- that can be canceled
    CardRevealedWindow Timing
  | -- | a character, or any character, declared as a defender
    DefenderDeclaredWindow Timing (Maybe Target)
  | -- | a character declared as a defender against the enemy, as Snow Warg reads
    DefenderDeclaredAgainstWindow Timing EnemyId
  | AllyLeftPlayWindow Timing
  | -- | a character, or any character, declared as an attacker
    AttackerDeclaredWindow Timing (Maybe Target)
  | -- | a hero with the trait, controlled by the player, left play
    HeroLeftPlayWindow Timing PlayerId Trait
  | -- | an ally with the trait, controlled by the player, left play
    AllyWithTraitLeftPlayWindow Timing PlayerId Trait
  | -- | a character, or one with the trait, readied during the refresh phase
    ReadiedWindow Timing (Maybe Trait)
  | -- | a character healed by anything but the source
    HealedByOtherWindow Timing Source
  | -- | an ally entered play, under the player and with the trait when given
    AllyEnteredPlayWindow Timing (Maybe PlayerId) (Maybe Trait)
  | -- | a card, or any card, discarded from the top of the player's deck
    DiscardedFromDeckWindow Timing PlayerId (Maybe CardId)
  | -- | any number of cards discarded from the top of a matching player's deck, as Fornost
    -- Square reads for the first player
    PlayerDiscardedFromDeckWindow Timing PlayerMatcher
  | -- | the When Revealed effects of a treachery canceled, as Ruins of Arnor reads
    TreacheryWhenRevealedCanceledWindow Timing
  | -- | the player played a matching card from their hand
    PlayedCardFromHandWindow Timing PlayerId CardMatcher
  | -- | any hero dealt damage
    HeroDealtDamageWindow Timing
  | -- | the character itself left play, for its own abilities to answer
    LeftPlayWindow Timing Target
  | -- | a character sharing a trait with the given one left play
    LeftPlaySharingTraitWindow Timing Target
  | -- | the character attacked alone, and dealt damage beyond what the enemy had left
    AttackedAloneWithExcessWindow Timing Target
  | -- | an enemy destroyed by an attack a matching hero made
    EnemyDestroyedByHeroWindow Timing HeroMatcher
  | -- | an encounter or quest card's effect raised the player's threat
    ThreatRaisedByEncounterWindow Timing PlayerId
  | -- | a player other than the given one raised his threat
    OtherPlayerRaisedThreatWindow Timing PlayerId
  | -- | an enemy engaged a player other than the given one, as Amarthiúl answers
    OtherPlayerEngagedEnemyWindow Timing PlayerId
  | ChangedControlWindow Timing Target
  | -- | any player plays an event
    EventPlayedWindow Timing
  | -- | an attack with a character of the trait among its attackers dealt damage
    AttackDealtDamageWithTraitWindow Timing Trait
  | -- | a location entered play, in the staging area
    LocationEntersPlayWindow Timing
  | -- | the players traveled to a matching location
    TraveledToWindow Timing LocationMatcher
  | -- | any player played a matching card from their hand
    CardPlayedWindow Timing CardMatcher
  | -- | a player other than the given one played an attachment on a hero the given one
    -- controls
    AttachmentPlayedOnHeroWindow Timing PlayerId
  | -- | a matching enemy attacked the player
    EnemyAttacksPlayerWindow Timing PlayerId EnemyMatcher
  | -- | a player engaged the enemy in the optional engagement step
    EnemyOptionallyEngagedWindow Timing EnemyId
  | -- | the active location explored, as "after the active location leaves play as an
    -- explored location" reads
    ActiveLocationExploredWindow Timing
  | -- | the player let an attack go undefended
    AttackUndefendedWindow Timing PlayerId
  | EnemyDefeatedWindow Timing
  | -- | the enemy was defeated, as "when attached enemy is destroyed" reads
    ThisEnemyDefeatedWindow Timing EnemyId
  | -- | any character dealt damage, as Honour Guard reads
    CharacterDealtDamageWindow Timing
  | -- | the enemy's attack, or any enemy's, destroyed a character
    CharacterDestroyedByAttackWindow Timing (Maybe EnemyId)
  | -- | a character with the trait was dealt damage, as Gondorian Discipline reads
    CharacterWithTraitDealtDamageWindow Timing Trait
  | -- | an attack the character was among dealt the enemy damage
    AttackDealtDamageByWindow Timing Target
  | -- | a card effect placed resources in the pool of a hero the player controls
    HeroGainedResourcesWindow Timing PlayerId
  | -- | an encounter card with surge revealed from the encounter deck
    SurgeCardRevealedWindow Timing
  | -- | a matching hero declared as a defender, while an enemy other than the attacker
    -- matches
    HeroDefenderDeclaredWindow Timing HeroMatcher EnemyMatcher
  | -- | any number of time counters removed from the card, or from any card
    TimeCountersRemovedWindow Timing (Maybe Target)
  | -- | any number of time counters removed from a quest stage, the current quest
    TimeCountersRemovedFromQuestWindow Timing
  | -- | the last time counter removed from the card
    LastTimeCounterRemovedWindow Timing Target
  | -- | the player raised his threat from the Doomed keyword
    ThreatRaisedByDoomedWindow Timing PlayerId
  | -- | the player, or any player, drew any number of cards
    DrewCardsWindow Timing (Maybe PlayerId)
  | -- | any location explored
    AnyExploredWindow Timing
  | -- | the character destroyed
    TargetDestroyedWindow Timing Target
  | -- | any encounter card revealed from the encounter deck, as Don't Be Hasty! answers
    -- before its keywords and When Revealed effects resolve
    EncounterCardRevealedWindow Timing
  | -- | the character defended an attack, taking no damage while defending it
    DefendedWithoutDamageWindow Timing Target
  | -- | a player card effect reduced any player's threat, as The Empty Mug reads
    ThreatReducedByPlayerCardWindow Timing
  | -- | the card entered the staging area, as Ruined Plaza reads
    CardEntersStagingAreaWindow Timing Target
  | -- | the players advanced to a quest stage, as Fen of Reeds reads
    AdvancedToStageWindow Timing
  | -- | a matching enemy attacked any player, as a Guardian enemy at Hill Barrow
    MatchingEnemyAttacksWindow Timing EnemyMatcher
  | -- | the player would be eliminated by reaching his threat elimination level, as Favor
    -- of the Valar reads
    EliminatedByThreatWindow Timing PlayerId
  | -- | the quest card was defeated, as The Long Defeat reads
    QuestDefeatedWindow Timing Target
  | -- | any quest card was defeated, as Mendor reads
    AnyQuestDefeatedWindow Timing
  | -- | a character the player controlled was destroyed
    OwnCharacterDestroyedWindow Timing PlayerId
  | -- | a character the controller of the host (a hero) controlled was destroyed, as
    -- Scarred reads "you control" of the hero it is attached to
    HostControllersCharacterDestroyedWindow Timing Target
  | -- | a card effect discarded an ally with the trait from play, as Éothain reads
    AllyWithTraitDiscardedByCardEffectWindow Timing Trait
  | -- | an attack the character took part in destroyed an enemy with at least this many
    -- printed hit points, as Sword of Númenor reads
    EnemyWithPrintedHitPointsDestroyedByAttackWindow Timing Int Target
  | -- | a matching enemy destroyed, as None Return reads "a non-unique enemy"
    EnemyDestroyedWindow Timing CardMatcher
  | -- | a matching location explored, as Leave No Trace reads "a non-unique location"
    ExploredMatchingWindow Timing CardMatcher
  | -- | a card discarded from the player's hand, as Silver Harp answers
    DiscardedFromHandWindow Timing PlayerId
  | -- | the card readied, by anything, as Wellinghall Preserver answers
    BecameReadyWindow Timing Target
  | -- | a matching hero declared as a defender, as Dori answers another hero
    HeroDeclaredAsDefenderWindow Timing HeroMatcher
  | -- | an encounter card revealed whose title is in the victory display, as The Door is
    -- Closed! answers
    RevealedTitleInVictoryDisplayWindow Timing
  | -- | it became Day or Night, as "when it becomes Night" reads
    BecomesWindow TimeOfDay
  | -- | any captured cards rescued (Escape from Mount Gram)
    CapturedCardsRescuedWindow Timing
  | -- | the player drew cards at the beginning of the resource phase, as Círdan the
    -- Shipwright reads
    DrewForResourcePhaseWindow Timing PlayerId
  | -- | the player drew their setup hand, as Galdor of the Havens answers instead of a
    -- mulligan
    SetupHandDrawnWindow Timing PlayerId
  | -- | a player committed characters to a Sailing test, as Swift Raider answers
    CommittedToSailingTestWindow Timing
  | -- | a matching location was turned over to its other side: read as it now is, as
    -- "after X is flipped over while active" reads
    LocationFlippedWindow Timing LocationMatcher
  | -- | a matching location became the active location
    LocationBecameActiveWindow Timing LocationMatcher
  | -- | a matching player raised his threat, as Sam Gamgee answers "a player"
    ThreatRaisedWindow Timing PlayerMatcher
  | -- | a shadow card's effect raised an enemy's attack, as Tides of Fate reads
    ShadowIncreasedAttackWindow Timing
  | -- | an enemy's attack destroyed a character with the trait defending it, as Last Stand
    DefenderWithTraitDestroyedWindow Timing Trait
  | -- | the player committed his characters to the quest, as Scouting Party reads
    PlayerCommittedCharactersWindow Timing PlayerId
  | -- | the card played from hand was paid for from a single hero's pool, as Master
    -- Ironsmith and Linhir Sea-captain read
    PaidFromOneHeroWindow Timing Target
  | -- | the attachment was attached to a hero, as Dúnedain Remedy answers its own
    AttachedToHeroWindow Timing Target
  | -- | an attachment with the trait was attached to a hero, as Rune-master answers a Signal
    TraitAttachedToHeroWindow Timing Trait
  | -- | the character was removed from the quest, as Windfola answers
    RemovedFromQuestWindow Timing Target
  | -- | any player played a matching card from his hand, as Hithlain answers a Silvan ally
    AnyPlayedCardFromHandWindow Timing CardMatcher
  | -- | a character other than the given one would be dealt damage, as Vigilant Guard
    -- answers
    AnotherCharacterDealtDamageWindow Timing Target
  | -- | a matching character declared as a defender against a matching enemy, as Terrible
    -- to Behold answers a Noble against an engaged enemy
    CharacterDefenderDeclaredWindow Timing HeroMatcher AllyMatcher EnemyMatcher
  | -- | the first window when the player plays with the FAQ's errata, else the second, as
    -- the window a card's errata changed
    ErrataWindow PlayerId WindowMatcher WindowMatcher
  | -- | an objective with the card code exhausted, as "After Cave Torch exhausts" reads
    ObjectiveExhaustedWindow Timing CardCode
  | -- | a matching enemy engaged a player, as "When a Goblin enemy engages a player" reads
    MatchingEnemyEngagedWindow Timing EnemyMatcher
  | -- | a matching enemy was dealt damage, as Orc Taskmaster answers a Goblin
    MatchingEnemyDealtDamageWindow Timing EnemyMatcher
  | -- | a quest card was bypassed
    QuestBypassedWindow Timing
  | -- | the enemy was turned faceup as a hidden card, as Orc Trickster answers (The Blood of
    -- Gondor)
    HiddenCardTurnedFaceupWindow Timing Target
  | -- | a matching enemy revealed from the encounter deck, as Concealed Woods reads one with
    -- ambush
    MatchingEnemyRevealedWindow Timing EnemyMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

windowMatches :: WindowMatcher -> Window -> Bool
windowMatches matcher (Window timing wType) = case (matcher, wType) of
  (EntersPlayWindow t target, EntersPlay target') -> t == timing && target == target'
  (PhaseBeginsWindow t phase, PhaseBegins phase') -> t == timing && phase == phase'
  (CommittedToQuestWindow t target, CommittedToQuest target') -> t == timing && target == target'
  (AnyCommittedToQuestWindow t, CommittedToQuest _) -> t == timing
  (EnemyEngagedWindow t eid, EnemyEngaged eid' _) -> t == timing && eid == eid'
  (AnyEnemyEngagedWindow t, EnemyEngaged _ _) -> t == timing
  (PlayerEngagedEnemyWindow t pid, EnemyEngaged _ pid') -> t == timing && pid == pid'
  (OptionallyEngagedWindow t pid, OptionallyEngaged _ pid') -> t == timing && pid == pid'
  (EnemyEntersStagingAreaWindow t, EntersStagingArea (EnemyTarget _)) -> t == timing
  (GainedResourcesWindow t target, GainedResources target') -> t == timing && target == target'
  (DealtDamageWindow t target, DealtDamage target' _) -> t == timing && target == target'
  (TraveledWindow t lid, Traveled lid') -> t == timing && lid == lid'
  (ExhaustedWindow t target, Exhausted target') -> t == timing && target == target'
  (AnyTraveledWindow t, Traveled _) -> t == timing
  (DefenderDeclaredWindow t who, DeclaredAsDefender target _) -> t == timing && maybe True (== target) who
  (DefenderDeclaredAgainstWindow t eid, DeclaredAsDefender _ eid') -> t == timing && eid == eid'
  (EnemyDestroyedByAttackWindow t who, EnemyDestroyedByAttack _ attackers _) -> t == timing && maybe True (`elem` attackers) who
  (EnemyWithTraitDestroyedByAttackWindow t trait who, EnemyDestroyedByAttack _ attackers traits) ->
    t == timing && trait `member` traits && who `elem` attackers
  (EnemyOfOtherPlayerDestroyedByAttackWindow t pid who, EngagedEnemyDestroyedByAttack pid' attackers) -> t == timing && pid /= pid' && who `elem` attackers
  (CharacterDestroyedWindow t, CharacterDestroyed _ _) -> t == timing
  (HeroDestroyedWindow t, CharacterDestroyed (HeroTarget _) _) -> t == timing
  (AllyDestroyedWindow t, CharacterDestroyed (AllyTarget _) _) -> t == timing
  (EnemyRevealedWindow t, Revealed (EnemyTarget _)) -> t == timing
  (TreacheryRevealedWindow t, Revealed (TreacheryTarget _)) -> t == timing
  (TreacheryWithTraitRevealedWindow {}, _) -> False
  (SideQuestRevealedWindow t, Revealed (QuestTarget _)) -> t == timing
  (ShadowEffectWindow t, ShadowEffectTriggered _ _) -> t == timing
  (ExploredWindow t lid, Explored lid') -> t == timing && lid == lid'
  (PhaseEndsWindow t phase, PhaseEnds phase') -> t == timing && phase == phase'
  (RoundEndsWindow, RoundEnds) -> True
  (StagingStepBeginsWindow, StagingStepBegins) -> True
  (StagingStepEndsWindow, StagingStepEnds) -> True
  (ProgressPlacedWindow t target, ProgressPlaced target' _) -> t == timing && target == target'
  (QuestedUnsuccessfullyWindow t, QuestedUnsuccessfully) -> t == timing
  (QuestedSuccessfullyWindow t, QuestedSuccessfully) -> t == timing
  (DefendedAttackWindow t who, DefendedAttack target _) -> t == timing && maybe True (== target) who
  (PlayedFromHandWindow t target, PlayedFromHand target') -> t == timing && target == target'
  (LocationRevealedWindow t, Revealed (LocationTarget _)) -> t == timing
  (ShadowEffectAgainstWindow t target, ShadowEffectAgainst target') -> t == timing && target == target'
  (AnyEnemyAttacksWindow t, EnemyAttacking _ _) -> t == timing
  (CharacterLeftPlayWindow t m, CharacterLeftPlay c) -> t == timing && cardMatches m c
  (AttackedWindow t target, AttackedEnemy target' _) -> t == timing && target == target'
  (ShadowEffectResolvedWindow t eid, ShadowEffectResolved _ eid') -> t == timing && eid == eid'
  (ShadowCardDealtWindow t pid, ShadowCardDealt _ _ pid') -> t == timing && pid == pid'
  (EnemyAttacksWindow t eid, EnemyAttacking eid' _) -> t == timing && eid == eid'
  (EnemyAttackedWindow t eid, EnemyAttacked eid') -> t == timing && eid == eid'
  (AllyLeftPlayWindow t, LeftPlay (AllyTarget _) _ _) -> t == timing
  (AttackerDeclaredWindow t who, DeclaredAsAttacker target _) -> t == timing && maybe True (== target) who
  (HeroLeftPlayWindow t pid trait, LeftPlay (HeroTarget _) pid' traits) -> t == timing && pid == pid' && trait `member` traits
  (AllyWithTraitLeftPlayWindow t pid trait, LeftPlay (AllyTarget _) pid' traits) -> t == timing && pid == pid' && trait `member` traits
  (ReadiedWindow t trait, Readied _ traits) -> t == timing && maybe True (`member` traits) trait
  (HealedByOtherWindow t source, Healed _ source') -> t == timing && source /= source'
  (AllyEnteredPlayWindow t pid trait, AllyEnteredPlay _ pid' traits) ->
    t == timing && maybe True (== pid') pid && maybe True (`member` traits) trait
  (DiscardedFromDeckWindow t pid card, DiscardedFromDeck pid' card') -> t == timing && pid == pid' && maybe True (== card') card
  (PlayedCardFromHandWindow t pid m, PlayedCardFromHand pid' code) -> t == timing && pid == pid' && cardMatches m code
  (HeroDealtDamageWindow t, DealtDamage (HeroTarget _) _) -> t == timing
  (LeftPlayWindow t target, LeftPlay target' _ _) -> t == timing && target == target'
  (AttackedAloneWithExcessWindow t target, AttackedAlone target' _ n) -> t == timing && target == target' && n > 0
  (ThreatRaisedByEncounterWindow t pid, ThreatRaisedByEncounter pid' _) -> t == timing && pid == pid'
  (OtherPlayerRaisedThreatWindow t pid, ThreatRaised pid') -> t == timing && pid /= pid'
  (OtherPlayerEngagedEnemyWindow t pid, EnemyEngaged _ pid') -> t == timing && pid /= pid'
  (ChangedControlWindow t target, ChangedControl target') -> t == timing && target == target'
  (EventPlayedWindow t, PlayingEvent _ _) -> t == timing
  (LocationEntersPlayWindow t, EntersStagingArea (LocationTarget _)) -> t == timing
  (CardPlayedWindow t m, PlayedCard _ code) -> t == timing && cardMatches m code
  (EnemyOptionallyEngagedWindow t eid, OptionallyEngaged eid' _) -> t == timing && eid == eid'
  (AttackUndefendedWindow t pid, AttackUndefended pid' _) -> t == timing && pid == pid'
  (EnemyDefeatedWindow t, EnemyDefeated _) -> t == timing
  (ThisEnemyDefeatedWindow t eid, EnemyDefeated eid') -> t == timing && eid == eid'
  (CharacterDealtDamageWindow t, DealtDamage (HeroTarget _) _) -> t == timing
  (CharacterDealtDamageWindow t, DealtDamage (AllyTarget _) _) -> t == timing
  (CharacterDestroyedByAttackWindow t who, CharacterDestroyedByAttack eid _ _) -> t == timing && maybe True (== eid) who
  (TimeCountersRemovedWindow t who, TimeCountersRemoved target _) -> t == timing && maybe True (== target) who
  (TimeCountersRemovedFromQuestWindow t, TimeCountersRemoved (QuestTarget _) _) -> t == timing
  (LastTimeCounterRemovedWindow t target, LastTimeCounterRemoved target') -> t == timing && target == target'
  (ThreatRaisedByDoomedWindow t pid, ThreatRaisedByDoomed pid') -> t == timing && pid == pid'
  (DrewCardsWindow t who, CardsDrawn pid _) -> t == timing && maybe True (== pid) who
  (AnyExploredWindow t, Explored _) -> t == timing
  (TargetDestroyedWindow t target, CharacterDestroyed target' _) -> t == timing && target == target'
  (EncounterCardRevealedWindow t, Revealed _) -> t == timing
  (DefendedWithoutDamageWindow t target, DefendedWithoutDamage target' _) -> t == timing && target == target'
  (ThreatReducedByPlayerCardWindow t, ThreatReduced _ source) -> t == timing && isPlayerCardSource source
  (CardEntersStagingAreaWindow t target, EntersStagingArea target') -> t == timing && target == target'
  (AdvancedToStageWindow t, AdvancedToStage) -> t == timing
  (EliminatedByThreatWindow t pid, EliminatedByThreat pid') -> t == timing && pid == pid'
  (QuestDefeatedWindow t target, QuestDefeated target') -> t == timing && target == target'
  (AnyQuestDefeatedWindow t, QuestDefeated _) -> t == timing
  (OwnCharacterDestroyedWindow t pid, CharacterDestroyedUnder pid' _) -> t == timing && pid == pid'
  (AllyWithTraitDiscardedByCardEffectWindow t trait, AllyDiscardedByCardEffect _ traits) -> t == timing && trait `member` traits
  (EnemyDestroyedWindow t m, EnemyDestroyed card) -> t == timing && cardMatches m card
  (DiscardedFromHandWindow t pid, DiscardedFromHand pid' _) -> t == timing && pid == pid'
  (BecameReadyWindow t target, BecameReady target') -> t == timing && target == target'
  (BecomesWindow time, Became time') -> time == time'
  (TreacheryWhenRevealedCanceledWindow t, WhenRevealedCanceled (TreacheryTarget _)) -> t == timing
  (CapturedCardsRescuedWindow t, CapturedCardsRescued {}) -> t == timing
  (DrewForResourcePhaseWindow t pid, DrewForResourcePhase pid' _) -> t == timing && pid == pid'
  (SetupHandDrawnWindow t pid, SetupHandDrawn pid') -> t == timing && pid == pid'
  (CommittedToSailingTestWindow t, CommittedToSailingTest {}) -> t == timing
  (ShadowIncreasedAttackWindow t, ShadowIncreasedAttack {}) -> t == timing
  (PaidFromOneHeroWindow t target, PaidFrom target' payers) -> t == timing && target == target' && case ordNub payers of
    [HeroTarget _] -> True
    _ -> False
  (AttachedToHeroWindow t a, Attached a' (HeroTarget _) _) -> t == timing && a == a'
  (TraitAttachedToHeroWindow t trait, Attached _ (HeroTarget _) traits) -> t == timing && trait `member` traits
  (RemovedFromQuestWindow t c, RemovedFromQuest c') -> t == timing && c == c'
  (HiddenCardTurnedFaceupWindow t c, HiddenCardTurnedFaceup c') -> t == timing && c == c'
  (AnyPlayedCardFromHandWindow t m, PlayedCardFromHand _ code) -> t == timing && cardMatches m code
  (AnotherCharacterDealtDamageWindow t other, DealtDamage c@(HeroTarget _) _) -> t == timing && c /= other
  (AnotherCharacterDealtDamageWindow t other, DealtDamage c@(AllyTarget _) _) -> t == timing && c /= other
  -- these read the game, so "LordOfTheRings.Helpers.Window" matches them
  (LeftPlaySharingTraitWindow {}, _) -> False
  (EnemyDestroyedByHeroWindow {}, _) -> False
  (EnemyWithTraitRevealedWindow {}, _) -> False
  (EnemyWithTraitEntersStagingAreaWindow {}, _) -> False
  (EnemyWithModifierEngagedWindow {}, _) -> False
  (CardRevealedWindow {}, _) -> False
  (AttackDealtDamageWithTraitWindow {}, _) -> False
  (TraveledToWindow {}, _) -> False
  (AttachmentPlayedOnHeroWindow {}, _) -> False
  (EnemyAttacksPlayerWindow {}, _) -> False
  (ExploredWithoutVictoryWindow {}, _) -> False
  (ActiveLocationExploredWindow {}, _) -> False
  (CharacterWithTraitDealtDamageWindow {}, _) -> False
  (HeroGainedResourcesWindow {}, _) -> False
  (SurgeCardRevealedWindow {}, _) -> False
  (HeroDefenderDeclaredWindow {}, _) -> False
  (AttackDealtDamageByWindow {}, _) -> False
  (MatchingEnemyAttacksWindow {}, _) -> False
  (EnemyWithPrintedHitPointsDestroyedByAttackWindow {}, _) -> False
  (ExploredMatchingWindow {}, _) -> False
  (HeroDeclaredAsDefenderWindow {}, _) -> False
  (RevealedTitleInVictoryDisplayWindow {}, _) -> False
  (PlayerDiscardedFromDeckWindow {}, _) -> False
  (LocationFlippedWindow {}, _) -> False
  (LocationBecameActiveWindow {}, _) -> False
  (ThreatRaisedWindow {}, _) -> False
  (DefenderWithTraitDestroyedWindow {}, _) -> False
  (PlayerCommittedCharactersWindow {}, _) -> False
  (CharacterDefenderDeclaredWindow {}, _) -> False
  (ObjectiveExhaustedWindow {}, _) -> False
  (MatchingEnemyEngagedWindow {}, _) -> False
  (MatchingEnemyDealtDamageWindow {}, _) -> False
  (QuestBypassedWindow t, QuestBypassed _) -> t == timing
  -- read with the player's errata in 'windowMatchesM'
  (ErrataWindow _ errata _, _) -> windowMatches errata (Window timing wType)
  _ -> False
