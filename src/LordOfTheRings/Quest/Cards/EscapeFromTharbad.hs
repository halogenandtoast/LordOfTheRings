module LordOfTheRings.Quest.Cards.EscapeFromTharbad (escapeFromTharbad) where

import Data.List (scanl)
import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers
import LordOfTheRings.Trait (Trait (Orc))

newtype EscapeFromTharbad = EscapeFromTharbad QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromTharbad :: QuestCard EscapeFromTharbad
escapeFromTharbad = quest EscapeFromTharbad Cards.escapeFromTharbad

instance HasModifiersFor EscapeFromTharbad where
  getModifiersFor (EscapeFromTharbad a) = eliminationLevelsLowered a

-- | Time 3. Forced: After the last time counter is removed from the quest, either raise each
-- player's threat by 3 or Bellach makes an immediate attack against Nalir. Then, place 3
-- time counters on this stage.
-- Forced: After a character leaves play, raise its controller's threat by 2.
instance HasAbilities EscapeFromTharbad where
  getAbilities (EscapeFromTharbad a) =
    [ mkAbility a 1 $ forced $ lastTimeCounterRemoved a
    , mkAbility a 2 $ forced $ CharacterLeftPlayWindow After AnyCard
    , emptyMugAbility a 3
    ]

orcEnemy :: CardMatcher
orcEnemy = CardMatchAll [CardWithType EnemyType, CardWithTrait Orc]

-- | When Revealed: Add Bellach and The Crossing at Tharbad to the staging area. Then shuffle
-- the encounter discard pile into the encounter deck and discard cards from the encounter
-- deck until X Orc enemies have been discarded, where X is the number of players in the
-- game. Add all discarded Orc enemies to the staging area.
-- Progress that would be placed on this quest are instead placed on The Crossing at Tharbad.
instance RunMessage EscapeFromTharbad where
  runMessage msg q@(EscapeFromTharbad attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      EscapeFromTharbad <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.bellachTroubleInTharbad)
      addSetAsideToStaging (cardIs Locations.theCrossingAtTharbad)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- as far into the deck as its Xth Orc enemy, or all of it
      let orcsSeen = scanl (\k c -> if cardMatches orcEnemy c then k + 1 else k) (0 :: Int) deck
          n = maybe (length deck) fst $ find ((>= players) . snd) (zip [0 ..] orcsSeen)
      discardTopOfEncounterDeck attrs n
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches orcEnemy) cards
      pure q
    UseCardAbility lead (isSource attrs -> True) 1 _ -> do
      bellach <- select (EnemyWithCardCode $ toCardCode Enemies.bellachTroubleInTharbad)
      nalir <- getNalir
      chooseOrRunOneM lead do
        questionLabeled "escapeFromTharbad.choose"
        causedBy attrs
        labeled "escapeFromTharbad.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 3
        when (isJust nalir) $ for_ (take 1 bellach) \b ->
          labeled "escapeFromTharbad.bellachAttacks" $ attackNalir attrs b
      placeTimeCounters attrs attrs 3
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      for_ [pid | Window _ (LeftPlay _ pid _) <- ws] \pid -> encounterRaisesThreat pid 2
      pure q
    UseCardAbility _ (isSource attrs -> True) 3 ws -> do
      removeCardThatReducedThreat ws
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      crossing <- select (LocationWithCardCode $ toCardCode Locations.theCrossingAtTharbad)
      for_ crossing \c -> placeProgress attrs c n
      pure q
    _ -> EscapeFromTharbad <$> runMessage msg attrs
