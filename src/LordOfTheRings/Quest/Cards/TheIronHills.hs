module LordOfTheRings.Quest.Cards.TheIronHills (theIronHills) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType, LocationType), cdThreat, cdVictoryPoints, toCardDef)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheKingsQuest.Helpers (locationsInVictoryDisplay)

newtype TheIronHills = TheIronHills QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theIronHills :: QuestCard TheIronHills
theIronHills = quest TheIronHills Cards.theIronHills

{- | This stage cannot be defeated unless there are 3 locations with victory points in the
victory display. It has 0 quest points (Hall of Beorn), so it is defeated as soon as there
are: the engine checks it again as the block lifts, and a check while it is still blocked
defeats nothing.
-}
instance HasModifiersFor TheIronHills where
  getModifiersFor (TheIronHills a) = when a.flipped do
    n <- length . filter (isJust . cdVictoryPoints . toCardDef) <$> locationsInVictoryDisplay
    when (n < 3) $ modifySelf a [CannotBeDefeated]

-- | Forced: At the beginning of the quest phase, discard cards from the top of the
-- encounter deck until a location is discarded. Either add that location to the staging
-- area, or raise each player's threat by X, where X is that location's printed threat.
instance HasAbilities TheIronHills where
  getAbilities (TheIronHills a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

enemyOrLocation :: CardMatcher
enemyOrLocation = CardMatchAny [CardWithType EnemyType, CardWithType LocationType]

{- | Setup: Build the Caves deck and set Fire-drake, Dragon Hoard, and the Dragon Might
encounter set aside, out of play (the scenario does, as it gathers the encounter deck).
Shuffle the encounter deck. Each player discards cards from the top of the encounter deck
until he discards an enemy or location and adds that card to the staging area, in turn
order from the first player. Added, not revealed, its When Revealed effects and surge do
not resolve.
-}
instance RunMessage TheIronHills where
  runMessage msg q@(TheIronHills attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      shuffleEncounterDeck
      doStep 1 msg
      pure q
    DoStep n inner@(PerformSetup (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          discardEncounterCardsUntilFor attrs (PlayerTarget pid) enemyOrLocation
          doStep (n + 1) inner
        [] -> turnOver attrs
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget _) cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches enemyOrLocation) cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardEncounterCardsUntil attrs (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (find (cardMatches $ CardWithType LocationType) cards) \c -> do
        let x = fromMaybe 0 $ cdThreat (toCardDef c)
        lead <- getLead
        chooseOneM lead do
          questionLabeled "theIronHills.choose"
          causedBy attrs
          showing [EncounterCard c]
          labeled "theIronHills.staging" $ addToStagingFromDiscard c
          labeledWith "theIronHills.threat" [intVar "n" x] $ eachPlayer (`raiseThreat` x)
      pure q
    CheckStage qid | qid == attrs.id && attrs.flipped -> do
      completeQuest attrs.id
      pure q
    _ -> TheIronHills <$> runMessage msg attrs
