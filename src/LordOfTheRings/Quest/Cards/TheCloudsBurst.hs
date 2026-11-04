module LordOfTheRings.Quest.Cards.TheCloudsBurst (theCloudsBurst) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheCloudsBurst = TheCloudsBurst QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCloudsBurst :: QuestCard TheCloudsBurst
theCloudsBurst = quest TheCloudsBurst Cards.theCloudsBurst

bolg :: EnemyMatcher
bolg = EnemyWithCardCode (toCardCode Enemies.bolg)

-- | Progress cannot be placed on this stage. While Bolg is in the staging area, it is
-- considered to be engaged with the first player.
instance HasModifiersFor TheCloudsBurst where
  getModifiersFor (TheCloudsBurst a) = when a.flipped do
    modifySelf a [ProgressLimit 0]
    staged <- select (bolg <> EnemyInStagingArea)
    for_ staged \eid -> modified a eid [EngagedWithEachPlayer FirstPlayer]

{- | When Revealed: Each player must search the encounter deck and discard pile for a copy
of Bodyguard of Bolg and add it to the staging area, if able. Shuffle the encounter deck.

If Bolg is destroyed, the players win the game.
-}
instance RunMessage TheCloudsBurst where
  runMessage msg q@(TheCloudsBurst attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheCloudsBurst <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscard pid attrs (cardIs Enemies.bodyguardOfBolg)
      pure q
    Destroy _ (EnemyTarget eid) | attrs.flipped -> do
      whenM (selectAny $ EnemyWithId eid <> bolg) winGame
      TheCloudsBurst <$> runMessage msg attrs
    _ -> TheCloudsBurst <$> runMessage msg attrs
