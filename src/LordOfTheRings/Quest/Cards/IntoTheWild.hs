module LordOfTheRings.Quest.Cards.IntoTheWild (intoTheWild) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (eitherRingwraithOr)

newtype IntoTheWild = IntoTheWild QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheWild :: QuestCard IntoTheWild
intoTheWild = quest IntoTheWild Cards.intoTheWild

-- | Forced: At the end of the round, either remove X progress from the current quest or
-- shuffle 1 out of play Ringwraith into the encounter deck. X is the number of players in
-- the game.
instance HasAbilities IntoTheWild where
  getAbilities (IntoTheWild a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed: Add Midgewater to the staging area. Each player places 1 progress on
the current quest.

The first player chooses for the Forced effect. The progress is removable in full only
with at least X on the stage.

The players cannot advance while Midgewater is in play.
-}
instance RunMessage IntoTheWild where
  runMessage msg q@(IntoTheWild attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      IntoTheWild <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.midgewater)
      eachPlayer \_ -> placeProgress attrs attrs 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      x <- length <$> getRemainingPlayers
      let n = min x attrs.progress
      eitherRingwraithOr lead attrs "intoTheWild.choose" (attrs.progress >= x) "intoTheWild.removeProgress"
        $ when (n > 0) (removeTokens attrs attrs Progress n)
      pure q
    _ -> IntoTheWild <$> runConditionalStage (selectNone midgewater) msg attrs
   where
    midgewater = LocationWithCardCode (toCardCode Locations.midgewater)
