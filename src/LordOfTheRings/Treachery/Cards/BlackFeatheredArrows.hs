module LordOfTheRings.Treachery.Cards.BlackFeatheredArrows (blackFeatheredArrows) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (attacksTheWeaker)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackFeatheredArrows = BlackFeatheredArrows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackFeatheredArrows :: TreacheryCard BlackFeatheredArrows
blackFeatheredArrows = treachery BlackFeatheredArrows Cards.blackFeatheredArrows

{- | When Revealed: Until the end of the round, add 1 to the archery total at this stage for
each ally currently at this stage: the allies its players control, counted now. The stage's
quest card gains the archery, so it adds to this stage's total only.
Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character has
fewer printed hit points than the attacking enemy).
-}
instance RunMessage BlackFeatheredArrows where
  runMessage msg t@(BlackFeatheredArrows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      n <- length <$> select (AllyControlledBy PlayerAtThisStage)
      stage <- getCurrentQuest
      for_ stage \q -> when (n > 0) $ roundModifier attrs q (GainKeyword (Archery n))
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      attacksTheWeaker attrs eid defenders
      pure t
    _ -> BlackFeatheredArrows <$> runMessage msg attrs
