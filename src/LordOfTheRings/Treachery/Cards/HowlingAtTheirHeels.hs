module LordOfTheRings.Treachery.Cards.HowlingAtTheirHeels (howlingAtTheirHeels) where

import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (atOrcsStage, orcsAreaThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HowlingAtTheirHeels = HowlingAtTheirHeels TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

howlingAtTheirHeels :: TreacheryCard HowlingAtTheirHeels
howlingAtTheirHeels = treachery HowlingAtTheirHeels Cards.howlingAtTheirHeels

{- | When Revealed: If the players are not at the same stage as the Orcs, add the total
threat of each card in the Orcs' area to the total threat in the staging area until the
end of the phase. If the players are at the same stage as the Orcs, each enemy gets +1
threat until the end of the phase.
Shadow: Raise your threat by 1.

-}
instance RunMessage HowlingAtTheirHeels where
  runMessage msg t@(HowlingAtTheirHeels attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      together <- atOrcsStage
      if together
        then select AnyEnemy >>= traverse_ \e -> phaseModifier attrs e (ThreatModifier 1)
        else do
          x <- orcsAreaThreat
          when (x > 0) $ phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid 1
      pure t
    _ -> HowlingAtTheirHeels <$> runMessage msg attrs
