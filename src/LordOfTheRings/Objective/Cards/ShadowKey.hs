module LordOfTheRings.Objective.Cards.ShadowKey (shadowKey) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Claim
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype ShadowKey = ShadowKey ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowKey :: ObjectiveCard ShadowKey
shadowKey = objective ShadowKey Cards.shadowKey

-- | Forced: At the end of each round, attached hero suffers 1 damage.
instance HasAbilities ShadowKey where
  getAbilities (ShadowKey a) = claimAbility a <> [mkAbility a 2 $ forced RoundEndsWindow | AttachedTo _ <- [a.placement]]

instance RunMessage ShadowKey where
  runMessage msg o@(ShadowKey attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      claimObjective pid attrs
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      withHost attrs \host -> dealDamage attrs host 1
      pure o
    _ -> ShadowKey <$> runMessage msg attrs
