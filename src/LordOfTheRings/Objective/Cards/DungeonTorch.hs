module LordOfTheRings.Objective.Cards.DungeonTorch (dungeonTorch) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Claim
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype DungeonTorch = DungeonTorch ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonTorch :: ObjectiveCard DungeonTorch
dungeonTorch = objective DungeonTorch Cards.dungeonTorch

-- | Forced: At the end of each round, raise attached hero's controller's threat by 2.
instance HasAbilities DungeonTorch where
  getAbilities (DungeonTorch a) = claimAbility a <> [mkAbility a 2 $ forced RoundEndsWindow | AttachedTo _ <- [a.placement]]

instance RunMessage DungeonTorch where
  runMessage msg o@(DungeonTorch attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      claimObjective pid attrs
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      withHost attrs \case
        HeroTarget hid -> (`raiseThreat` 2) . (.owner) =<< getHero hid
        _ -> pure ()
      pure o
    _ -> DungeonTorch <$> runMessage msg attrs
