module LordOfTheRings.Ally.Cards.SoldierOfIsengard (soldierOfIsengard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype SoldierOfIsengard = SoldierOfIsengard AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2, on its card def.
soldierOfIsengard :: AllyCard SoldierOfIsengard
soldierOfIsengard = ally SoldierOfIsengard Cards.soldierOfIsengard

-- | Response: After a player raises his threat from the doomed keyword, give control of
-- Soldier of Isengard to that player. Offered for a player who does not control it already.
instance HasAbilities SoldierOfIsengard where
  getAbilities (SoldierOfIsengard a) =
    [ mkAbility a 1 $ response $ MatchingThreatRaisedByDoomedWindow After (NotPlayer $ PlayerWithId pid)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage SoldierOfIsengard where
  runMessage msg s@(SoldierOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (headMay [p | Window After (ThreatRaisedByDoomed p) <- ws]) (takeControl a.id)
      pure s
    _ -> SoldierOfIsengard <$> runMessage msg a
