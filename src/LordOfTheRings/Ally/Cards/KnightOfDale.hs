module LordOfTheRings.Ally.Cards.KnightOfDale (knightOfDale) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype KnightOfDale = KnightOfDale AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

knightOfDale :: AllyCard KnightOfDale
knightOfDale = ally KnightOfDale Cards.knightOfDale

-- | Action: Spend 1 leadership resource to ready Knight of Dale. (Limit once per round.)
-- Valour Action: Ready Knight of Dale. (Limit once per round.) Each has its own limit.
instance HasAbilities KnightOfDale where
  getAbilities (KnightOfDale a) =
    limited (PlayerLimit PerRound 1) (restricted a 1 exhausted $ ActionAbility (SpendResourcesCost Leadership 1))
      : [ limited (PlayerLimit PerRound 1) $ restricted a 2 (exhausted <> valour pid) $ ActionAbility Free
        | Just pid <- [placementController a.placement]
        ]
   where
    exhausted = exists (AllyWithId a.id <> ExhaustedAlly)

instance RunMessage KnightOfDale where
  runMessage msg s@(KnightOfDale a) = case msg of
    UseCardAbility _ (isSource a -> True) n _ | n `elem` [1, 2] -> do
      ready a
      pure s
    _ -> KnightOfDale <$> runMessage msg a
