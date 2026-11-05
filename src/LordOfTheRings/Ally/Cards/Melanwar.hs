module LordOfTheRings.Ally.Cards.Melanwar (melanwar) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (TravelPhase))

newtype Melanwar = Melanwar AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Ered Mithrin campaign, from Lost in Mirkwood. Ranged, on his card def.
melanwar :: AllyCard Melanwar
melanwar = ally Melanwar Cards.melanwar

-- | Response: After the travel phase begins, exhaust Melanwar to place 1 progress on the
-- active location. If you quested unsuccessfully this round, place 3 progress on the
-- active location instead.
instance HasAbilities Melanwar where
  getAbilities (Melanwar a) =
    [restricted a 1 (exists IsActiveLocation) $ ResponseAbility (PhaseBeginsWindow After TravelPhase) (exhaustCost a)]

instance RunMessage Melanwar where
  runMessage msg s@(Melanwar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      failed <- elem QuestedUnsuccessfullyThisRound <$> getModifiers GameTarget
      active <- select IsActiveLocation
      for_ active \lid -> placeProgress a lid (if failed then 3 else 1)
      pure s
    _ -> Melanwar <$> runMessage msg a
