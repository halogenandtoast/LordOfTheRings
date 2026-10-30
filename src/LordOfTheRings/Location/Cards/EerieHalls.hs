module LordOfTheRings.Location.Cards.EerieHalls (eerieHalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (controlsClue)

newtype EerieHalls = EerieHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eerieHalls :: LocationCard EerieHalls
eerieHalls = location EerieHalls Cards.eerieHalls

-- | Action: Raise your threat by 1 to lower the threat of Eerie Halls by 1 until the end of
-- the phase. Any player may trigger this action.
instance HasAbilities EerieHalls where
  getAbilities (EerieHalls a) = [anyPlayer $ mkAbility a 1 $ ActionAbility (RaiseThreatCost 1)]

-- | Shadow: If the defending player does not control at least 1 Clue objective, the
-- attacking enemy makes an additional attack after this one.
instance RunMessage EerieHalls where
  runMessage msg l@(EerieHalls a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a a (ThreatModifier (-1))
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      clue <- controlsClue pid
      unless clue $ additionalAttack pid eid
      pure l
    _ -> EerieHalls <$> runMessage msg a
