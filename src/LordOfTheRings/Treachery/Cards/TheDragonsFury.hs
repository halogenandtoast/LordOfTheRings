module LordOfTheRings.Treachery.Cards.TheDragonsFury (theDragonsFury) where

import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (getDagnir, getHrogarsHill)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDragonsFury = TheDragonsFury TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDragonsFury :: TreacheryCard TheDragonsFury
theDragonsFury = treachery TheDragonsFury Cards.theDragonsFury

{- | When Revealed: Dagnir makes an immediate attack against the first player. Undefended
damage from this attack must be dealt to Hrogar's Hill.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage TheDragonsFury where
  runMessage msg t@(TheDragonsFury attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getDagnir >>= traverse_ \d -> do
        getHrogarsHill >>= traverse_ \hill -> attackModifier attrs d (UndefendedDamageTo $ QuestTarget hill)
        lead <- getLead
        immediateAttack lead d
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> TheDragonsFury <$> runMessage msg attrs
