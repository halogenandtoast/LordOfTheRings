module LordOfTheRings.Treachery.Cards.PowerfulInWrath (powerfulInWrath) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (damageHrogarsHill, getHrogarsHill)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PowerfulInWrath = PowerfulInWrath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1, as its card def says.
powerfulInWrath :: TreacheryCard PowerfulInWrath
powerfulInWrath = treachery PowerfulInWrath Cards.powerfulInWrath

{- | When Revealed: Discard the active location and deal X damage to Hrogar's Hill, where X
is that location's [threat]. If no damage is dealt this way, Powerful in Wrath gains
surge.
Shadow: Excess damage from this attack must be dealt to Hrogar's Hill.
-}
instance RunMessage PowerfulInWrath where
  runMessage msg t@(PowerfulInWrath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select IsActiveLocation
      x <- sum <$> traverse getLocationThreat active
      for_ active discard
      damageHrogarsHill attrs x
      when (x <= 0) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      getHrogarsHill >>= traverse_ \hill -> attackModifier attrs eid (ExcessDamageTo $ QuestTarget hill)
      pure t
    _ -> PowerfulInWrath <$> runMessage msg attrs
