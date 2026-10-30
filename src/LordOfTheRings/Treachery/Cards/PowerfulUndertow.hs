module LordOfTheRings.Treachery.Cards.PowerfulUndertow (powerfulUndertow) where

import LordOfTheRings.Trait (Trait (Underwater))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PowerfulUndertow = PowerfulUndertow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

powerfulUndertow :: TreacheryCard PowerfulUndertow
powerfulUndertow = treachery PowerfulUndertow Cards.powerfulUndertow

-- | When Revealed: You cannot flip the active location this round. If the active location
-- is Underwater, resolve its Forced effect as if it is the end of the quest phase. If there
-- is no Underwater active location, Powerful Undertow gains doomed 3, instead.
-- Each Underwater location's end-of-quest-phase Forced effect is its first ability.
instance RunMessage PowerfulUndertow where
  runMessage msg t@(PowerfulUndertow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select IsActiveLocation
      for_ active \lid -> roundModifier attrs lid CannotBeFlipped
      underwater <- select (IsActiveLocation <> LocationWithTrait Underwater)
      lead <- getLead
      for_ underwater \lid -> resolveAbility lead (LocationSource lid) 1
      when (null underwater) $ raiseThreatFromDoomed 3
      pure t
    _ -> PowerfulUndertow <$> runMessage msg attrs
