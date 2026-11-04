module LordOfTheRings.Treachery.Cards.TerribleStrength (terribleStrength) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TerribleStrength = TerribleStrength TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Shadow and Flame's Balrog deck, in nightmare mode.
terribleStrength :: TreacheryCard TerribleStrength
terribleStrength = treachery TerribleStrength Cards.terribleStrength

-- | Shadow: For each excess point of combat damage dealt by this attack (damage that is
-- dealt beyond the remaining hit points of the character damaged by this attack) you must
-- damage another character you control. Exhaust all characters damaged by this effect.
instance RunMessage TerribleStrength where
  runMessage msg t@(TerribleStrength attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      createEffect attrs eid [ExcessDamageToOtherCharacters, ExhaustsCharactersDamagedByExcess] UntilEndOfAttack
      pure t
    _ -> TerribleStrength <$> runMessage msg attrs
