module LordOfTheRings.Treachery.Cards.BitingWind (bitingWind) where

import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BitingWind = BitingWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bitingWind :: TreacheryCard BitingWind
bitingWind = treachery BitingWind Cards.bitingWind

-- | When Revealed: Assign X damage among characters committed to the quest. X is the
-- number of characters committed to the quest. The first player assigns it.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage BitingWind where
  runMessage msg t@(BitingWind attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      committed <- getCommittedCharacters
      dealDamageDividedAmong attrs lead committed (length committed)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> BitingWind <$> runMessage msg attrs
