module LordOfTheRings.Treachery.Cards.BlockingWargs (blockingWargs) where

import LordOfTheRings.Helpers.Character (getCommittedCharacters)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlockingWargs = BlockingWargs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blockingWargs :: TreacheryCard BlockingWargs
blockingWargs = treachery BlockingWargs Cards.blockingWargs

-- | Surge.
-- When Revealed: Deal 1 damage to each character committed to the quest.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage BlockingWargs where
  runMessage msg t@(BlockingWargs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- getCommittedCharacters
      for_ committed \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> BlockingWargs <$> runMessage msg attrs
