module LordOfTheRings.Treachery.Cards.SwarmingInsect (swarmingInsect) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwarmingInsect = SwarmingInsect TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swarmingInsect :: TreacheryCard SwarmingInsect
swarmingInsect = treachery SwarmingInsect Cards.swarmingInsect

-- | When Revealed: Deal 1 damage to each character without any attachments.
-- Shadow: If a character (including Wilyador) has more damage than each other character,
-- deal 3 additional damage to that character.
instance RunMessage SwarmingInsect where
  runMessage msg t@(SwarmingInsect attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      bare <- filterM (fmap null . getAllAttachmentsOn) characters
      for_ bare \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      damaged <- traverse (\c -> (c,) <$> getDamage c) =<< selectCharacters AnyHero AnyAlly
      case sortOn (Down . snd) damaged of
        (c, d) : rest | d > 0, all ((< d) . snd) rest -> dealDamage attrs c 3
        _ -> pure ()
      pure t
    _ -> SwarmingInsect <$> runMessage msg attrs
