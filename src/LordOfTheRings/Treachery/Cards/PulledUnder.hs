module LordOfTheRings.Treachery.Cards.PulledUnder (pulledUnder) where

import LordOfTheRings.Trait (Trait (Tentacle))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PulledUnder = PulledUnder TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pulledUnder :: TreacheryCard PulledUnder
pulledUnder = treachery PulledUnder Cards.pulledUnder

-- | When Revealed: Deal 3 damage to each character with a Tentacle attachment. If no
-- characters were damaged by this effect, Pulled Under gains surge.
-- Shadow: If the defending character has a Tentacle attachment, discard that character.
instance RunMessage PulledUnder where
  runMessage msg t@(PulledUnder attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      entangled <- filterM (fmap notNull . attachedWithTrait Tentacle) characters
      for_ entangled \c -> dealDamage attrs c 3
      when (null entangled) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      entangled <- filterM (fmap notNull . attachedWithTrait Tentacle) defenders
      traverse_ (discard attrs) entangled
      pure t
    _ -> PulledUnder <$> runMessage msg attrs
