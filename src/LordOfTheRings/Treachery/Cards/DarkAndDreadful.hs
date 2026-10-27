module LordOfTheRings.Treachery.Cards.DarkAndDreadful (darkAndDreadful) where

import LordOfTheRings.Trait (Trait (Dark))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarkAndDreadful = DarkAndDreadful TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkAndDreadful :: TreacheryCard DarkAndDreadful
darkAndDreadful = treachery DarkAndDreadful Cards.darkAndDreadful

-- | When Revealed: Deal 1 damage to each exhausted character. (2 damage instead if the
-- active location is a Dark location.)
-- Shadow: Deal 1 damage to the defending character. (Attacking enemy gets +2 [Attack]
-- instead if this attack is undefended.)
instance RunMessage DarkAndDreadful where
  runMessage msg t@(DarkAndDreadful attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c (if dark then 2 else 1)
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      ifUndefended defenders (enemyGetsAttack attrs eid 2) (for_ defenders \d -> dealDamage attrs d 1)
      pure t
    _ -> DarkAndDreadful <$> runMessage msg attrs
