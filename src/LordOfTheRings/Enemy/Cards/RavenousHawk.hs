module LordOfTheRings.Enemy.Cards.RavenousHawk (ravenousHawk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RavenousHawk = RavenousHawk EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Forced: When a card effect instructs a player to "look" at cards in the encounter deck
-- and Ravenous Hawk is among those cards, that player must put Ravenous Hawk into play
-- engaged with him. (The Hunt for Gollum watches the encounter deck for it.)
ravenousHawk :: EnemyCard RavenousHawk
ravenousHawk = enemy RavenousHawk Cards.ravenousHawk

instance RunMessage RavenousHawk where
  runMessage msg (RavenousHawk attrs) = RavenousHawk <$> runMessage msg attrs
