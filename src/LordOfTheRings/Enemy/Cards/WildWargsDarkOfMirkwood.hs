module LordOfTheRings.Enemy.Cards.WildWargsDarkOfMirkwood (wildWargsDarkOfMirkwood) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Goblin))

newtype WildWargsDarkOfMirkwood = WildWargsDarkOfMirkwood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildWargsDarkOfMirkwood :: EnemyCard WildWargsDarkOfMirkwood
wildWargsDarkOfMirkwood = enemy WildWargsDarkOfMirkwood Cards.wildWargsDarkOfMirkwood

instance HasAbilities WildWargsDarkOfMirkwood where
  getAbilities (WildWargsDarkOfMirkwood a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage WildWargsDarkOfMirkwood where
  runMessage msg e@(WildWargsDarkOfMirkwood a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> discardTopOfEncounterDeckFor a pid 1
      pure e
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Goblin) cards) $ putEnemyIntoPlayEngagedFromDiscard pid
      pure e
    _ -> WildWargsDarkOfMirkwood <$> runMessage msg a
