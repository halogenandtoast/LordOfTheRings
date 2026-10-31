module LordOfTheRings.Location.Cards.DesecratedGrounds (desecratedGrounds) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

-- | Uncharted, under a Lost Island side (The Fate of Númenor Nightmare).
newtype DesecratedGrounds = DesecratedGrounds LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

desecratedGrounds :: LocationCard DesecratedGrounds
desecratedGrounds = location DesecratedGrounds Cards.desecratedGrounds

-- | Each Undead enemy gets +1 threat and +1 attack.
instance HasModifiersFor DesecratedGrounds where
  getModifiersFor (DesecratedGrounds a) = do
    undead <- select (EnemyWithTrait Undead)
    for_ undead \e -> modified a e [ThreatModifier 1, AttackModifier 1]

-- | Forced: After Desecrated Grounds is flipped over while active, deal 1 damage to each
-- exhausted character.
instance HasAbilities DesecratedGrounds where
  getAbilities (DesecratedGrounds a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage DesecratedGrounds where
  runMessage msg l@(DesecratedGrounds a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 1
      pure l
    _ -> DesecratedGrounds <$> runMessage msg a
