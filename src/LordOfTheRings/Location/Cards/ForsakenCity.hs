module LordOfTheRings.Location.Cards.ForsakenCity (forsakenCity) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

-- | Uncharted, under a Lost Island side (The Fate of Númenor Nightmare).
newtype ForsakenCity = ForsakenCity LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

forsakenCity :: LocationCard ForsakenCity
forsakenCity = location ForsakenCity Cards.forsakenCity

-- | Each Undead enemy gains "immune to player card effects."
instance HasModifiersFor ForsakenCity where
  getModifiersFor (ForsakenCity a) = do
    undead <- select (EnemyWithTrait Undead)
    for_ undead \e -> modified a e [ImmuneToPlayerCardEffects]

-- | Forced: After Forsaken City is flipped over while active, each engaged Undead enemy
-- makes an immediate attack.
instance HasAbilities ForsakenCity where
  getAbilities (ForsakenCity a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage ForsakenCity where
  runMessage msg l@(ForsakenCity a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        undead <- select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId p))
        for_ undead (immediateAttack p)
      pure l
    _ -> ForsakenCity <$> runMessage msg a
