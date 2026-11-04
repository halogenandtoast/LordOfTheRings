module LordOfTheRings.Location.Cards.DesertOasis (desertOasis) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DesertOasis = DesertOasis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desertOasis :: LocationCard DesertOasis
desertOasis = location DesertOasis Cards.desertOasis

-- | While Desert Oasis is the active location, it gains: "Response: When Desert Oasis is
-- explored, heal 4 damage from among characters in play."
-- Forced: After Desert Oasis becomes the active location, each player exhausts a character
-- he controls.
instance HasAbilities DesertOasis where
  getAbilities (DesertOasis a) =
    [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]
      <> [mkAbility a 2 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The player taking the Response heals the damage a point at a time, from the damaged
-- characters left, as The Long Defeat's.
instance RunMessage DesertOasis where
  runMessage msg l@(DesertOasis a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      doStep 4 (UseCardAbility pid (toSource a) 1 [])
      pure l
    DoStep n (UseCardAbility pid (isSource a -> True) 1 _) | n > 0 -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOrRunOneM pid do
        questionLabeledWith "desertOasis.heal" [intVar "count" n]
        causedBy a
        targets damaged \c -> do
          healDamage a c 1
          doStep (n - 1) (UseCardAbility pid (toSource a) 1 [])
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \p -> chooseAndExhaust a p 1
      pure l
    _ -> DesertOasis <$> runMessage msg a
