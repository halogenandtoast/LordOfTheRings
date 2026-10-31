module LordOfTheRings.Location.Cards.HivesAndHives (hivesAndHives) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HivesAndHives = HivesAndHives LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hivesAndHives :: LocationCard HivesAndHives
hivesAndHives = location HivesAndHives Cards.hivesAndHives

instance HasAbilities HivesAndHives where
  getAbilities (HivesAndHives a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Forced: After any amount of progress is placed on Hives and Hives, deal 1 damage to
-- each character in play.
-- Shadow: Attacking enemy gets +2 attack.
instance RunMessage HivesAndHives where
  runMessage msg l@(HivesAndHives a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> dealDamage a c 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 2
      pure l
    _ -> HivesAndHives <$> runMessage msg a
