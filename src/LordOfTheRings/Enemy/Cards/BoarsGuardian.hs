module LordOfTheRings.Enemy.Cards.BoarsGuardian (boarsGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype BoarsGuardian = BoarsGuardian EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarsGuardian :: EnemyCard BoarsGuardian
boarsGuardian = enemy BoarsGuardian Cards.boarsGuardian

-- | Cannot have non-Key attachments: a Key comes as an objective, which this does not bar.
instance HasModifiersFor BoarsGuardian where
  getModifiersFor (BoarsGuardian a) = modified a a [CannotHaveAttachments]

-- | Forced: After the last time counter is removed from Boar's Guardian the engaged player
-- must discard an ally he controls. Place 2 time counters on Boar's Guardian.
instance HasAbilities BoarsGuardian where
  getAbilities (BoarsGuardian a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage BoarsGuardian where
  runMessage msg e@(BoarsGuardian attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "boarsGuardian.discard"
          causedBy attrs
          targets allies discard
      placeTimeCounters attrs attrs 2
      pure e
    _ -> BoarsGuardian <$> runMessage msg attrs
