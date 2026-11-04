module LordOfTheRings.Enemy.Cards.GreatCaveTrollJourneyInTheDark (greatCaveTrollJourneyInTheDark) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GreatCaveTrollJourneyInTheDark = GreatCaveTrollJourneyInTheDark EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatCaveTrollJourneyInTheDark :: EnemyCard GreatCaveTrollJourneyInTheDark
greatCaveTrollJourneyInTheDark = enemy GreatCaveTrollJourneyInTheDark Cards.greatCaveTrollJourneyInTheDark

-- | Cannot have attachments.
instance HasModifiersFor GreatCaveTrollJourneyInTheDark where
  getModifiersFor (GreatCaveTrollJourneyInTheDark a) = modifySelf a [CannotHaveAttachments]

-- | For each point of excess combat damage dealt by Great Cave-troll (damage that is dealt
-- beyond the remaining hit points of the character damaged by its attack) remove 1
-- progress from the current quest.
instance RunMessage GreatCaveTrollJourneyInTheDark where
  runMessage msg (GreatCaveTrollJourneyInTheDark attrs) = case msg of
    CheckWindows ws -> do
      for_ [n | Window After (AttackDealtExcessDamage eid _ n) <- ws, eid == attrs.id] $ removeProgressFromQuest attrs
      GreatCaveTrollJourneyInTheDark <$> runMessage msg attrs
    _ -> GreatCaveTrollJourneyInTheDark <$> runMessage msg attrs
