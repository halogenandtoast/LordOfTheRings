module LordOfTheRings.Enemy.Cards.UlfastSentry (ulfastSentry) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers (ulfastX)

newtype UlfastSentry = UlfastSentry EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

ulfastSentry :: EnemyCard UlfastSentry
ulfastSentry = enemy UlfastSentry Cards.ulfastSentry

-- | X is 1 more than the number of resources on the main quest: its attack.
instance HasModifiersFor UlfastSentry where
  getModifiersFor (UlfastSentry a) = do
    x <- ulfastX
    modifySelf a [AttackModifier x]

-- | Forced: After Ulfast Sentry engages you, either place 1 resource on the main quest or
-- Ulfast Sentry makes an attack.
instance HasAbilities UlfastSentry where
  getAbilities (UlfastSentry a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | The player it engaged chooses.
instance RunMessage UlfastSentry where
  runMessage msg e@(UlfastSentry attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid ->
        chooseOneM pid do
          questionLabeled "ulfastSentry.choose"
          causedBy attrs
          labeled "ulfastSentry.resource" $ placeResourcesOnMainQuest attrs 1
          labeled "ulfastSentry.attack" $ immediateAttack pid attrs.id
      pure e
    _ -> UlfastSentry <$> runMessage msg attrs
