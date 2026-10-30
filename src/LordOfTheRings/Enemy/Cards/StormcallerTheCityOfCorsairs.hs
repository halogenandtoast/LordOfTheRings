module LordOfTheRings.Enemy.Cards.StormcallerTheCityOfCorsairs (stormcallerTheCityOfCorsairs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

-- | The City of Corsairs' Stormcaller, a Ship enemy: not The Thing in the Depths' ship.
newtype StormcallerTheCityOfCorsairs = StormcallerTheCityOfCorsairs EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

stormcallerTheCityOfCorsairs :: EnemyCard StormcallerTheCityOfCorsairs
stormcallerTheCityOfCorsairs = enemy StormcallerTheCityOfCorsairs Cards.stormcallerTheCityOfCorsairs

-- | Boarding 1. Cannot have attachments.
instance HasModifiersFor StormcallerTheCityOfCorsairs where
  getModifiersFor (StormcallerTheCityOfCorsairs a) = modifySelf a [CannotHaveAttachments]

-- | Forced: At the end of the round, if the Stormcaller is engaged with a player, either
-- return it to the staging area or shift your heading off-course.
instance HasAbilities StormcallerTheCityOfCorsairs where
  getAbilities (StormcallerTheCityOfCorsairs a) = [mkAbility a 1 $ forced RoundEndsWindow | EngagedWith _ <- [a.placement]]

instance RunMessage StormcallerTheCityOfCorsairs where
  runMessage msg e@(StormcallerTheCityOfCorsairs attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      chooseOneM pid do
        questionLabeled "stormcaller.choose"
        causedBy attrs
        labeled "stormcaller.return" $ placeEnemy attrs.id StagingArea
        labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse attrs
      pure e
    _ -> StormcallerTheCityOfCorsairs <$> runMessage msg attrs
