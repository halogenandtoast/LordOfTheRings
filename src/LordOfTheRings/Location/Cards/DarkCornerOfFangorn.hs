module LordOfTheRings.Location.Cards.DarkCornerOfFangorn (darkCornerOfFangorn) where

import LordOfTheRings.Keyword (Keyword (Hinder))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Trait (Trait (Huorn))

newtype DarkCornerOfFangorn = DarkCornerOfFangorn LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

darkCornerOfFangorn :: LocationCard DarkCornerOfFangorn
darkCornerOfFangorn = location DarkCornerOfFangorn Cards.darkCornerOfFangorn

-- | While Dark Corner of Fangorn is in the staging area, each Huorn enemy loses the hinder
-- keyword.
instance HasModifiersFor DarkCornerOfFangorn where
  getModifiersFor (DarkCornerOfFangorn a) = when (a.placement == StagingArea) do
    huorns <- select (EnemyWithTrait Huorn)
    for_ huorns \e -> modified a e [LoseKeyword Hinder]

-- | Forced: After Dark Corner of Fangorn becomes the active location, each engaged Huorn
-- enemy makes an immediate attack.
instance HasAbilities DarkCornerOfFangorn where
  getAbilities (DarkCornerOfFangorn a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage DarkCornerOfFangorn where
  runMessage msg l@(DarkCornerOfFangorn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      huorns <- traverse getEnemy =<< select (EnemyWithTrait Huorn <> EnemyEngagedWith AnyPlayer)
      for_ huorns \e -> case e.placement of
        EngagedWith pid -> immediateAttack pid e.id
        _ -> pure ()
      pure l
    _ -> DarkCornerOfFangorn <$> runMessage msg a
