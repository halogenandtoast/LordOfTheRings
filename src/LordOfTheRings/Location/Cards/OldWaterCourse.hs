module LordOfTheRings.Location.Cards.OldWaterCourse (oldWaterCourse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype OldWaterCourse = OldWaterCourse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

oldWaterCourse :: LocationCard OldWaterCourse
oldWaterCourse = location OldWaterCourse Cards.oldWaterCourse

-- | A travel cost that cannot be paid bars the travel: not while a player has no ready hero
-- to exhaust.
instance HasModifiersFor OldWaterCourse where
  getModifiersFor (OldWaterCourse a) = do
    players <- getRemainingPlayers
    stuck <- flip anyM players \p -> selectNone (HeroControlledBy (PlayerWithId p) <> ReadyHero)
    when stuck $ modifySelf a [CannotBeTraveledTo]

-- | While Old Water-course is the active location, it gains: "Response: When Old
-- Water-course is explored, reduce each player's threat by 5."
instance HasAbilities OldWaterCourse where
  getAbilities (OldWaterCourse a) = [mkAbility a 1 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

-- | Travel: Each player must exhaust a hero he controls to travel here.
instance RunMessage OldWaterCourse where
  runMessage msg l@(OldWaterCourse a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (chooseAndExhaustHero a)
      OldWaterCourse <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> lowerThreatBy a pid 5
      pure l
    _ -> OldWaterCourse <$> runMessage msg a
