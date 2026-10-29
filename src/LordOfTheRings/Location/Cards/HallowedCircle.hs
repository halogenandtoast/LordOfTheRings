module LordOfTheRings.Location.Cards.HallowedCircle (hallowedCircle) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (controlledKeys)
import LordOfTheRings.Trait (Trait (Guardian))

newtype HallowedCircle = HallowedCircle LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

hallowedCircle :: LocationCard HallowedCircle
hallowedCircle = location HallowedCircle Cards.hallowedCircle

-- | Immune to player card effects. X is the number of players in the game. The players
-- cannot travel here without 3 ready Key objectives to exhaust.
instance HasModifiersFor HallowedCircle where
  getModifiersFor (HallowedCircle a) = do
    players <- length <$> getRemainingPlayers
    readyKeys <- length . filter (not . (.exhausted)) <$> controlledKeys
    modified a a $ [ThreatModifier players, ImmuneToPlayerCardEffects] <> [CannotBeTraveledTo | readyKeys < 3]

-- | Forced: After the players travel here, each engaged Guardian enemy makes an immediate
-- attack.
instance HasAbilities HallowedCircle where
  getAbilities (HallowedCircle a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Travel: The players must exhaust 3 Key objectives.
-- The Keys exhaust as the Travel cost is paid, before the location becomes active and the
-- "after the players travel here" window opens.
instance RunMessage HallowedCircle where
  runMessage msg l@(HallowedCircle a) = case msg of
    TravelTo lid | lid == a.id -> do
      held <- take 3 . filter (not . (.exhausted)) <$> controlledKeys
      traverse_ exhaust held
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      guardians <- select (EnemyWithTrait Guardian)
      for_ guardians \eid -> do
        e <- getEnemy eid
        for_ (placementController e.placement) \pid -> immediateAttack pid eid
      pure l
    _ -> HallowedCircle <$> runMessage msg a
