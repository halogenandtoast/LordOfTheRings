module LordOfTheRings.Location.Cards.PlainOfIsengard (plainOfIsengard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (wizardry)

newtype PlainOfIsengard = PlainOfIsengard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

plainOfIsengard :: LocationCard PlainOfIsengard
plainOfIsengard = location PlainOfIsengard Cards.plainOfIsengard

-- | X is the number of players in the game. While Plain of Isengard is in the staging
-- area, archery damage must be assigned to allies, if able.
instance HasModifiersFor PlainOfIsengard where
  getModifiersFor (PlainOfIsengard a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier players]
    when (a.placement == StagingArea) $ modified a GameTarget [ArcheryDamageToAllies]

-- | Wizardry: Each player discards 1 ally he controls (see 'wizardry').
instance HasAbilities PlainOfIsengard where
  getAbilities (PlainOfIsengard a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage PlainOfIsengard where
  runMessage msg l@(PlainOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      wizardry a (toCardCode a) =<< getRemainingPlayers
      pure l
    _ -> PlainOfIsengard <$> runMessage msg a
