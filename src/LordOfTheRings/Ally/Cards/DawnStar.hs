module LordOfTheRings.Ally.Cards.DawnStar (dawnStar) where

import LordOfTheRings.Ally.Fleet
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype DawnStar = DawnStar AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

dawnStar :: AllyCard DawnStar
dawnStar = ally DawnStar Cards.dawnStar

-- | Immune to player card effects. Draw 1 additional card during the resource phase.
instance HasModifiersFor DawnStar where
  getModifiersFor (DawnStar a) = do
    shipModifiers a
    for_ (shipController a) \pid -> modified a (PlayerTarget pid) [AdditionalResourcePhaseDraws 1]

-- | Raise your starting threat by 3: as the fleet is prepared, it enters play.
instance HasAbilities DawnStar where
  getAbilities (DawnStar a) = [mkAbility a 1 $ silent $ EntersPlayWindow After (toTarget a)]

-- | If the Dawn Star is destroyed, its controller is eliminated from the game.
instance RunMessage DawnStar where
  runMessage msg s@(DawnStar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ (shipController a) \pid -> raiseThreat pid 3
      pure s
    _ -> DawnStar <$> runShip (traverse_ eliminatePlayer (shipController a)) msg a
