module LordOfTheRings.Ally.Cards.PorosGarrison (porosGarrison) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (placementController)

newtype PorosGarrison = PorosGarrison AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally: set aside at setup, the first player takes control of it as Battle
-- at the Crossings is revealed.
porosGarrison :: AllyCard PorosGarrison
porosGarrison = ally PorosGarrison Cards.porosGarrison

-- | Immune to player card effects. You can optionally engage an additional enemy during
-- the encounter phase.
instance HasModifiersFor PorosGarrison where
  getModifiersFor (PorosGarrison a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    for_ (placementController a.placement) \pid -> modified a (PlayerTarget pid) [OptionallyEngagesAdditionalEnemy]

-- | If Poros Garrison leaves play, the players lose the game.
instance HasAbilities PorosGarrison where
  getAbilities (PorosGarrison a) = [mkAbility a 1 $ forced $ LeftPlayWindow After (toTarget a)]

instance RunMessage PorosGarrison where
  runMessage msg s@(PorosGarrison a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      gameOver Lost
      pure s
    _ -> PorosGarrison <$> runMessage msg a
