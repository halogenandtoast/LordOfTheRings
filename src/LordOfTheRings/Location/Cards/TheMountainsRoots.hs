module LordOfTheRings.Location.Cards.TheMountainsRoots (theMountainsRoots) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheMountainsRoots = TheMountainsRoots LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMountainsRoots :: LocationCard TheMountainsRoots
theMountainsRoots = location TheMountainsRoots Cards.theMountainsRoots

-- | X is the number of players in the game.
instance HasModifiersFor TheMountainsRoots where
  getModifiersFor (TheMountainsRoots a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ThreatModifier players, QuestPointModifier players]

-- | Shadow: attacking enemy gets +X attack. X is the number of players in the game.
instance RunMessage TheMountainsRoots where
  runMessage msg l@(TheMountainsRoots a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid . length =<< getRemainingPlayers
      pure l
    _ -> TheMountainsRoots <$> runMessage msg a
