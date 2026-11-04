module LordOfTheRings.Location.Cards.LakeInTheCavern (lakeInTheCavern) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (cdStage, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)

newtype LakeInTheCavern = LakeInTheCavern LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lakeInTheCavern :: LocationCard LakeInTheCavern
lakeInTheCavern = location LakeInTheCavern Cards.lakeInTheCavern

-- | X is twice the number of players in the game.
-- Players cannot travel here. Immune to player card effects.
instance HasModifiersFor LakeInTheCavern where
  getModifiersFor (LakeInTheCavern a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier (2 * players), CannotBeTraveledTo, ImmuneToPlayerCardEffects]

-- | Forced: After players advance to stage 3, remove Lake in the Cavern from the game.
instance RunMessage LakeInTheCavern where
  runMessage msg (LakeInTheCavern attrs) = case msg of
    CheckWindows ws | Window After AdvancedToStage `elem` ws -> do
      atStage3 <- getsGame (any ((== Just 3) . cdStage . toCardDef) . toList . (.entities.quests))
      when atStage3 $ removeFromGame attrs
      LakeInTheCavern <$> runMessage msg attrs
    _ -> LakeInTheCavern <$> runMessage msg attrs
