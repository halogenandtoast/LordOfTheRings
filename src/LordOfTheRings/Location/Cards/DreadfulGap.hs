module LordOfTheRings.Location.Cards.DreadfulGap (dreadfulGap) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DreadfulGap = DreadfulGap LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dreadfulGap :: LocationCard DreadfulGap
dreadfulGap = location DreadfulGap Cards.dreadfulGap

-- | X is the number of characters in play.
instance HasModifiersFor DreadfulGap where
  getModifiersFor (DreadfulGap a) = do
    n <- length <$> selectCharacters AnyHero AnyAlly
    modified a a [QuestPointModifier n]

-- | When Revealed: Immediately travel to Dreadful Gap. If another location is currently
-- active, return it to the staging area.
instance RunMessage DreadfulGap where
  runMessage msg l@(DreadfulGap a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      active <- select IsActiveLocation
      for_ active \other -> placeLocation other StagingArea
      travelWithoutCost a.id
      pure l
    _ -> DreadfulGap <$> runMessage msg a
