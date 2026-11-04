module LordOfTheRings.Location.Cards.RiverHarnen (riverHarnen) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype RiverHarnen = RiverHarnen LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

riverHarnen :: LocationCard RiverHarnen
riverHarnen = location RiverHarnen Cards.riverHarnen

-- | Immune to player card effects. The players cannot travel here unless there are at
-- least 20 progress tokens on stage 4B. Stage 4B cannot be defeated while River Harnen is
-- in play.
instance HasModifiersFor RiverHarnen where
  getModifiersFor (RiverHarnen a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    fords <- getsGame (filter ((== toCardCode Quests.theFordOfHarnen) . toCardCode) . toList . (.entities.quests))
    when (sum (map (.progress) fords) < 20) $ modifySelf a [CannotBeTraveledTo]
    for_ fords \q -> modified a (toTarget q) [CannotBeDefeated]

-- | Travel: Reveal 1 encounter card per player.
instance RunMessage RiverHarnen where
  runMessage msg (RiverHarnen a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \_ -> revealEncounterCard
      RiverHarnen <$> runMessage msg a
    _ -> RiverHarnen <$> runMessage msg a
