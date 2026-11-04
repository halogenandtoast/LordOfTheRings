module LordOfTheRings.Quest.Cards.TheLowerDeeps (theLowerDeeps) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType), IsCard (toCard), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEffects, gameEntities)
import LordOfTheRings.Placement (Placement (StagingArea, Unplaced))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype TheLowerDeeps = TheLowerDeeps QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLowerDeeps :: QuestCard TheLowerDeeps
theLowerDeeps = quest TheLowerDeeps Cards.theLowerDeeps

-- | The 'when revealed' effects of the first treachery revealed each round cannot be
-- canceled: the first is the oldest of the round's revealed-treachery effects.
instance HasModifiersFor TheLowerDeeps where
  getModifiersFor (TheLowerDeeps a) = when a.flipped do
    effects <- getsGame gameEffects
    let revealed = [tid | Effect (TreacherySource tid) GameTarget ms _ <- effects, TreacheryRevealedThisRound `elem` ms]
    for_ (lastMay revealed) \tid -> modified a (TreacheryTarget tid) [WhenRevealedCannotBeCanceled]

instance HasAbilities TheLowerDeeps where
  getAbilities (TheLowerDeeps a) = exploringGundabad a <> bypass a

-- | Forced: When this stage is defeated, search the Caves deck for a location and make it
-- the active location. Shuffle the Caves deck. The active location before it returns to
-- the staging area, and the new one becomes active as traveling makes one, so The First
-- Forge's Forced effect answers it.
instance RunMessage TheLowerDeeps where
  runMessage msg q@(TheLowerDeeps attrs) = case msg of
    DoStep 1 (PutLocationIntoPlay card _) -> do
      found <- getsGame (find ((== card.id) . (.id) . toCard) . toList . entitiesLocations . gameEntities)
      for_ found (makeActive . toId)
      pure q
    _ -> TheLowerDeeps <$> runExploringGundabad defeated msg attrs
   where
    defeated = do
      locations <- filter ((== LocationType) . cdCardType . toCardDef) <$> getCavesDeck
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theLowerDeeps.location"
        causedBy attrs
        showing (map EncounterCard locations)
        targets locations \l -> do
          takeFromCavesDeck l
          traverse_ (`placeLocation` StagingArea) =<< select IsActiveLocation
          putLocationIntoPlay l Unplaced
          doStep 1 (PutLocationIntoPlay l Unplaced)
      shuffleCavesDeck
