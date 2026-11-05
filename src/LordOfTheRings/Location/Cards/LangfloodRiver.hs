module LordOfTheRings.Location.Cards.LangfloodRiver (langfloodRiver) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (stagingAreaToTheLeft)

newtype LangfloodRiver = LangfloodRiver LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

langfloodRiver :: LocationCard LangfloodRiver
langfloodRiver = location LangfloodRiver Cards.langfloodRiver

instance HasModifiersFor LangfloodRiver where
  getModifiersFor (LangfloodRiver a) = when (a.placement == StagingArea) do
    quests <- getMainQuests
    for_ quests (\q -> modified a q [ProgressPerRoundLimit 4])

instance HasAbilities LangfloodRiver where
  getAbilities (LangfloodRiver a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage LangfloodRiver where
  runMessage msg l@(LangfloodRiver a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      next <- stagingAreaToTheLeft
      for_ next \pid -> do
        here <- getStageLead
        -- Leave play first, clearing attachments and tokens, then enter the next
        -- area as a fresh location. The original exploration still opens its After
        -- window, but must not discard the new instance of the card.
        cancelMatching \case
          Discard _ (LocationTarget lid) -> lid == a.id
          _ -> False
        discard a a
        focusAreaOf pid
        addToStagingFromDiscard (MkEncounterCard a.cardId $ toCardCode a)
        focusAreaOf here
      pure l
    _ -> LangfloodRiver <$> runMessage msg a
