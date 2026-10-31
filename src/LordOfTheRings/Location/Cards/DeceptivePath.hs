module LordOfTheRings.Location.Cards.DeceptivePath (deceptivePath) where

import LordOfTheRings.Card (CardDef (..), CardType (LocationType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype DeceptivePath = DeceptivePath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

deceptivePath :: LocationCard DeceptivePath
deceptivePath = location DeceptivePath Cards.deceptivePath

-- | Progress cannot be placed on Deceptive Path while it is in the staging area.
instance HasModifiersFor DeceptivePath where
  getModifiersFor (DeceptivePath a) = when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]

instance HasAbilities DeceptivePath where
  getAbilities (DeceptivePath a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Forced: After Deceptive Path becomes the active location, shuffle the encounter discard
-- pile into the encounter deck and discard cards from the top of the encounter deck until
-- a location is discarded. Reveal that location and make it the active location. Then,
-- discard Deceptive Path.
instance RunMessage DeceptivePath where
  runMessage msg l@(DeceptivePath a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
      discardTopOfEncounterDeck a $ maybe (length deck) (+ 1) (findIndex isLocation deck)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (find isLocation cards) \found -> do
        discard a
        removeFromEncounterDiscard found
        putIntoPlayAsActiveLocation found
      pure l
    _ -> DeceptivePath <$> runMessage msg a
   where
    isLocation = (== LocationType) . cdCardType . toCardDef
