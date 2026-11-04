module LordOfTheRings.Location.Cards.HideousDepths (hideousDepths) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (isTentacleEnemy)

newtype HideousDepths = HideousDepths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hideousDepths :: LocationCard HideousDepths
hideousDepths = location HideousDepths Cards.hideousDepths

-- | With no Tentacle enemy in the encounter discard pile, its Travel cost cannot be paid.
instance HasModifiersFor HideousDepths where
  getModifiersFor (HideousDepths a) = do
    tentacles <- filter isTentacleEnemy <$> getEncounterDiscard
    when (null tentacles) $ modified a a [CannotBeTraveledTo]

-- | Forced: After a player looks at one or more cards in the encounter deck, shuffle the
-- encounter deck.
instance HasAbilities HideousDepths where
  getAbilities (HideousDepths a) = [mkAbility a 1 $ forced $ LookedAtEncounterDeckWindow After]

-- | Travel: Reveal the topmost Tentacle enemy in the encounter discard pile to travel here.
instance RunMessage HideousDepths where
  runMessage msg l@(HideousDepths a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleEncounterDeck
      pure l
    TravelTo lid | lid == a.id -> do
      tentacles <- filter isTentacleEnemy <$> getEncounterDiscard
      for_ (take 1 tentacles) revealFromEncounterDiscard
      HideousDepths <$> runMessage msg a
    _ -> HideousDepths <$> runMessage msg a
