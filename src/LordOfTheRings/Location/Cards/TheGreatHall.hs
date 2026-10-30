module LordOfTheRings.Location.Cards.TheGreatHall (theGreatHall) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard (..))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (undeadEnemy)

newtype TheGreatHall = TheGreatHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatHall :: LocationCard TheGreatHall
theGreatHall = location TheGreatHall Cards.theGreatHall

-- | Forced: When The Great Hall is explored, each player discards the top 5 cards of the
-- encounter deck and chooses an Undead enemy he discarded in this way to add to the
-- staging area.
instance HasAbilities TheGreatHall where
  getAbilities (TheGreatHall a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage TheGreatHall where
  runMessage msg l@(TheGreatHall a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfEncounterDeckFor a p 5
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      let undead = filter (cardMatches undeadEnemy) cards
      chooseOrRunOneM pid do
        questionLabeled "theGreatHall.choose"
        causedBy a
        showing (map EncounterCard undead)
        for_ undead \c -> targeting (CardIdTarget c.ecId) $ addToStagingFromDiscard c
      pure l
    _ -> TheGreatHall <$> runMessage msg a
