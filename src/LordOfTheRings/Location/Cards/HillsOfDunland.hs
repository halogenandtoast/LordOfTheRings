module LordOfTheRings.Location.Cards.HillsOfDunland (hillsOfDunland) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDunlandTrap.Helpers (enemyWithTrait)
import LordOfTheRings.Trait (Trait (Dunland))

newtype HillsOfDunland = HillsOfDunland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillsOfDunland :: LocationCard HillsOfDunland
hillsOfDunland = location HillsOfDunland Cards.hillsOfDunland

-- | While Hills of Dunland is in the staging area, it gains: "Forced: After a player
-- draws any number of cards, discard the top card of the encounter deck. If the discarded
-- card is a Dunland enemy, put it into play engaged with that player. This ability does
-- not stack with other copies of Hills of Dunland."
--
-- Only the copy in the staging area with the least id has it, standing for them all.
instance HasAbilities HillsOfDunland where
  getAbilities (HillsOfDunland a) =
    [ restricted a 1 (NotCriterion $ LocationExists $ LocationInStagingArea <> LocationWithCardCode (toCardCode a) <> LocationWithIdBelow a.id)
      $ forced
      $ DrewCardsWindow After Nothing
    | a.placement == StagingArea
    ]

-- | Travel: Each player draws a card to travel here.
instance RunMessage HillsOfDunland where
  runMessage msg l@(HillsOfDunland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (CardsDrawn pid _) <- ws] \pid -> discardTopOfEncounterDeckFor a (PlayerTarget pid) 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ (filter (cardMatches $ enemyWithTrait Dunland) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure l
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> drawCards a p 1
      pure l
    _ -> HillsOfDunland <$> runMessage msg a
