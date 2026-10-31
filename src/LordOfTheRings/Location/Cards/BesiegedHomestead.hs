module LordOfTheRings.Location.Cards.BesiegedHomestead (besiegedHomestead) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers, withVillagers)

newtype BesiegedHomestead = BesiegedHomestead LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Villagers 2.
besiegedHomestead :: LocationCard BesiegedHomestead
besiegedHomestead = location (BesiegedHomestead . withVillagers 2) Cards.besiegedHomestead

-- | Forced: When any amount of progress is placed on a location in the staging area,
-- discard 1 villager token from that location. Read off the tokens as they are placed.
-- Travel: The first player searches the encounter deck for an enemy, reveals it, and puts
-- it into play engaged with him. Shuffle the encounter deck.
instance RunMessage BesiegedHomestead where
  runMessage msg l@(BesiegedHomestead a) = case msg of
    PlaceTokens _ (LocationTarget lid) Progress n | n > 0 -> do
      staged <- selectAny (LocationWithId lid <> LocationInStagingArea)
      when staged $ void $ discardVillagers a lid 1
      BesiegedHomestead <$> runMessage msg a
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      enemies <- getsGame (filter ((== EnemyType) . cdCardType . toCardDef) . (.encounterDeck) . toAttrs . (.scenario))
      chooseOrRunOneM lead do
        questionLabeled "besiegedHomestead.enemy"
        causedBy a
        targets enemies \card -> do
          revealEncounterCardEngaged lead card
          shuffleEncounterDeck
      pure l
    _ -> BesiegedHomestead <$> runMessage msg a
