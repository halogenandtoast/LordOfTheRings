module LordOfTheRings.Quest.Cards.GrimbeornsQuest (grimbeornsQuest) where

import LordOfTheRings.Card (CardDef (..), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype GrimbeornsQuest = GrimbeornsQuest QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimbeornsQuest :: QuestCard GrimbeornsQuest
grimbeornsQuest = quest GrimbeornsQuest Cards.grimbeornsQuest

-- | Setup: Add The Carrock to the staging area. Remove 4 unique Troll cards and 4 copies of
-- the "Sacked!" card from the encounter deck and set them aside, out of play. Then shuffle
-- 1 "Sacked!" card per player back into the encounter deck.
-- Forced: After placing the 7th progress token on Grimbeorn's Quest, The Carrock becomes
-- the active location. Discard the previous active location from play.
instance RunMessage GrimbeornsQuest where
  runMessage msg q@(GrimbeornsQuest attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      players <- length <$> getRemainingPlayers
      deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
      let trolls = [c | c <- deck, cdUnique (toCardDef c), Troll `member` cdCardTraits (toCardDef c)]
          sacked = take 4 [c | c <- deck, toCardCode c == toCardCode Treacheries.sacked]
      searchEncounterDeckToStaging lead (cardIs Locations.theCarrock)
      -- the copies shuffled back in are simply never taken out
      for_ (trolls <> drop players sacked) $ push . SetAside
      push $ ShuffleDeck EncounterDeck
      pure $ GrimbeornsQuest $ attrs {questFlipped = True}
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.progress < 7 && attrs.progress + n >= 7 -> do
      carrock <- select (LocationWithCardCode $ toCardCode Locations.theCarrock)
      active <- select (IsActiveLocation <> not_ (LocationWithCardCode $ toCardCode Locations.theCarrock))
      for_ carrock \l -> do
        for_ active discard
        push $ PlaceLocation l ActiveLocation
      GrimbeornsQuest <$> runMessage msg attrs
    _ -> GrimbeornsQuest <$> runMessage msg attrs
