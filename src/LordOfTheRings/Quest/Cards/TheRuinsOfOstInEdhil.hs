module LordOfTheRings.Quest.Cards.TheRuinsOfOstInEdhil (theRuinsOfOstInEdhil) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (..), CardType (LocationType), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (bellachShielded, mouldOnHero, triggerEachScour)
import LordOfTheRings.Trait (Trait (OstInEdhil))

newtype TheRuinsOfOstInEdhil = TheRuinsOfOstInEdhil QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRuinsOfOstInEdhil :: QuestCard TheRuinsOfOstInEdhil
theRuinsOfOstInEdhil = quest TheRuinsOfOstInEdhil Cards.theRuinsOfOstInEdhil

-- | Bellach is immune to player card effects and cannot be engaged.
instance HasModifiersFor TheRuinsOfOstInEdhil where
  getModifiersFor (TheRuinsOfOstInEdhil a) = bellachShielded a

-- | Time 3. Forced: After the last time counter is removed from this stage, trigger each
-- Scour effect currently in play. Place 3 time counters on this stage.
instance HasAbilities TheRuinsOfOstInEdhil where
  getAbilities (TheRuinsOfOstInEdhil a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Setup: Add Bellach, The Orcs' Search, and The Secret Chamber to the staging area. Attach
-- Celebrimbor's Mould to The Secret Chamber. Each player adds a different Ost-in-Edhil
-- location to the staging area. Shuffle the encounter deck.
-- The players cannot advance unless Celebrimbor's Mould is attached to a hero.
instance RunMessage TheRuinsOfOstInEdhil where
  runMessage msg q@(TheRuinsOfOstInEdhil attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- encounterDeck
      for_ (mapMaybe (firstCopy deck) [Enemies.bellachCelebrimborsSecret, Objectives.theOrcsSearch, Locations.theSecretChamber]) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      doStep 1 msg
      eachPlayer \pid -> doStep 2 (WithTarget (PlayerTarget pid) msg)
      shuffleEncounterDeck
      pure $ TheRuinsOfOstInEdhil $ attrs {questFlipped = True}
    -- Celebrimbor's Mould, onto The Secret Chamber now in the staging area
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      mould <- (`firstCopy` Objectives.celebrimborsMould) <$> encounterDeck
      chamber <- select (LocationWithCardCode $ toCardCode Locations.theSecretChamber)
      for_ ((,) <$> mould <*> headMay chamber) \(card, lid) -> do
        removeFromEncounterDeck card
        putObjectiveIntoPlayAttached card lid
      pure q
    -- the player adds an Ost-in-Edhil location, its title different from those added
    DoStep 2 (WithTarget (PlayerTarget pid) (Flip (isTarget attrs -> True))) -> do
      added <- traverse getLocation =<< select (LocationInStagingArea <> LocationWithTrait OstInEdhil)
      let taken = map (cdName . toCardDef) added
          ostInEdhil c =
            cdCardType (toCardDef c) == LocationType
              && OstInEdhil `member` cdCardTraits (toCardDef c)
              && cdName (toCardDef c) `notElem` taken
      options <- filter ostInEdhil <$> encounterDeck
      chooseOrRunOneM pid do
        questionLabeled "theRuinsOfOstInEdhil.addLocation"
        causedBy attrs
        showing (map EncounterCard options)
        targets options \card -> do
          removeFromEncounterDeck card
          addToStaging card
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      triggerEachScour
      placeTimeCounters attrs attrs 3
      pure q
    _ -> TheRuinsOfOstInEdhil <$> runConditionalStage mouldOnHero msg attrs
   where
    encounterDeck = getsGame ((.encounterDeck) . toAttrs . (.scenario))
    firstCopy deck def = find ((== toCardCode def) . toCardCode) deck
