module LordOfTheRings.Quest.Cards.JourneyInTheBlackPit (journeyInTheBlackPit) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (attachCaveTorch)
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (triggerLostEffects)

newtype JourneyInTheBlackPit = JourneyInTheBlackPit QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyInTheBlackPit :: QuestCard JourneyInTheBlackPit
journeyInTheBlackPit = quest JourneyInTheBlackPit Cards.journeyInTheBlackPit

-- | Each location gets +1 threat.
instance HasModifiersFor JourneyInTheBlackPit where
  getModifiersFor (JourneyInTheBlackPit a) = when a.flipped do
    locations <- select AnyLocation
    for_ locations \l -> modified a l [ThreatModifier 1]

-- | If the players quest unsuccessfully, trigger all "Lost:" effects in play.
instance HasAbilities JourneyInTheBlackPit where
  getAbilities (JourneyInTheBlackPit a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After | a.flipped]

-- | Setup: The first player attaches Cave Torch to a hero of his choice.
-- When Revealed: Discard cards from the top of the encounter deck until you discard X
-- locations, where X is one less than the number of players in the game (minimum of 1).
-- Add those locations to the staging area, and shuffle the other discarded cards back into
-- the encounter deck.
instance RunMessage JourneyInTheBlackPit where
  runMessage msg q@(JourneyInTheBlackPit attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      attachCaveTorch attrs
      players <- length <$> getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- ponytail: the cards above them would be shuffled straight back, so only the
      -- locations leave the deck
      let locations = take (max 1 $ players - 1) $ filter ((== LocationType) . cdCardType . toCardDef) deck
      for_ locations \l -> do
        removeFromEncounterDeck l
        addToStaging l
      shuffleEncounterDeck
      pure $ JourneyInTheBlackPit $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      triggerLostEffects attrs
      pure q
    _ -> JourneyInTheBlackPit <$> runMessage msg attrs
