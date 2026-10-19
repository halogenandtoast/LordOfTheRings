module LordOfTheRings.Quest.Cards.TheNecromancersTower (theNecromancersTower) where

import LordOfTheRings.Card (CardDef (..), CardType (ObjectiveType), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype TheNecromancersTower = TheNecromancersTower QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNecromancersTower :: QuestCard TheNecromancersTower
theNecromancersTower = quest TheNecromancersTower Cards.theNecromancersTower

-- | The players, as a group, cannot play more than 1 ally card each round.
instance HasModifiersFor TheNecromancersTower where
  getModifiersFor (TheNecromancersTower a) = modified a GameTarget [OneAllyPerRound]

-- | Setup: Search the encounter deck for the 3 objective cards, reveal and place them in the
-- staging area. Also, place the Nazgul of Dol Guldur face up but out of play, alongside the
-- quest deck. Then, shuffle the encounter deck, and attach 1 encounter to each objective.
-- When Revealed: Randomly select 1 hero card and turn it facedown. That hero is now
-- considered a "prisoner" until it is "rescued".
-- Players cannot advance to the next stage unless they have at least 1 objective card.
instance RunMessage TheNecromancersTower where
  runMessage msg q@(TheNecromancersTower attrs) = case msg of
    DoStep 7 Setup -> do
      lead <- getLead
      push $ questionLabel "Perform setup: flip The Necromancer's Tower" lead [targetLabel attrs [Flip $ toTarget attrs]]
      pure q
    Flip (isTarget attrs -> True) -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
      let objectives = [c | c <- deck, cdCardType (toCardDef c) == ObjectiveType]
          nazgul = [c | c <- deck, toCardCode c == toCardCode Enemies.nazgulOfDolGuldur]
      heroes <- select AnyHero
      prisoner <- for (nonEmpty heroes) \hs -> TakePrisoner <$> uniform (toList hs)
      pushAll
        $ map RemoveFromEncounterDeck objectives
        <> map SetAside nazgul
        <> [ShuffleDeck EncounterDeck]
        <> map PutObjectiveIntoPlay objectives
        <> toList prisoner
      pure $ TheNecromancersTower $ attrs {questFlipped = True}
    _ -> TheNecromancersTower <$> runConditionalStage ((>= 1) <$> claimedObjectives) msg attrs
