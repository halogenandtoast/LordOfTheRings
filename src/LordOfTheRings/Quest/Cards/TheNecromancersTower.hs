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
import LordOfTheRings.Scenario.Scenarios.MirkwoodPaths (campaignPrisoner)

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
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
      let objectives = [c | c <- deck, cdCardType (toCardDef c) == ObjectiveType]
          nazgul = [c | c <- deck, toCardCode c == toCardCode Enemies.nazgulOfDolGuldur]
      for_ objectives $ push . RemoveFromEncounterDeck
      for_ nazgul $ push . SetAside
      push $ ShuffleDeck EncounterDeck
      for_ objectives $ push . PutObjectiveIntoPlay
      askToResolveWhenRevealed attrs
      pure $ TheNecromancersTower $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      -- in campaign mode, the prisoner the Campaign Log names (Escape from Dol Guldur's
      -- campaign card) is the one randomly selected first
      named <- campaignPrisoner
      -- Nightmare: "Forced: When stage 1B is revealed, the number of random heroes captured
      -- is determined by the number of players: 1-2 players = 1, 3 players = 2, 4 players =
      -- 3. (A single player cannot have more than 1 hero captured by this effect.)"
      nightmare <- inNightmareMode
      if nightmare
        then do
          players <- getRemainingPlayers
          captives <- take (max 1 (length players - 1)) <$> shuffleM players
          -- the named prisoner's controller is among the captives, the named hero theirs
          namedOwner <- traverse (fmap (.owner) . getHero) named
          let captives' = case namedOwner of
                Just p | p `notElem` captives -> p : take (length captives - 1) captives
                _ -> captives
          for_ captives' \p ->
            case named of
              Just h | Just p == namedOwner -> push $ TakePrisoner h
              _ -> takeRandomPrisoner (HeroControlledBy $ PlayerWithId p)
        else maybe (takeRandomPrisoner AnyHero) (push . TakePrisoner) named
      pure q
    _ -> TheNecromancersTower <$> runConditionalStage ((>= 1) <$> claimedObjectives) msg attrs
   where
    takeRandomPrisoner matcher = do
      heroes <- select matcher
      for_ (nonEmpty heroes) \hs -> push . TakePrisoner =<< uniform (toList hs)
