module LordOfTheRings.Quest.Cards.RiddlesInTheDark (riddlesInTheDark) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (RiddleArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (gollumEnemy)

newtype RiddlesInTheDark = RiddlesInTheDark QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

riddlesInTheDark :: QuestCard RiddlesInTheDark
riddlesInTheDark = quest RiddlesInTheDark Cards.riddlesInTheDark

{- | Cards in the riddle area are immune to player card effects and cannot leave the riddle
area except by quest effects.

The riddle area is this stage, Gollum placed there, Bilbo's Magic Ring and Bilbo Baggins,
whom the first player still controls. While Bilbo is there he cannot quest, attack, take
damage from cards other than Gollum, have attachments played on him by the players, or
defend except against Gollum (The Hobbit: Over Hill and Under Hill rules insert, "The
Riddle Area"). This stage is not the current quest: the players quest against stage 1.
-}
instance HasModifiersFor RiddlesInTheDark where
  getModifiersFor (RiddlesInTheDark a) = do
    modifySelf a [CannotBeCurrentQuest, ImmuneToPlayerCardEffects]
    gollums <- select gollumEnemy
    for_ gollums \g -> modified a g [ImmuneToPlayerCardEffects]
    rings <- getsGame (filter ((== toCardCode Objectives.bilbosMagicRing) . toCardCode) . toList . (.entities.objectives))
    for_ rings \r -> modified a r [ImmuneToPlayerCardEffects]
    getBilbo >>= traverse_ \b ->
      modified a b
        $ [ CannotCommitToQuest
          , CannotAttack
          , CannotDefendAgainst (NotEnemy gollumEnemy)
          , CannotHavePlayerAttachments
          , ImmuneToPlayerCardEffects
          ]
        <> maybe [CannotTakeDamage] (\g -> [CannotTakeDamageExceptFrom (EnemySource g)]) (headMay gollums)

{- | Setup: Search the encounter deck for Gollum and Bilbo's Magic Ring. Place Gollum and
Bilbo Baggins in the riddle area and attach Bilbo's Magic Ring to Bilbo Baggins. Then,
shuffle the encounter deck.

Progress tokens cannot be added to, or removed from, this quest except by answering
riddles: the scenario places what a riddle finds, and Not fair! Not fair! removes it as
its text reads. Never defeated on its own, it has stage 1 checked once it is complete.
-}
instance RunMessage RiddlesInTheDark where
  runMessage msg q@(RiddlesInTheDark attrs) = case msg of
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (filter (cardMatches $ cardIs Enemies.gollumDungeonsDeep) deck) \gollum -> do
        removeFromEncounterDeck gollum
        void $ createEnemyAt gollum RiddleArea
      getBilbo >>= traverse_ \b ->
        for_ (filter (cardMatches $ cardIs Objectives.bilbosMagicRing) deck) \ring -> do
          removeFromEncounterDeck ring
          putObjectiveIntoPlayAttached ring b
      shuffleEncounterDeck
      turnOver attrs
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress n
      | source == ScenarioSource -> do
          let attrs' = attrs & tokensL %~ addTokens Progress n
          done <- hasEnoughProgress attrs'
          when done $ traverse_ checkStage =<< firstStage
          pure $ RiddlesInTheDark attrs'
      | otherwise -> pure q
    RemoveTokens source (isTarget attrs -> True) Progress _ | not (byRiddle source) -> pure q
    CheckStage qid | qid == attrs.id -> pure q
    _ -> RiddlesInTheDark <$> runMessage msg attrs
   where
    -- a riddle answered, or Not fair! Not fair! removing it
    byRiddle = \case
      ScenarioSource -> True
      TreacherySource _ -> True
      _ -> False
    firstStage = getsGame (fmap toId . find ((== toCardCode Cards.outOfTheFryingPan) . toCardCode) . toList . (.entities.quests))
