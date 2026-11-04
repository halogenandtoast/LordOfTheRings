module LordOfTheRings.Quest.Cards.CapturedBySpiders (capturedBySpiders) where

import LordOfTheRings.Card (genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameSeparation)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (readyUnconsciousCharacterOf, spendToReadyUnconscious, unconsciousRestrictions)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck))

newtype CapturedBySpiders = CapturedBySpiders QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

capturedBySpiders :: QuestCard CapturedBySpiders
capturedBySpiders = quest CapturedBySpiders Cards.capturedBySpiders

-- | Unconscious characters cannot quest, attack, defend, collect resources, trigger
-- abilities, be poisoned, or ready (except by effects that target unconscious characters).
-- Do not pass the first player token.
instance HasModifiersFor CapturedBySpiders where
  getModifiersFor (CapturedBySpiders a) = when a.flipped do
    unconsciousRestrictions a
    lead <- getLead
    modified a (PlayerTarget lead) [CannotLoseFirstPlayerToken]

-- | Action: Spend 2 Baggins resources to ready an unconscious character you control.
instance HasAbilities CapturedBySpiders where
  getAbilities (CapturedBySpiders a) = [spendToReadyUnconscious a 1 | a.flipped]

{- | When Revealed: Remove all resources from each hero's resource pool. Then, make each
character you control (except Bilbo Baggins) unconscious. ("You" is the first player,
whose stage this is; the heroes are those in his area.)

When this stage is complete, do not advance to stage 4 until the end of the quest phase.
(Combine staging areas if necessary.) Apart from the other players, the first player joins
them at the end of the quest phase, and they are at stage 4 together (see
'LordOfTheRings.Game.Base.Separation'); alone, he advances then.
-}
instance RunMessage CapturedBySpiders where
  runMessage msg q@(CapturedBySpiders attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      CapturedBySpiders <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      removeAllResourcesFromEachHero attrs
      lead <- getLead
      bilbo <- map toTarget <$> select (HeroWithTitle "Bilbo Baggins")
      characters <- filter (`notElem` bilbo) <$> charactersControlledBy lead
      traverse_ makeUnconscious characters
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      readyUnconsciousCharacterOf attrs pid
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      let attrs' = attrs & tokensL %~ addTokens Progress n
      separated <- getsGame (isJust . gameSeparation)
      before <- hasEnoughProgress attrs
      done <- hasEnoughProgress attrs'
      when (separated && done && not before) joinAnotherPlayer
      pure $ CapturedBySpiders attrs'
    -- complete, it waits for the end of the quest phase
    CheckStage qid | qid == attrs.id -> pure q
    EndPhase QuestPhase -> do
      separated <- getsGame (isJust . gameSeparation)
      done <- hasEnoughProgress attrs
      when (done && not separated) do
        -- the other players' stage 4 left with them, if they left the game while apart
        questDeck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
        unless (any ((== toCardCode Cards.battleWithTheSpiders) . toCardCode) questDeck) do
          stage4 <- genEncounterCard Cards.battleWithTheSpiders
          push $ PutOnBottomOfQuestDeck stage4
        push $ CompleteQuest attrs.id
      CapturedBySpiders <$> runMessage msg attrs
    _ -> CapturedBySpiders <$> runMessage msg attrs
