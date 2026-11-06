module LordOfTheRings.Quest.Cards.TheShadowHost (theShadowHost) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Battle))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)
import LordOfTheRings.Trait (Trait (Oathbreaker))

newtype TheShadowHost = TheShadowHost QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theShadowHost :: QuestCard TheShadowHost
theShadowHost = quest TheShadowHost Cards.theShadowHost

instance HasModifiersFor TheShadowHost where
  getModifiersFor (TheShadowHost a) = when a.flipped do
    time <- currentNight
    when (time == Just Midnight) $ modifySelf a [GainKeyword Battle]

instance RunMessage TheShadowHost where
  runMessage msg q@(TheShadowHost a) = case msg of
    Flip (isTarget a -> True) | not a.flipped -> do
      askToResolveWhenRevealed a
      TheShadowHost <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscardToReveal pid a (CardWithType EnemyType <> CardWithTrait Oathbreaker)
      addSetAsideToStaging (cardIs Enemies.theLordOfTheDeadErech)
      shuffleEncounterDeck
      pure q
    PlaceTokens _ (isTarget a -> True) Progress _ | a.flipped -> do
      rescued <- selectAny $ AllyWithCardCode (toCardCode Objectives.derufin) <> AllyControlledBy AnyPlayer
      if rescued then TheShadowHost <$> runMessage msg a else pure q
    CompleteQuest qid | qid == a.id -> q <$ winGame
    _ -> TheShadowHost <$> runMessage msg a
