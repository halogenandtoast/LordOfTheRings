module LordOfTheRings.Quest.Cards.TheWickedWillow (theWickedWillow) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Massive))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheWickedWillow = TheWickedWillow QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWickedWillow :: QuestCard TheWickedWillow
theWickedWillow = quest TheWickedWillow Cards.theWickedWillow

instance HasModifiersFor TheWickedWillow where
  getModifiersFor (TheWickedWillow a) = when a.flipped do
    willows <- select $ EnemyWithCardCode (toCardCode Enemies.oldManWillow)
    for_ willows \eid -> modified a eid [GainKeyword Massive, CannotLeaveStagingArea]
    -- Withywindle's printed victory restriction keeps the last stage pending.
    blocked <- selectAny $ LocationWithCardCode (toCardCode Locations.withywindle)
    when blocked $ modifySelf a [CannotBeDefeated]

instance RunMessage TheWickedWillow where
  runMessage msg q@(TheWickedWillow a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      TheWickedWillow <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      addSetAsideToStaging (cardIs Enemies.oldManWillow)
      addSetAsideToStaging (cardIs Locations.withywindle)
      players <- length <$> getPlayerOrder
      revealEncounterCards (max 0 $ players - 1)
      pure q
    CompleteQuest qid | qid == a.id -> q <$ winGame
    _ -> TheWickedWillow <$> runMessage msg a
