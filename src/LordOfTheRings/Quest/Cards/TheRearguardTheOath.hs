module LordOfTheRings.Quest.Cards.TheRearguardTheOath (theRearguardTheOath) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype TheRearguardTheOath = TheRearguardTheOath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRearguardTheOath :: QuestCard TheRearguardTheOath
theRearguardTheOath = quest TheRearguardTheOath Cards.theRearguardTheOath

instance HasModifiersFor TheRearguardTheOath where
  getModifiersFor (TheRearguardTheOath a) = when a.flipped do
    blocked <- selectAny (EnemyWithCardCode $ toCardCode Enemies.goblinTroopDarkOfMirkwood)
    when blocked $ modifySelf a [CannotBeDefeated]

instance RunMessage TheRearguardTheOath where
  runMessage msg q@(TheRearguardTheOath attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheRearguardTheOath <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.goblinTroopDarkOfMirkwood)
      others <- drop 1 <$> getRemainingPlayers
      for_ others \pid -> searchEncounterDeckAndDiscard pid attrs (CardWithType EnemyType <> CardWithTrait Goblin)
      shuffleEncounterDeck
      pure q
    _ -> TheRearguardTheOath <$> runMessage msg attrs
