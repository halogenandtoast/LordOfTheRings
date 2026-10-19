module LordOfTheRings.Quest.Cards.AChosenPathDontLeaveThePath (aChosenPathDontLeaveThePath) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher
import LordOfTheRings.Outcome
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Spider))

newtype AChosenPathDontLeaveThePath = AChosenPathDontLeaveThePath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aChosenPathDontLeaveThePath :: QuestCard AChosenPathDontLeaveThePath
aChosenPathDontLeaveThePath = quest AChosenPathDontLeaveThePath Cards.aChosenPathDontLeaveThePath

instance RunMessage AChosenPathDontLeaveThePath where
  runMessage msg q@(AChosenPathDontLeaveThePath attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      AChosenPathDontLeaveThePath <$> runMessage msg attrs
    -- When Revealed: Each player must search the encounter deck and discard pile for 1
    -- Spider card of his choice, and add it to the staging area.
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> push $ SearchEncounterDeckAndDiscard pid (toTarget attrs) (CardWithTrait Spider)
      pure q
    -- The players must find and defeat Ungoliant's Spawn to win this game.
    Destroy (EnemyTarget eid) -> do
      defeated <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.ungoliantsSpawn))
      when defeated winGame
      AChosenPathDontLeaveThePath <$> runMessage msg attrs
    _ -> AChosenPathDontLeaveThePath <$> runMessage msg attrs
