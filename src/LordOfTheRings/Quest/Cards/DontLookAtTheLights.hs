module LordOfTheRings.Quest.Cards.DontLookAtTheLights (dontLookAtTheLights) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (firstPlayerControlsSmeagol, searchDeckAndDiscardFor, undeadEnemy)
import LordOfTheRings.Trait (Trait (Undead))

newtype DontLookAtTheLights = DontLookAtTheLights QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dontLookAtTheLights :: QuestCard DontLookAtTheLights
dontLookAtTheLights = quest DontLookAtTheLights Cards.dontLookAtTheLights

-- | While any player is engaged with an Undead enemy, progress cannot be placed on this
-- stage.
instance HasModifiersFor DontLookAtTheLights where
  getModifiersFor (DontLookAtTheLights a) = when a.flipped do
    engaged <- selectAny (EnemyWithTrait Undead <> EnemyEngagedWith AnyPlayer)
    when engaged $ modifySelf a [ProgressLimit 0]

{- | When Revealed: Each player searches the encounter deck and discard pile for an Undead
enemy and puts it into play engaged with him. Shuffle all locations in the victory display
into the encounter deck. One player after another, each searching as the one before left
them.

The players cannot defeat this stage unless the first player controls Sméagol. If the
players defeat this stage, they win the game: it is the last in the quest deck.
-}
instance RunMessage DontLookAtTheLights where
  runMessage msg q@(DontLookAtTheLights attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DontLookAtTheLights <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure q
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          searchDeckAndDiscardFor pid "dontLookAtTheLights.search" undeadEnemy (putEnemyIntoPlayEngaged pid)
          doStep (n + 1) inner
        [] -> shuffleVictoryDisplayIntoEncounterDeck (CardWithType LocationType)
      pure q
    Flip _ -> recheck
    SetFirstPlayer _ -> recheck
    _ -> DontLookAtTheLights <$> runConditionalStage firstPlayerControlsSmeagol msg attrs
   where
    recheck = do
      push $ CheckStage attrs.id
      DontLookAtTheLights <$> runConditionalStage firstPlayerControlsSmeagol msg attrs
