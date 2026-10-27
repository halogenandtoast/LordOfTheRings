module LordOfTheRings.Quest.Cards.OutOfTheDepths (outOfTheDepths) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OutOfTheDepths = OutOfTheDepths QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheDepths :: QuestCard OutOfTheDepths
outOfTheDepths = quest OutOfTheDepths Cards.outOfTheDepths

-- | Each player cannot commit more allies to the quest than the number of heroes he is
-- also committing to the quest.
instance HasModifiersFor OutOfTheDepths where
  getModifiersFor (OutOfTheDepths a) = do
    players <- getRemainingPlayers
    for_ players \pid -> do
      heroes <- length <$> select (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
      allies <- length <$> select (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
      when (allies >= heroes) do
        waiting <- select (AllyControlledBy (PlayerWithId pid) <> NotAlly CommittedAlly)
        for_ waiting \ally -> modified a ally [CannotCommitToQuest]

-- | When Revealed: Reveal 1 card from the encounter deck per player, and add it to the
-- staging area.
-- If the players defeat this stage, they have won the game (it is the last in the quest deck).
instance RunMessage OutOfTheDepths where
  runMessage msg q@(OutOfTheDepths attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      OutOfTheDepths <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer $ const revealEncounterCard
      pure q
    _ -> OutOfTheDepths <$> runMessage msg attrs
