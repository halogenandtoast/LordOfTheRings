module LordOfTheRings.Treachery.Cards.SurroundedByShadow (surroundedByShadow) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (getHiddenCards, takeHiddenCard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SurroundedByShadow = SurroundedByShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

surroundedByShadow :: TreacheryCard SurroundedByShadow
surroundedByShadow = treachery SurroundedByShadow Cards.surroundedByShadow

-- | When Revealed: Each player must either take 1 hidden card for each questing hero he
-- controls, or remove one hero he controls from the quest for each hidden card in his
-- play area.
instance RunMessage SurroundedByShadow where
  runMessage msg t@(SurroundedByShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- select (HeroControlledBy (PlayerWithId pid) <> CommittedHero)
        hidden <- length <$> getHiddenCards pid
        chooseOneM pid do
          questionLabeled "surroundedByShadow.choose"
          causedBy attrs
          labeled "surroundedByShadow.take" $ replicateM_ (length questing) (takeHiddenCard pid)
          labeled "surroundedByShadow.remove" $ removeHeroes attrs pid hidden questing
      pure t
    _ -> SurroundedByShadow <$> runMessage msg attrs

-- | The player removes heroes from the quest, picking them one at a time, as many as he has
-- hidden cards.
removeHeroes :: ReverseQueue m => TreacheryAttrs -> PlayerId -> Int -> [HeroId] -> m ()
removeHeroes attrs pid n heroes = when (n > 0 && not (null heroes)) do
  chooseOrRunOneM pid do
    questionLabeled "surroundedByShadow.removeHero"
    causedBy attrs
    targets heroes \h -> do
      removeFromQuest h
      removeHeroes attrs pid (n - 1) (filter (/= h) heroes)
