module LordOfTheRings.Quest.Cards.DeadlyCreatures (deadlyCreatures) where

import LordOfTheRings.EncounterSet (EncounterSet (DesertCreatures))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (runStage3A)

newtype DeadlyCreatures = DeadlyCreatures QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

deadlyCreatures :: QuestCard DeadlyCreatures
deadlyCreatures = quest DeadlyCreatures Cards.deadlyCreatures

-- | Enemies in the staging area cannot take damage.
instance HasModifiersFor DeadlyCreatures where
  getModifiersFor (DeadlyCreatures a) = when a.flipped do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [CannotTakeDamage]

instance HasAbilities DeadlyCreatures where
  getAbilities (DeadlyCreatures a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]

{- | When Revealed: Set each other stage 3A aside, out of play. Shuffle the encounter
discard pile and the Desert Creatures encounter set into the encounter deck. Then, each
player reveals an encounter card.
Forced: After an enemy engages a player, deal 1 damage to a hero that player controls. The
player chooses the hero.
-}
instance RunMessage DeadlyCreatures where
  runMessage msg q@(DeadlyCreatures attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DeadlyCreatures <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      runStage3A DesertCreatures
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneHero attrs pid 1
      pure q
    _ -> DeadlyCreatures <$> runMessage msg attrs
