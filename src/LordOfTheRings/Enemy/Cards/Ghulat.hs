module LordOfTheRings.Enemy.Cards.Ghulat (ghulat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, villagerDies)

newtype Ghulat = Ghulat EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

ghulat :: EnemyCard Ghulat
ghulat = enemy Ghulat Cards.ghulat

-- | X is the number of damage tokens on Dead Villagers.
instance HasModifiersFor Ghulat where
  getModifiersFor (Ghulat a) = do
    x <- deadVillagerCount
    when (x > 0) $ modified a a [AttackModifier x]

-- | Forced: When Ghulat attacks, place 1 damage token on Dead Villagers.
instance HasAbilities Ghulat where
  getAbilities (Ghulat a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | While Ghulat is in play, the game cannot end: Protect the Villagers cannot be defeated.
-- ponytail: a loss (Lord Alcaron leaving play, every player eliminated) still ends the game
-- at once rather than once Ghulat leaves play; the players lose either way
instance RunMessage Ghulat where
  runMessage msg e@(Ghulat attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      villagerDies attrs
      pure e
    _ -> Ghulat <$> runMessage msg attrs
