module LordOfTheRings.Treachery.Cards.FuryAndMalice (furyAndMalice) where

import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Trait (Trait (Raider))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FuryAndMalice = FuryAndMalice TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

furyAndMalice :: TreacheryCard FuryAndMalice
furyAndMalice = treachery FuryAndMalice Cards.furyAndMalice

-- | When Revealed: Add 1 resource to each engaged Raider enemy. Then, each player assigns
-- damage among characters he controls equal to the number of resource tokens on Raider
-- enemies engaged with him. If no damage is assigned by this effect, Fury and Malice gains
-- surge.
instance RunMessage FuryAndMalice where
  runMessage msg t@(FuryAndMalice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      raiders <- select (EnemyWithTrait Raider <> EnemyEngagedWith AnyPlayer)
      for_ raiders \r -> placeResources attrs r 1
      doStep 1 msg
      pure t
    -- the resources counted once they are added
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      owed <- for players \p -> do
        raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider <> EnemyEngagedWith (PlayerWithId p))
        pure (p, sum $ map (countTokens Resource . enemyTokens . toAttrs) raiders)
      for_ [(p, n) | (p, n) <- owed, n > 0] \(p, n) -> dealDamageDivided attrs p n
      when (all ((== 0) . snd) owed) revealEncounterCard
      pure t
    _ -> FuryAndMalice <$> runMessage msg attrs
