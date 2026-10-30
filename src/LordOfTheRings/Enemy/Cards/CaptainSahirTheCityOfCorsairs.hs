module LordOfTheRings.Enemy.Cards.CaptainSahirTheCityOfCorsairs (captainSahirTheCityOfCorsairs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

-- | The City of Corsairs' Captain Sahír: single-sided, not The Stormcaller Elite's.
newtype CaptainSahirTheCityOfCorsairs = CaptainSahirTheCityOfCorsairs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

captainSahirTheCityOfCorsairs :: EnemyCard CaptainSahirTheCityOfCorsairs
captainSahirTheCityOfCorsairs = enemy CaptainSahirTheCityOfCorsairs Cards.captainSahirTheCityOfCorsairs

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | Captain Sahír engages the first player.
instance HasModifiersFor CaptainSahirTheCityOfCorsairs where
  getModifiersFor (CaptainSahirTheCityOfCorsairs a) = do
    lead <- getLead
    modifySelf a [EngagesOnly lead]

{- | For each point of damage Captain Sahír would take, discard 1 resource from him and
cancel that damage.
For every 5 resources on Captain Sahír, he makes an additional attack against the engaged
player each combat phase: once the player's enemies have made their attacks, counted then.
-}
instance RunMessage CaptainSahirTheCityOfCorsairs where
  runMessage msg e@(CaptainSahirTheCityOfCorsairs attrs) = case msg of
    CheckWindows ws
      | n : _ <- [n | Window When (DealtDamage t n) <- ws, t == toTarget attrs]
      , resources attrs > 0 -> do
          let k = min n (resources attrs)
          cancelSomeDamage attrs k
          removeTokens attrs attrs Resource k
          CaptainSahirTheCityOfCorsairs <$> runMessage msg attrs
    -- the game's own attacks against the player go on the queue before this step
    EnemyAttacks pid | EngagedWith pid == attrs.placement -> do
      doStep 1 msg
      pure e
    DoStep 1 (EnemyAttacks pid) | EngagedWith pid == attrs.placement -> do
      replicateM_ (resources attrs `div` 5) $ immediateAttack pid attrs.id
      pure e
    _ -> CaptainSahirTheCityOfCorsairs <$> runMessage msg attrs
