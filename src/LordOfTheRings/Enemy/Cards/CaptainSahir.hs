module LordOfTheRings.Enemy.Cards.CaptainSahir (captainSahir) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | The Stormcaller Elite's Captain Sahír, enemy side faceup: flipped, he is the
-- objective-ally of "LordOfTheRings.Ally.Cards.CaptainSahir".
newtype CaptainSahir = CaptainSahir EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

captainSahir :: EnemyCard CaptainSahir
captainSahir = enemy CaptainSahir Cards.captainSahir

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | Captain Sahír engages the first player.
instance HasModifiersFor CaptainSahir where
  getModifiersFor (CaptainSahir a) = do
    lead <- getLead
    modifySelf a [EngagesOnly lead]

-- | Forced: After Captain Sahír attacks, place 2 resource tokens on him (4 instead if his
-- attack destroyed a character).
instance HasAbilities CaptainSahir where
  getAbilities (CaptainSahir a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | For each point of damage Captain Sahír would take, discard 1 resource from him and
-- cancel that damage.
instance RunMessage CaptainSahir where
  runMessage msg e@(CaptainSahir attrs) = case msg of
    CheckWindows ws
      | n : _ <- [n | Window When (DealtDamage t n) <- ws, t == toTarget attrs]
      , resources attrs > 0 -> do
          let k = min n (resources attrs)
          cancelSomeDamage attrs k
          removeTokens attrs attrs Resource k
          CaptainSahir <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      destroyed <- attackDestroyedACharacter attrs.id
      placeResources attrs attrs (if destroyed then 4 else 2)
      pure e
    _ -> CaptainSahir <$> runMessage msg attrs
