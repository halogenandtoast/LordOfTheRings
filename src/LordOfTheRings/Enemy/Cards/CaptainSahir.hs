module LordOfTheRings.Enemy.Cards.CaptainSahir (captainSahir) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

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
      | n : _ <- [n | Window When (DealtDamage t n) <- ws, t == toTarget attrs] -> do
          -- Nightmare's Sahír's Advance: "Resources on Na'asiyah are considered to be on
          -- Captain Sahír, as well (and can be removed as if they were on him)." His own go
          -- first.
          sharing <- getsGame (any ((== toCardCode Quests.sahirsAdvanceNightmare) . toCardCode) . toList . (.entities.quests))
          naasiyah <- if sharing then select (EnemyWithCardCode $ toCardCode Cards.naasiyah) else pure []
          theirs <- for naasiyah \eid -> (eid,) . countTokens Resource . enemyTokens . toAttrs <$> getEnemy eid
          let own = resources attrs
              k = min n (own + sum (map snd theirs))
          when (k > 0) do
            cancelSomeDamage attrs k
            removeTokens attrs attrs Resource (min k own)
            for_ (take 1 theirs) \(eid, _) -> when (k > own) $ removeTokens attrs eid Resource (k - own)
          CaptainSahir <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      destroyed <- attackDestroyedACharacter attrs.id
      placeResources attrs attrs (if destroyed then 4 else 2)
      pure e
    _ -> CaptainSahir <$> runMessage msg attrs
