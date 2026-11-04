module LordOfTheRings.Enemy.Cards.CryptStalker (cryptStalker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (firstPlayersTensDigit)

newtype CryptStalker = CryptStalker EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

cryptStalker :: EnemyCard CryptStalker
cryptStalker = enemy CryptStalker Cards.cryptStalker

-- | X is the tens digit of the first player's threat: its threat, attack and defense,
-- printed X.
instance HasModifiersFor CryptStalker where
  getModifiersFor (CryptStalker a) = do
    x <- firstPlayersTensDigit
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x, DefenseModifier x]

-- | Forced: When Crypt Stalker attacks, raise the defending player's threat by 1.
instance HasAbilities CryptStalker where
  getAbilities (CryptStalker a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Either raise your threat by 2, or return attacking enemy to the staging area.
-- Returned now, the attack under way still resolves, as Methedras Lookout's.
instance RunMessage CryptStalker where
  runMessage msg e@(CryptStalker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> encounterRaisesThreat pid 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "cryptStalker.choose"
        causedBy attrs
        labeled "cryptStalker.raiseThreat" $ encounterRaisesThreat pid 2
        labeled "cryptStalker.return" $ placeEnemy eid StagingArea
      pure e
    _ -> CryptStalker <$> runMessage msg attrs
