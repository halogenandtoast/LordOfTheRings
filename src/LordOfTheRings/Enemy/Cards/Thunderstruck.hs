module LordOfTheRings.Enemy.Cards.Thunderstruck (thunderstruck) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Thunderstruck = Thunderstruck EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign.
thunderstruck :: EnemyCard Thunderstruck
thunderstruck = enemy Thunderstruck Cards.thunderstruck

-- | Archery 1. Boarding 2. Immune to player card effects. Thunderstruck gets +2 hit points
-- per player and cannot take more than 4 damage each round.
instance HasModifiersFor Thunderstruck where
  getModifiersFor (Thunderstruck a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ImmuneToPlayerCardEffects, HitPointModifier (2 * players), DamageLimit 4]

-- | Shadow: Either add Thunderstruck to the staging area or deal 2 damage to the defending
-- character.
instance RunMessage Thunderstruck where
  runMessage msg e@(Thunderstruck attrs) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      chooseOneM pid do
        questionLabeled "thunderstruck.shadow"
        causedBy attrs
        labeled "thunderstruck.staging" $ putShadowEnemyIntoStaging eid cid
        unless (null defenders) $ labeled "thunderstruck.damage" $ for_ defenders \d -> dealDamage attrs d 2
      pure e
    _ -> Thunderstruck <$> runMessage msg attrs
