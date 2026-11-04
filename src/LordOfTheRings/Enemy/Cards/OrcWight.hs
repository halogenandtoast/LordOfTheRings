module LordOfTheRings.Enemy.Cards.OrcWight (orcWight) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcWight = OrcWight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcWight :: EnemyCard OrcWight
orcWight = enemy OrcWight Cards.orcWight

-- | Forced: After Orc Wight enters play, deal it 2 shadow cards.
instance HasAbilities OrcWight where
  getAbilities (OrcWight a) =
    [ mkAbility a 1 $ forced $ EntersPlayWindow After (toTarget a)
    ]

-- | Shadow: If attacking enemy has at least 1 other shadow card currently dealt to it,
-- return it to the staging area after this attack.
instance RunMessage OrcWight where
  runMessage msg e@(OrcWight attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      replicateM_ 2 $ dealShadowCard attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      others <- filter ((/= cid) . (.id)) . (.shadowCards) <$> getEnemy eid
      unless (null others) $ returnToStagingAfterAttack eid
      pure e
    _ -> OrcWight <$> runMessage msg attrs
