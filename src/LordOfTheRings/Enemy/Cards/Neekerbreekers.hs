module LordOfTheRings.Enemy.Cards.Neekerbreekers (neekerbreekers) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Neekerbreekers = Neekerbreekers EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

neekerbreekers :: EnemyCard Neekerbreekers
neekerbreekers = enemy Neekerbreekers Cards.neekerbreekers

-- | Forced: After any number of time counters are removed from the current quest, the
-- engaged player must deal 2 damage to an ally he controls.
instance HasAbilities Neekerbreekers where
  getAbilities (Neekerbreekers a) =
    [ restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer)
        $ forced
        $ TimeCountersRemovedFromQuestWindow After
    ]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage Neekerbreekers where
  runMessage msg e@(Neekerbreekers attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> do
        allies <- select (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "neekerbreekers.damageAlly"
          causedBy attrs
          targets allies \ally -> dealDamage attrs ally 2
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> Neekerbreekers <$> runMessage msg attrs
