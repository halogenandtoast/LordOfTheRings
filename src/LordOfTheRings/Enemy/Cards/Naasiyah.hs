module LordOfTheRings.Enemy.Cards.Naasiyah (naasiyah) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | The Stormcaller Elite's Na'asiyah, enemy side faceup: flipped, she is the objective-ally
-- of "LordOfTheRings.Ally.Cards.Naasiyah".
newtype Naasiyah = Naasiyah EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

naasiyah :: EnemyCard Naasiyah
naasiyah = enemy Naasiyah Cards.naasiyah

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | Na'asiyah engages the last player.
instance HasModifiersFor Naasiyah where
  getModifiersFor (Naasiyah a) = do
    lastPlayer <- getLastPlayer
    modifySelf a [EngagesOnly lastPlayer]

-- | Forced: When Na'asiyah engages you, discard each event in your hand. Add 1 resource to
-- Na'asiyah for each event discarded in this way.
-- Forced: When Na'asiyah attacks or defends, she gets +1 attack and +1 defense for this
-- attack for each resource on her. After this attack, discard 1 resource from her.
instance HasAbilities Naasiyah where
  getAbilities (Naasiyah a) =
    mkAbility a 1 (forced $ EnemyEngagedWindow When a.id)
      : [ ab
        | resources a > 0
        , ab <- [mkAbility a 2 $ forced $ EnemyAttacksWindow When a.id, mkAbility a 3 $ forced $ EnemyAttackedWindow When a.id]
        ]

instance RunMessage Naasiyah where
  runMessage msg e@(Naasiyah attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        events <- length . filter ((== EventType) . (.cardType)) . (.hand) <$> getPlayer pid
        discardEventsFromHand pid
        when (events > 0) $ placeResources attrs attrs events
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      strengthen
      afterAttack attrs.id [RemoveTokens (toSource attrs) (toTarget attrs) Resource 1]
      pure e
    UseCardAbility _ (isSource attrs -> True) 3 _ -> do
      strengthen
      afterAttackOn attrs.id [RemoveTokens (toSource attrs) (toTarget attrs) Resource 1]
      pure e
    _ -> Naasiyah <$> runMessage msg attrs
   where
    strengthen = do
      n <- resourcesHeld attrs (resources attrs)
      attackModifier attrs attrs (AttackModifier n)
      attackModifier attrs attrs (DefenseModifier n)
