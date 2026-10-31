module LordOfTheRings.Enemy.Cards.SpidersOfMirkwood (spidersOfMirkwood) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SpidersOfMirkwood = SpidersOfMirkwood EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

spidersOfMirkwood :: EnemyCard SpidersOfMirkwood
spidersOfMirkwood = enemy SpidersOfMirkwood Cards.spidersOfMirkwood

-- | While it is engaged with you, Spiders of Mirkwood gets +1 attack for each exhausted
-- character you control.
instance HasModifiersFor SpidersOfMirkwood where
  getModifiersFor (SpidersOfMirkwood a) = for_ [p | EngagedWith p <- [a.placement]] \pid -> do
    n <- length <$> exhaustedCharactersOf pid
    when (n > 0) $ modifySelf a [AttackModifier n]

-- | Shadow: Choose and exhaust 1 character you control. If this attack was undefended, also
-- deal that character 2 damage.
instance RunMessage SpidersOfMirkwood where
  runMessage msg e@(SpidersOfMirkwood attrs) = case msg of
    ResolveShadowEffect cid eid pid defender | cid == attrs.cardId -> do
      readyOnes <- selectCharacters (ReadyHero <> HeroControlledBy (PlayerWithId pid)) (ReadyAlly <> AllyControlledBy (PlayerWithId pid))
      chooseOrRunOneM pid do
        questionLabeled "spidersOfMirkwood.exhaust"
        causedBy attrs
        targets readyOnes \c -> do
          exhaust c
          when (null defender) $ dealDamage (EnemySource eid) c 2
      pure e
    _ -> SpidersOfMirkwood <$> runMessage msg attrs
