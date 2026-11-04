module LordOfTheRings.Enemy.Cards.HaradrimArcher (haradrimArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))

newtype HaradrimArcher = HaradrimArcher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimArcher :: EnemyCard HaradrimArcher
haradrimArcher = enemy HaradrimArcher Cards.haradrimArcher

-- | Threat X. Archery X. X is the number of players in the game.
instance HasModifiersFor HaradrimArcher where
  getModifiersFor (HaradrimArcher a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier players, GainKeyword (Archery players)]

-- | Forced: After Haradrim Archer engages you, assign X damage among characters you
-- control.
instance HasAbilities HaradrimArcher where
  getAbilities (HaradrimArcher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage HaradrimArcher where
  runMessage msg e@(HaradrimArcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      players <- length <$> getRemainingPlayers
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageDivided attrs pid players
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> HaradrimArcher <$> runMessage msg attrs
