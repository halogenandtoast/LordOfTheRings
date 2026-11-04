module LordOfTheRings.Enemy.Cards.RavenousWarg (ravenousWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype RavenousWarg = RavenousWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenousWarg :: EnemyCard RavenousWarg
ravenousWarg = enemy RavenousWarg Cards.ravenousWarg

-- | Forced: When Ravenous Warg attacks, the defending player must declare the character he
-- controls with the most damage as the defender. (Even if exhausted.)
instance HasAbilities RavenousWarg where
  getAbilities (RavenousWarg a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | The defending player chooses among those tied for the most damage, all of his
-- characters if none is damaged.
-- Shadow: Defending character gets -1 defense for each quest stage in play with no
-- progress on it.
instance RunMessage RavenousWarg where
  runMessage msg e@(RavenousWarg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- windows] \pid -> do
        characters <- charactersControlledBy pid
        damaged <- for characters \c -> (c,) <$> getDamage c
        let most = foldr (max . snd) 0 damaged
        chooseOrRunOneM pid do
          questionLabeled "ravenousWarg.defender"
          causedBy attrs
          targets [c | (c, n) <- damaged, n == most] (declareAsDefender pid attrs.id)
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      n <- stagesWithoutProgress
      when (n > 0) $ for_ defenders \d -> attackModifier (CardIdSource cid) d (DefenseModifier (negate n))
      pure e
    _ -> RavenousWarg <$> runMessage msg attrs
