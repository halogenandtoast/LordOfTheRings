module LordOfTheRings.Enemy.Cards.WhiteWarg (whiteWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)

newtype WhiteWarg = WhiteWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

whiteWarg :: EnemyCard WhiteWarg
whiteWarg = enemy WhiteWarg Cards.whiteWarg

-- | Forced: After White Warg engages you, deal 1 damage to a character you control (2
-- damage instead if it is Night).
instance HasAbilities WhiteWarg where
  getAbilities (WhiteWarg a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If it is Night, attacking enemy makes an additional attack after this one.
instance RunMessage WhiteWarg where
  runMessage msg e@(WhiteWarg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      n <- bool 1 2 <$> isNight
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneCharacter attrs pid n
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM isNight $ additionalAttack pid eid
      pure e
    _ -> WhiteWarg <$> runMessage msg attrs
