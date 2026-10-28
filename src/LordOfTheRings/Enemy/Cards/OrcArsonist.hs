module LordOfTheRings.Enemy.Cards.OrcArsonist (orcArsonist) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype OrcArsonist = OrcArsonist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcArsonist :: EnemyCard OrcArsonist
orcArsonist = enemy OrcArsonist Cards.orcArsonist

-- | Forced: When Orc Arsonist engages a player, deal 1 shadow card to each enemy engaged
-- with that player.
instance HasAbilities OrcArsonist where
  getAbilities (OrcArsonist a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: attacking enemy gets +1 attack. Deal it another shadow card.
instance RunMessage OrcArsonist where
  runMessage msg e@(OrcArsonist attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      traverse_ dealShadowCard =<< select (EnemyEngagedWith $ PlayerWithId pid)
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      dealShadowCardsDuringAttack eid pid defenders 1
      pure e
    _ -> OrcArsonist <$> runMessage msg attrs
