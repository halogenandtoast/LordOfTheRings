module LordOfTheRings.Enemy.Cards.OrcAmbusher (orcAmbusher) where

import LordOfTheRings.Card (CardType (ObjectiveAllyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype OrcAmbusher = OrcAmbusher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcAmbusher :: EnemyCard OrcAmbusher
orcAmbusher = enemy OrcAmbusher Cards.orcAmbusher

-- | Forced: After Orc Ambusher engages you, discard 1 non-objective ally you control, if
-- able.
instance HasAbilities OrcAmbusher where
  getAbilities (OrcAmbusher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if undefended.)
instance RunMessage OrcAmbusher where
  runMessage msg e@(OrcAmbusher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      allies <-
        filterM (fmap ((/= ObjectiveAllyType) . cdCardType . toCardDef) . getAlly)
          =<< select (AllyControlledBy $ PlayerWithId pid)
      unless (null allies) $ chooseAndDiscard attrs pid (map toTarget allies)
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defenders 3 1)
      pure e
    _ -> OrcAmbusher <$> runMessage msg attrs
