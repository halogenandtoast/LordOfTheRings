module LordOfTheRings.Enemy.Cards.CorsairLoyalist (corsairLoyalist) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype CorsairLoyalist = CorsairLoyalist EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairLoyalist :: EnemyCard CorsairLoyalist
corsairLoyalist = enemy CorsairLoyalist Cards.corsairLoyalist

-- | While Corsair Loyalist has 4 or more resources on it, its attacks are considered to be
-- undefended.
instance HasModifiersFor CorsairLoyalist where
  getModifiersFor (CorsairLoyalist a) = when (a.resources >= 4) $ modifySelf a [CannotBeDefended]

-- | Shadow: If attacking enemy is a Raider, either add 2 resources to attacking enemy, or
-- attacking enemy makes an additional attack after this one.
instance RunMessage CorsairLoyalist where
  runMessage msg e@(CorsairLoyalist attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) do
        chooseOneM pid do
          questionLabeled "corsairLoyalist.choose"
          causedBy (CardIdTarget cid)
          labeled "corsairLoyalist.resources" $ placeResources (CardIdSource cid) eid 2
          labeled "corsairLoyalist.attack" $ additionalAttack pid eid
      pure e
    _ -> CorsairLoyalist <$> runMessage msg attrs
