module LordOfTheRings.Enemy.Cards.BlackForestBatsRhosgobel (blackForestBatsRhosgobel) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Trait (Trait (Eagle))

newtype BlackForestBatsRhosgobel = BlackForestBatsRhosgobel EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackForestBatsRhosgobel :: EnemyCard BlackForestBatsRhosgobel
blackForestBatsRhosgobel = enemy BlackForestBatsRhosgobel Cards.blackForestBatsRhosgobel

-- | Only Eagle characters or characters with ranged can attack or defend against Black
-- Forest Bats.
instance HasModifiersFor BlackForestBatsRhosgobel where
  getModifiersFor (BlackForestBatsRhosgobel a) = modified a a [OnlyFoughtBy Eagle Ranged]

-- | Shadow: If this attack is undefended, the damage must be placed on Wilyador.
instance RunMessage BlackForestBatsRhosgobel where
  runMessage msg e@(BlackForestBatsRhosgobel attrs) = case msg of
    ResolveShadowEffect cid eid _ Nothing | cid == attrs.cardId -> do
      wilyador <- select (AllyWithCardCode $ toCardCode Objectives.wilyador)
      for_ wilyador $ undefendedDamageTo attrs eid
      pure e
    _ -> BlackForestBatsRhosgobel <$> runMessage msg attrs
