module LordOfTheRings.Enemy.Cards.CorsairPyromaniac (corsairPyromaniac) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype CorsairPyromaniac = CorsairPyromaniac EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairPyromaniac :: EnemyCard CorsairPyromaniac
corsairPyromaniac = enemy CorsairPyromaniac Cards.corsairPyromaniac

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | Corsair Pyromaniac gets +1 threat, +1 attack, and +1 defense for each resource token on
-- it.
instance HasModifiersFor CorsairPyromaniac where
  getModifiersFor (CorsairPyromaniac a) = do
    let n = resources a
    when (n > 0) $ modifySelf a [ThreatModifier n, AttackModifier n, DefenseModifier n]

-- | Forced: After a card is placed underneath The Havens Burn, place 1 resource on Corsair
-- Pyromaniac.
instance RunMessage CorsairPyromaniac where
  runMessage msg (CorsairPyromaniac attrs) = case msg of
    PlaceUnderneath (ObjectiveTarget oid) cards -> do
      o <- getObjective oid
      when (toCardCode o == toCardCode Objectives.theHavensBurn) $ placeResources attrs attrs (length cards)
      CorsairPyromaniac <$> runMessage msg attrs
    _ -> CorsairPyromaniac <$> runMessage msg attrs
