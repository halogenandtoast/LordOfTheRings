module LordOfTheRings.Enemy.Cards.SouthronChampion (southronChampion) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (chooseDamageOrMinasTirith, damageMinasTirith)

newtype SouthronChampion = SouthronChampion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronChampion :: EnemyCard SouthronChampion
southronChampion = enemy SouthronChampion Cards.southronChampion

{- | Forced: After Southron Champion is dealt a shadow card with no shadow effect, either
deal 3 damage to Minas Tirith, or it makes an additional attack after this one. A shadow
card is known to have no effect as it turns face up during its attack, as Lord Alcaron's;
the player it attacks chooses.

Shadow: Deal 1 damage to Minas Tirith.
-}
instance RunMessage SouthronChampion where
  runMessage msg e@(SouthronChampion attrs) = case msg of
    RevealShadowCard eid cid
      | eid == attrs.id
      , Just card <- find ((== cid) . (.id)) attrs.shadowCards
      , cid `notElem` attrs.enemyShadowRevealed
      , not (cdShadowEffect $ toCardDef card)
      , Just pid <- placementController attrs.placement -> do
          chooseDamageOrMinasTirith attrs pid "southronChampion" 3 $ additionalAttack pid attrs.id
          SouthronChampion <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      damageMinasTirith attrs 1
      pure e
    _ -> SouthronChampion <$> runMessage msg attrs
