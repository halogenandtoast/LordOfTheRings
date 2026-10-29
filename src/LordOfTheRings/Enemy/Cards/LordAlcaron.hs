module LordOfTheRings.Enemy.Cards.LordAlcaron (lordAlcaron) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)

newtype LordAlcaron = LordAlcaron EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lordAlcaron :: EnemyCard LordAlcaron
lordAlcaron = enemy LordAlcaron Cards.lordAlcaron

-- | Forced: When Lord Alcaron attacks, place 1 progress token on To the Tower.
instance HasAbilities LordAlcaron where
  getAbilities (LordAlcaron a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Forced: When Lord Alcaron is dealt a shadow card with no shadow effect, he makes an
-- additional attack after this one (deal and resolve another shadow card). A shadow card
-- is known to have no effect as it turns face up during his attack.
instance RunMessage LordAlcaron where
  runMessage msg e@(LordAlcaron attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeOnTower attrs 1
      pure e
    RevealShadowCard eid cid
      | eid == attrs.id
      , Just card <- find ((== cid) . (.id)) attrs.shadowCards
      , cid `notElem` attrs.enemyShadowRevealed
      , not (cdShadowEffect $ toCardDef card)
      , Just pid <- placementController attrs.placement -> do
          additionalAttack pid attrs.id
          LordAlcaron <$> runMessage msg attrs
    _ -> LordAlcaron <$> runMessage msg attrs
