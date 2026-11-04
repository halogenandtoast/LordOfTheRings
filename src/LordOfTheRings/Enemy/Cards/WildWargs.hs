module LordOfTheRings.Enemy.Cards.WildWargs (wildWargs) where

import LordOfTheRings.Card (cdRiddle, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WildWargs = WildWargs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildWargs :: EnemyCard WildWargs
wildWargs = enemy WildWargs Cards.wildWargs

{- | Forced: If Wild Wargs is dealt a shadow card with a riddle, it gets +2 attack. Read as
its shadow cards turn up, as Warg-rider's: +2 for the attack, once for each such card. A
riddle dealt as a shadow card cannot be answered then (rules insert, "Riddles").
-}
instance RunMessage WildWargs where
  runMessage msg (WildWargs attrs) = case msg of
    ResolveShadow eid _ _ | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      for_ (filter (isJust . cdRiddle . toCardDef) faceDown) \_ -> enemyGetsAttack attrs attrs.id 2
      WildWargs <$> runMessage msg attrs
    _ -> WildWargs <$> runMessage msg attrs
