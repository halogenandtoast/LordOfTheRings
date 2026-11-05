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
each shadow card turns faceup ('revealedShadowCards'): +2 for the attack, once for each such card. A
riddle dealt as a shadow card cannot be answered then (rules insert, "Riddles").
-}
instance RunMessage WildWargs where
  runMessage msg (WildWargs attrs) = case msg of
    CheckWindows ws -> do
      for_ [c | (c, _, _) <- revealedShadowCards attrs ws, isJust (cdRiddle $ toCardDef c)] \_ -> enemyGetsAttack attrs attrs.id 2
      WildWargs <$> runMessage msg attrs
    _ -> WildWargs <$> runMessage msg attrs
