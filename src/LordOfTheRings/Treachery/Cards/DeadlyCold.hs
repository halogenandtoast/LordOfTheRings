module LordOfTheRings.Treachery.Cards.DeadlyCold (deadlyCold) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeadlyCold = DeadlyCold TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadlyCold :: TreacheryCard DeadlyCold
deadlyCold = treachery DeadlyCold Cards.deadlyCold

{- | When Revealed: For each ready character a player controls, he must choose: either
exhaust that character, or deal 1 damage to it.
Shadow: Attacking enemy gets +1 attack (+2 attack instead if defending character is
damaged).
-}
instance RunMessage DeadlyCold where
  runMessage msg t@(DeadlyCold attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        let you = PlayerWithId p
        ready' <- selectCharacters (HeroControlledBy you <> ReadyHero) (AllyControlledBy you <> ReadyAlly)
        for_ ready' \c -> chooseOneM p do
          questionLabeled "deadlyCold.choose"
          causedBy attrs
          aimedAt [c]
          labeled "deadlyCold.exhaust" $ exhaust c
          labeled "deadlyCold.damage" $ dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      enemyGetsAttack (CardIdSource cid) eid (if damaged then 2 else 1)
      pure t
    _ -> DeadlyCold <$> runMessage msg attrs
