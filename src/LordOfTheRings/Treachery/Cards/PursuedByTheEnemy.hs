module LordOfTheRings.Treachery.Cards.PursuedByTheEnemy (pursuedByTheEnemy) where

import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PursuedByTheEnemy = PursuedByTheEnemy TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of Journey in the Dark's burden set. Doomed 1. Surge.
pursuedByTheEnemy :: TreacheryCard PursuedByTheEnemy
pursuedByTheEnemy = treachery PursuedByTheEnemy Cards.pursuedByTheEnemy

{- | When Revealed: Each enemy engaged with a player, and not in the staging area, makes an
immediate attack: against the player it is engaged with.
Shadow: Attacking enemy makes an additional attack after this one.
-}
instance RunMessage PursuedByTheEnemy where
  runMessage msg t@(PursuedByTheEnemy attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse getEnemy =<< select (EnemyEngagedWith AnyPlayer)
      for_ [(pid, e.id) | e <- enemies, EngagedWith pid <- [e.placement]] $ uncurry immediateAttack
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> PursuedByTheEnemy <$> runMessage msg attrs
