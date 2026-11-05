module LordOfTheRings.Ally.Cards.Derufin (derufin) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameAttackDestroyed)

newtype Derufin = Derufin AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
derufin :: AllyCard Derufin
derufin = ally Derufin Cards.derufin

-- | Response: After Derufin participates in an attack that destroys an enemy engaged with
-- a player, discard Derufin to reduce the engaged player's threat by X, where X is the
-- destroyed enemy's printed threat.
instance HasAbilities Derufin where
  getAbilities (Derufin a) = [mkAbility a 1 $ response $ EngagedEnemyDestroyedByAttackWindow After (toTarget a)]

instance RunMessage Derufin where
  runMessage msg s@(Derufin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      -- the enemy is gone: its card is read from what the attack destroyed
      destroyed <- getsGame gameAttackDestroyed
      let printedThreat d = fromMaybe 0 (cdThreat (toCardDef d.card))
          xs =
            [ printedThreat d
            | Window _ (EnemyDestroyedByAttack eid _ _) <- windows
            , d <- take 1 [d | d <- destroyed, d.target == EnemyTarget eid]
            ]
      discard a a
      for_ (take 1 [p | Window _ (EngagedEnemyDestroyedByAttack p _) <- windows]) \engaged ->
        lowerThreatBy a engaged (sum $ take 1 xs)
      pure s
    _ -> Derufin <$> runMessage msg a
