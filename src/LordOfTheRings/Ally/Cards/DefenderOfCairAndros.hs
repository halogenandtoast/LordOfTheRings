module LordOfTheRings.Ally.Cards.DefenderOfCairAndros (defenderOfCairAndros) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype DefenderOfCairAndros = DefenderOfCairAndros AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

defenderOfCairAndros :: AllyCard DefenderOfCairAndros
defenderOfCairAndros = ally DefenderOfCairAndros Cards.defenderOfCairAndros

-- | Defender of Cair Andros gets +X defense, where X is the tens digit of your threat.
-- (Limit +4 defense.)
instance HasModifiersFor DefenderOfCairAndros where
  getModifiersFor (DefenderOfCairAndros a) = for_ (placementController a.placement) \pid -> do
    x <- min 4 . (`div` 10) <$> getThreat pid
    when (x > 0) $ modifySelf a [DefenseModifier x]

-- | Valour Response: After Defender of Cair Andros is declared as a defender, deal 1
-- damage to the attacking enemy.
instance HasAbilities DefenderOfCairAndros where
  getAbilities (DefenderOfCairAndros a) =
    [ restricted a 1 (valour pid) $ response $ DefenderDeclaredWindow After (Just $ toTarget a)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage DefenderOfCairAndros where
  runMessage msg s@(DefenderOfCairAndros a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [eid | Window _ (DeclaredAsDefender _ eid) <- windows] \eid -> dealDamage a eid 1
      pure s
    _ -> DefenderOfCairAndros <$> runMessage msg a
