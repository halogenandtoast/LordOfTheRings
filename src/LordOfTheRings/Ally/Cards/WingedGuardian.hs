module LordOfTheRings.Ally.Cards.WingedGuardian (wingedGuardian) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype WingedGuardian = WingedGuardian AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wingedGuardian :: AllyCard WingedGuardian
wingedGuardian = ally WingedGuardian Cards.wingedGuardian

-- | Sentinel. Winged Guardian cannot have restricted attachments.
-- Forced: After an attack in which Winged Guardian defends resolves, pay 1 Tactics
-- resource or discard Winged Guardian from play.
--
-- ponytail: no restricted attachment can reach an ally yet, so that line needs nothing.
instance HasAbilities WingedGuardian where
  getAbilities (WingedGuardian a) = [mkAbility a 1 $ forced $ defendedAttack a]

instance RunMessage WingedGuardian where
  runMessage msg s@(WingedGuardian a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      payOrDiscard pid a Tactics
      pure s
    _ -> WingedGuardian <$> runMessage msg a
