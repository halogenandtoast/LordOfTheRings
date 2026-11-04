module LordOfTheRings.Ally.Cards.HamaTheTreasonOfSaruman (hamaTheTreasonOfSaruman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype HamaTheTreasonOfSaruman = HamaTheTreasonOfSaruman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hamaTheTreasonOfSaruman :: AllyCard HamaTheTreasonOfSaruman
hamaTheTreasonOfSaruman = ally HamaTheTreasonOfSaruman Cards.hamaTheTreasonOfSaruman

-- | Combat Action: Ready Háma. He gets +3 [defense] until the end of the phase. At the end
-- of the phase, discard Háma. (Limit once per round.)
instance HasAbilities HamaTheTreasonOfSaruman where
  getAbilities (HamaTheTreasonOfSaruman a) =
    [limited (PlayerLimit PerRound 1) $ restricted a 1 (DuringPhase CombatPhase) $ ActionAbility Free]

instance RunMessage HamaTheTreasonOfSaruman where
  runMessage msg s@(HamaTheTreasonOfSaruman a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      phaseModifiers a a [DefenseModifier 3, DiscardAtEndOfPhase]
      pure s
    _ -> HamaTheTreasonOfSaruman <$> runMessage msg a
