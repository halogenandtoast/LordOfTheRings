module LordOfTheRings.Ally.Cards.GiantBear (giantBear) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GiantBear = GiantBear AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantBear :: AllyCard GiantBear
giantBear = ally GiantBear Cards.giantBear

-- | Cannot have attachments.
instance HasModifiersFor GiantBear where
  getModifiersFor (GiantBear a) = modifySelf a [CannotHaveAttachments]

-- | Action: Ready Giant Bear. At the end of the phase in which you trigger this effect,
-- shuffle Giant Bear back into your deck. (Limit once per phase.)
instance HasAbilities GiantBear where
  getAbilities (GiantBear a) =
    [limited (PlayerLimit PerPhase 1) $ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly) $ ActionAbility Free]

instance RunMessage GiantBear where
  runMessage msg s@(GiantBear a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      phaseModifier a a ShuffleIntoDeckAtEndOfPhase
      pure s
    _ -> GiantBear <$> runMessage msg a
