module LordOfTheRings.Treachery.Cards.OverrunByOrcs (overrunByOrcs) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OverrunByOrcs = OverrunByOrcs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overrunByOrcs :: TreacheryCard OverrunByOrcs
overrunByOrcs = treachery OverrunByOrcs Cards.overrunByOrcs

-- | When Revealed: Until the end of the phase, increase the total threat in the staging
-- area by X, where X is twice the stage number of the main quest.
-- Shadow: Assign X damage among characters you control, where X is the stage number of
-- the main quest.
instance RunMessage OverrunByOrcs where
  runMessage msg t@(OverrunByOrcs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- getMainStage
      phaseModifier attrs GameTarget (StagingThreatBonus $ 2 * x)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      x <- getMainStage
      dealDamageDivided (CardIdSource cid) pid x
      pure t
    _ -> OverrunByOrcs <$> runMessage msg attrs
