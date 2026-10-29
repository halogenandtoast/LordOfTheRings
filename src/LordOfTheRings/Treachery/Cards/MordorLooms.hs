module LordOfTheRings.Treachery.Cards.MordorLooms (mordorLooms) where

import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard, turnHiddenCardsFaceup)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MordorLooms = MordorLooms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mordorLooms :: TreacheryCard MordorLooms
mordorLooms = treachery MordorLooms Cards.mordorLooms

-- | Surge.
-- When Revealed: Each player takes 1 hidden card.
-- Shadow: If this attack destroys a character, that character's controller turns each of
-- his hidden cards faceup.
instance RunMessage MordorLooms where
  runMessage msg t@(MordorLooms attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer takeHiddenCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      controllers <- ordNub . mapMaybe (.controller) <$> attackDestroyed eid
      for_ controllers turnHiddenCardsFaceup
      pure t
    _ -> MordorLooms <$> runMessage msg attrs
