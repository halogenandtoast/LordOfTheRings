module LordOfTheRings.Treachery.Cards.StrangeWeariness (strangeWeariness) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StrangeWeariness = StrangeWeariness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

strangeWeariness :: TreacheryCard StrangeWeariness
strangeWeariness = treachery StrangeWeariness Cards.strangeWeariness

{- | When Revealed: Each character committed to the quest gets -1 willpower until the end of
the round.
Shadow: Attacking enemy makes an additional attack against you after this one.
-}
instance RunMessage StrangeWeariness where
  runMessage msg t@(StrangeWeariness attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      for_ committed \c -> roundModifier attrs c (WillpowerModifier (-1))
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> StrangeWeariness <$> runMessage msg attrs
