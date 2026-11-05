module LordOfTheRings.Treachery.Cards.TheDarkLordsReach (theDarkLordsReach) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDarkLordsReach = TheDarkLordsReach TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDarkLordsReach :: TreacheryCard TheDarkLordsReach
theDarkLordsReach = treachery TheDarkLordsReach Cards.theDarkLordsReach

{- | When Revealed: Deal 1 damage to each exhausted character. Discard an attachment from
each character damaged by this effect: one still in play, its controller choosing which,
conditions among them, as "discard 1 attachment" may take them ('getAttachmentsOn').
Shadow: Discard an attachment you control.
-}
instance RunMessage TheDarkLordsReach where
  runMessage msg t@(TheDarkLordsReach attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- ordNub . map fst . filter ((> 0) . snd) <$> damageDealtBy attrs
      for_ damaged \c -> characterController c >>= traverse_ \pid -> do
        attached <- getAttachmentsOn c
        chooseOrRunOneM pid do
          questionLabeled "theDarkLordsReach.discard"
          causedBy attrs
          targets attached (discard attrs)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure t
    _ -> TheDarkLordsReach <$> runMessage msg attrs
