module LordOfTheRings.Treachery.Cards.ParalyzingFear (paralyzingFear) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ParalyzingFear = ParalyzingFear TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

paralyzingFear :: TreacheryCard ParalyzingFear
paralyzingFear = treachery ParalyzingFear Cards.paralyzingFear

-- | Counts as a Condition attachment with the text: "Forced: When attached hero would
-- ready, it does not ready unless attached hero's controller discards a card from the top
-- of his deck."
-- The hero cannot ready, but while the resource the discard puts on this card for its one
-- Ready is there.
-- The resource on it marks the paid Ready: treachery attrs hold no state of their own,
-- and the token is gone again before anyone is asked anything.
instance HasModifiersFor ParalyzingFear where
  getModifiersFor (ParalyzingFear a) =
    when (paid a == 0) $ withHost a \host -> modified a host [CannotReady]

paid :: TreacheryAttrs -> Int
paid = countTokens Resource . treacheryTokens

-- | Surge. When Revealed: Attach to a hero and exhaust that hero. The first player chooses.
instance RunMessage ParalyzingFear where
  runMessage msg t@(ParalyzingFear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- select AnyHero
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "paralyzingFear.attach"
        causedBy attrs
        targets heroes \h -> do
          attachTreachery attrs.treacheryId h
          exhaust h
      pure t
    Ready target | AttachedTo host@(HeroTarget hid) <- attrs.placement, target == host, paid attrs == 0 -> do
      exhausted <- selectAny (HeroWithId hid <> ExhaustedHero)
      controller <- characterController host
      for_ controller \pid -> do
        deck <- (.deck) <$> getPlayer pid
        when (exhausted && notNull deck) $ chooseOneM pid do
          questionLabeled "paralyzingFear.ready"
          causedBy attrs
          labeled "paralyzingFear.discard" do
            discardTopOfDeck pid 1
            placeResources attrs attrs 1
            encounterReadies host
            removeTokens attrs attrs Resource 1
          labeled "paralyzingFear.stayExhausted" $ pure ()
      ParalyzingFear <$> runMessage msg attrs
    _ -> ParalyzingFear <$> runMessage msg attrs
