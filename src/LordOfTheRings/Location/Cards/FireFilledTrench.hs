module LordOfTheRings.Location.Cards.FireFilledTrench (fireFilledTrench) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasQueue (peekQueue)
import LordOfTheRings.Event (Event, EventAttrs (..), lookupEvent)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FireFilledTrench = FireFilledTrench LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fireFilledTrench :: LocationCard FireFilledTrench
fireFilledTrench = location FireFilledTrench Cards.fireFilledTrench

-- | Forced: When a player plays an event that would cancel the effects of a treachery card
-- just revealed from the encounter deck, cancel the effects of that event. Then, discard
-- Fire-filled Trench.
instance HasAbilities FireFilledTrench where
  getAbilities (FireFilledTrench a) = [mkAbility a 1 $ forced $ EventPlayedWindow When]

{- | The event's play waits on the game's queue with the windows it answers: one played as
a treachery is revealed, whose play needs an encounter card's effects to be cancelable, as
A Test of Will's, would cancel them. Another event played then, the Forced effect does
nothing.
-}
instance RunMessage FireFilledTrench where
  runMessage msg l@(FireFilledTrench a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      queued <- lift peekQueue
      let cancels = [card.id | PlayEvent pid card pws <- queued, any (\case Window _ (Revealed (TreacheryTarget _)) -> True; _ -> False) pws, cancelsEncounterEffects (lookupEvent (toCardCode card) card.id pid)]
      for_ [c | Window _ (PlayingEvent _ c) <- ws, c `elem` cancels] \c -> do
        cancelEvent c
        discard a a
      pure l
    _ -> FireFilledTrench <$> runMessage msg a

-- | Whether the event's play needs an encounter card's effects to be cancelable.
cancelsEncounterEffects :: Maybe Event -> Bool
cancelsEncounterEffects = maybe False (mentions . eventCriteria . toAttrs)
 where
  mentions = \case
    EncounterEffectsCancelable -> True
    Criteria cs -> any mentions cs
    _ -> False
