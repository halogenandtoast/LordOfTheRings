module LordOfTheRings.Event.Cards.RallyingCry (rallyingCry) where

import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype RallyingCry = RallyingCry EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After an ally leaves play, add it to its owner's hand instead of placing it
-- in the discard pile.
-- Valour Action: Until the end of the phase, add each ally that leaves play to its
-- owner's hand instead of placing it in the discard pile.
rallyingCry :: EventCard RallyingCry
rallyingCry =
  eventWith RallyingCry Cards.rallyingCry \a ->
    a
      & windowL ?~ CharacterLeftPlayWindow After (CardWithType AllyType)
      & actionCriteriaL ?~ valour a.owner

-- | Played as the Valour Action, it has no windows.
instance RunMessage RallyingCry where
  runMessage msg e@(RallyingCry attrs) = case msg of
    PlayEvent _ card [] | card.id == toId attrs -> do
      phaseModifier attrs GameTarget AlliesLeavePlayToHand
      pure e
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [c | Window _ (CharacterLeftPlay (PlayerCard c)) <- windows]) \c ->
        for_ c.owner \owner -> returnFromDiscardToHand owner (pcId c)
      pure e
    _ -> RallyingCry <$> runMessage msg attrs
