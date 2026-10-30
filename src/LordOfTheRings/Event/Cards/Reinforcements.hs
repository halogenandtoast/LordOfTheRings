module LordOfTheRings.Event.Cards.Reinforcements (reinforcements) where

import LordOfTheRings.Card (Card (PlayerCard), PlayerCard, cdCardType, cdName, cdUnique, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype Reinforcements = Reinforcements EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | You must spend resources from 3 different heroes' resource pools to play this card, on
-- its card def. Action: The players, as a group, can put up to 2 allies into play from
-- their hands. Each of these allies may enter play under any player's control. At the end
-- of the phase, return each of those allies that are still in play to their owners'
-- hands.
reinforcements :: EventCard Reinforcements
reinforcements = event Reinforcements Cards.reinforcements

instance RunMessage Reinforcements where
  runMessage msg e@(Reinforcements attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      hands <- concatMap (.hand) <$> (traverse getPlayer =<< getRemainingPlayers)
      allies <- filterM (passesUnique . PlayerCard) [pc | PlayerCard pc <- hands, cdCardType (toCardDef pc) == AllyType]
      let sameUnique a b = cdUnique (toCardDef a) && cdName (toCardDef a) == cdName (toCardDef b)
          putUpTo :: ReverseQueue n => Int -> [PlayerCard] -> n ()
          putUpTo n options = when (n > 0 && notNull options) $ chooseOneM pid do
            questionLabeledWith "reinforcements.choose" [intVar "count" n]
            causedBy attrs
            targets options \pc -> do
              phaseModifier attrs (CardIdTarget pc.id) ReturnToHandAtEndOfPhase
              choosePlayerM pid attrs \p -> putAllyIntoPlayUnder p pc
              putUpTo (n - 1) [o | o <- options, o.id /= pc.id, not (sameUnique o pc)]
            labeled "done" (pure ())
      putUpTo 2 allies
      pure e
    _ -> Reinforcements <$> runMessage msg attrs
