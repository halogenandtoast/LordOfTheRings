module LordOfTheRings.Event.Cards.BulwarkOfTheWest (bulwarkOfTheWest) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype BulwarkOfTheWest = BulwarkOfTheWest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Playable with an ally to discard and a Condition attachment to choose.
bulwarkOfTheWest :: EventCard BulwarkOfTheWest
bulwarkOfTheWest =
  eventWith BulwarkOfTheWest Cards.bulwarkOfTheWest \a -> a & criteriaL .~ (exists (AllyControlledBy $ PlayerWithId a.owner) <> ConditionAttachmentExists)

-- | Action: Discard an ally you control to choose and discard a Condition attachment.
instance RunMessage BulwarkOfTheWest where
  runMessage msg e@(BulwarkOfTheWest attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      allies <- select (AllyControlledBy $ PlayerWithId pid)
      conditions <- getConditionAttachments
      chooseOrRunOneM pid do
        questionLabeled "bulwarkOfTheWest.ally"
        causedBy attrs
        targets allies \al -> do
          discard attrs al
          chooseOrRunOneM pid do
            questionLabeled "bulwarkOfTheWest.condition"
            causedBy attrs
            targets conditions (discard attrs)
      pure e
    _ -> BulwarkOfTheWest <$> runMessage msg attrs
