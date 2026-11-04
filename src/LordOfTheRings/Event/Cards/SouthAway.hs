module LordOfTheRings.Event.Cards.SouthAway (southAway) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SouthAway = SouthAway EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

stagedWithAttachment :: LocationMatcher
stagedWithAttachment = LocationInStagingArea <> LocationWithPlayerCardAttachment

southAway :: EventCard SouthAway
southAway =
  eventWith SouthAway Cards.southAway \a -> a & criteriaL .~ (DuringPhase TravelPhase <> exists stagedWithAttachment)

-- | Travel Action: Choose a location in the staging area with a player card attachment and
-- make it the active location, returning any previous active location to the staging
-- area. Reduce each player's threat by X, where X is the number of attachments on the
-- active location.
instance RunMessage SouthAway where
  runMessage msg e@(SouthAway attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      locations <- select stagedWithAttachment
      chooseOrRunOneM pid do
        questionLabeled "southAway.location"
        causedBy attrs
        targets locations \lid -> do
          active <- select IsActiveLocation
          for_ active \l -> placeLocation l StagingArea
          makeActive lid
          x <- length <$> getAttachmentsOn (toTarget lid)
          eachPlayer \p -> lowerThreatBy attrs p x
      pure e
    _ -> SouthAway <$> runMessage msg attrs
