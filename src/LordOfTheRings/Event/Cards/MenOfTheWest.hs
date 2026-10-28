module LordOfTheRings.Event.Cards.MenOfTheWest (menOfTheWest) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Trait (Trait (Outlands))

newtype MenOfTheWest = MenOfTheWest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Return X Outlands allies from your discard pile to your hand.
--
-- X is paid in leadership resources as the allies are chosen, 1 for each, so only as many
-- as the player can pay for are offered.
menOfTheWest :: EventCard MenOfTheWest
menOfTheWest =
  eventWith MenOfTheWest Cards.menOfTheWest \a ->
    a
      & criteriaL
      .~ ResourcesOfSphere (PlayerWithId a.owner) Leadership 1
      <> CardInDiscard a.owner outlandsAlly

outlandsAlly :: CardMatcher
outlandsAlly = CardWithType AllyType <> CardWithTrait Outlands

instance RunMessage MenOfTheWest where
  runMessage msg e@(MenOfTheWest attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      doStep 1 msg
      pure e
    -- the first ally chosen, then any more
    DoStep n inner@(PlayEvent pid card _) | card.id == toId attrs -> do
      allies <- filter (cardMatches outlandsAlly) . (.discard) <$> getPlayer pid
      spend <- getResourcesOfSphere pid Leadership
      when (spend > 0) $ chooseOneM pid do
        questionLabeled "menOfTheWest.choose"
        causedBy attrs
        targets [pc | PlayerCard pc <- allies] \pc -> do
          spendResources pid Leadership 1
          returnFromDiscardToHand pid pc.id
          doStep 2 inner
        when (n > 1) $ labeled "menOfTheWest.done" (pure ())
      pure e
    _ -> MenOfTheWest <$> runMessage msg attrs
