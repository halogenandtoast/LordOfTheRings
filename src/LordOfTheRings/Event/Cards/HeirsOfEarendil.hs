module LordOfTheRings.Event.Cards.HeirsOfEarendil (heirsOfEarendil) where

import LordOfTheRings.Card (cdQuestPoints, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Noldor))

newtype HeirsOfEarendil = HeirsOfEarendil EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

staged :: LocationMatcher
staged = LocationInStagingArea <> not_ UniqueLocation

-- | Play only if you control a unique character with the Noldor trait and another unique
-- character with the Dúnedain trait.
heirsOfEarendil :: EventCard HeirsOfEarendil
heirsOfEarendil =
  eventWith HeirsOfEarendil Cards.heirsOfEarendil \a ->
    a & criteriaL .~ (uniqueCharactersWith a.owner [Noldor, Dunedain] <> exists staged)

-- | Action: Choose a non-unique location in the staging area and raise your threat by X
-- to discard it. X is the chosen location's printed quest points.
instance RunMessage HeirsOfEarendil where
  runMessage msg e@(HeirsOfEarendil attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      locations <- select staged
      chooseOrRunOneM pid do
        questionLabeled "heirsOfEarendil.location"
        causedBy attrs
        targets locations \lid -> do
          x <- fromMaybe 0 . cdQuestPoints . toCardDef <$> getLocation lid
          raiseThreat pid x
          discard lid
      pure e
    _ -> HeirsOfEarendil <$> runMessage msg attrs
