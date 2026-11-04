module LordOfTheRings.Event.Cards.Backtrack (backtrack) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype Backtrack = Backtrack EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

reachable :: LocationMatcher
reachable = LocationInStagingArea <> LocationNotImmuneToPlayerCards

-- | Play only if the main quest has no keywords.
backtrack :: EventCard Backtrack
backtrack = eventWith Backtrack Cards.backtrack (criteriaL .~ (MainQuestWithoutKeywords <> exists reachable))

-- | Action: Remove X progress from the main quest to place X progress on a location in the
-- staging area.
instance RunMessage Backtrack where
  runMessage msg e@(Backtrack attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      progress <- getMainQuestProgress
      locations <- select reachable
      chooseOrRunOneM pid do
        questionLabeled "chooseLocation"
        causedBy attrs
        targets locations \l -> chooseOneM pid do
          questionLabeled "chooseX"
          causedBy attrs
          for_ [1 .. progress] \x -> labeledWith "xEquals" [intVar "x" x] do
            removeProgressFromMainQuest attrs x
            placeProgress attrs l x
      pure e
    _ -> Backtrack <$> runMessage msg attrs
