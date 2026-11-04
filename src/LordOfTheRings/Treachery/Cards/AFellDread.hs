module LordOfTheRings.Treachery.Cards.AFellDread (aFellDread) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AFellDread = AFellDread TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFellDread :: TreacheryCard AFellDread
aFellDread = treachery AFellDread Cards.aFellDread

{- | Counts as a Condition attachment with the text: "Attached hero's willpower is reduced
to 0. During the quest phase, attached hero must commit to the quest, if able."
-}
instance HasModifiersFor AFellDread where
  getModifiersFor (AFellDread a) = withHost a \host -> modified a host [WillpowerReducedToZero, MustCommitToQuest]

-- | When Revealed: The player with the highest threat must attach A Fell Dread to a hero
-- he controls without A Fell Dread attached. With a tie the first player chooses which of
-- the tied players.
instance RunMessage AFellDread where
  runMessage msg t@(AFellDread attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      threats <- traverse (\p -> (p,) . (.threat) <$> getPlayer p) =<< getRemainingPlayers
      let highest = foldr (max . snd) 0 threats
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "aFellDread.choosePlayer"
        causedBy attrs
        targets [p | (p, n) <- threats, n == highest] \p -> do
          heroes <- select (HeroControlledBy $ PlayerWithId p)
          free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) heroes
          chooseOrRunOneM p do
            questionLabeled "aFellDread.attach"
            causedBy attrs
            targets free \h -> attachTreachery attrs.treacheryId h
      pure t
    _ -> AFellDread <$> runMessage msg attrs
