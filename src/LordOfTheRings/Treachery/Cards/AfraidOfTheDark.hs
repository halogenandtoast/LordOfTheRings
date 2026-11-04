module LordOfTheRings.Treachery.Cards.AfraidOfTheDark (afraidOfTheDark) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Dark))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AfraidOfTheDark = AfraidOfTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

afraidOfTheDark :: TreacheryCard AfraidOfTheDark
afraidOfTheDark = treachery AfraidOfTheDark Cards.afraidOfTheDark

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. While the active
-- location has the Dark trait, reduce attached hero's willpower to 0 and treat its text box
-- as if it were blank, except for Traits."
instance HasModifiersFor AfraidOfTheDark where
  getModifiersFor (AfraidOfTheDark a) = case a.placement of
    AttachedTo host -> do
      dark <- selectAny (IsActiveLocation <> LocationWithTrait Dark)
      when dark $ modified a host [WillpowerReducedToZero, Blank]
    _ -> pure ()

-- | When Revealed: Attach to a questing hero and remove it from the quest, the first player
-- choosing which: one without a copy attached already (limit 1 per hero).
instance RunMessage AfraidOfTheDark where
  runMessage msg t@(AfraidOfTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      questing <- map toTarget <$> select CommittedHero
      eligible <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode) questing
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "afraidOfTheDark.attach"
        causedBy attrs
        targets eligible \h -> do
          attachTreachery attrs.treacheryId h
          removeFromQuest h
      pure t
    _ -> AfraidOfTheDark <$> runMessage msg attrs
