module LordOfTheRings.Treachery.Cards.HowlingOfWargs (howlingOfWargs) where

import LordOfTheRings.Card (CardType (EncounterSideQuestType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HowlingOfWargs = HowlingOfWargs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

howlingOfWargs :: TreacheryCard HowlingOfWargs
howlingOfWargs = treachery HowlingOfWargs Cards.howlingOfWargs

-- | When Revealed: Either deal 1 shadow card to each enemy in the staging area, or search
-- the encounter deck and discard pile for a side quest and add it to the staging area.
-- Shuffle the encounter deck if it is searched. The first player chooses.
-- Shadow: Deal 1 shadow card to each enemy engaged with you that has not attacked this
-- phase.
instance RunMessage HowlingOfWargs where
  runMessage msg t@(HowlingOfWargs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "howlingOfWargs.choose"
        causedBy attrs
        labeled "howlingOfWargs.shadow" $ traverse_ dealShadowCard =<< select EnemyInStagingArea
        labeled "howlingOfWargs.search" $ searchEncounterDeckAndDiscardToReveal lead attrs (CardWithType EncounterSideQuestType)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      engaged <- select (EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithId eid))
      traverse_ dealShadowCard engaged
      pure t
    _ -> HowlingOfWargs <$> runMessage msg attrs
