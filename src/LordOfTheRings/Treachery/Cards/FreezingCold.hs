module LordOfTheRings.Treachery.Cards.FreezingCold (freezingCold) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FreezingCold = FreezingCold TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

freezingCold :: TreacheryCard FreezingCold
freezingCold = treachery FreezingCold Cards.freezingCold

-- | Counts as a Condition attachment with the text: "Attached hero gets -2 [Willpower] and
-- cannot commit to a quest. If attached hero has more than 1 copy of Freezing Cold
-- attached, discard attached hero from play."
instance HasModifiersFor FreezingCold where
  getModifiersFor (FreezingCold a) = withHost a \host -> modified a host [WillpowerModifier (-2), CannotCommitToQuest]

-- | When Revealed: The first player attaches this card to a hero he controls.
instance RunMessage FreezingCold where
  runMessage msg t@(FreezingCold attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      chooseOrRunOneM lead do
        questionLabeled "freezingCold.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    -- the copies already there, with this one, are more than 1
    AttachTreachery tid host | tid == attrs.treacheryId -> do
      others <- treacheriesOn attrs.treacheryCardCode host
      unless (null others) $ discard attrs host
      FreezingCold <$> runMessage msg attrs
    _ -> FreezingCold <$> runMessage msg attrs
