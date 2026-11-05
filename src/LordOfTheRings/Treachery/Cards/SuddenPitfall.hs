module LordOfTheRings.Treachery.Cards.SuddenPitfall (suddenPitfall) where

import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenPitfall = SuddenPitfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenPitfall :: TreacheryCard SuddenPitfall
suddenPitfall = treachery SuddenPitfall Cards.suddenPitfall

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor SuddenPitfall where
  getModifiersFor (SuddenPitfall a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | When Revealed: The first player must discard 1 questing character he controls, if
-- able. This effect cannot be canceled.
-- Shadow: Discard the defending character from play.
instance RunMessage SuddenPitfall where
  runMessage msg t@(SuddenPitfall attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      questing <- selectCharacters (HeroControlledBy (PlayerWithId lead) <> CommittedHero) (AllyControlledBy (PlayerWithId lead) <> CommittedAlly)
      chooseOrRunOneM lead do
        questionLabeled "suddenPitfall.discard"
        causedBy attrs
        targets questing (discard attrs)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders (discard attrs)
      pure t
    _ -> SuddenPitfall <$> runMessage msg attrs
