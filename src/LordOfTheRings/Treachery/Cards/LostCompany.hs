module LordOfTheRings.Treachery.Cards.LostCompany (lostCompany) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LostCompany = LostCompany TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostCompany :: TreacheryCard LostCompany
lostCompany = treachery LostCompany Cards.lostCompany

-- | When Revealed: Each player removes 1 character he controls from the quest, if able.
-- Then, if any player has no characters committed to the quest, remove all characters
-- from the quest.
instance RunMessage LostCompany where
  runMessage msg t@(LostCompany attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- questingOf pid
        chooseOrRunOneM pid do
          questionLabeled "lostCompany.remove"
          causedBy attrs
          targets questing removeFromQuest
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      someoneAlone <- anyM (fmap null . questingOf) players
      when someoneAlone $ traverse_ removeFromQuest =<< selectCharacters CommittedHero CommittedAlly
      pure t
    _ -> LostCompany <$> runMessage msg attrs

questingOf :: HasGame m => PlayerId -> m [Target]
questingOf pid =
  selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId pid)) (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
