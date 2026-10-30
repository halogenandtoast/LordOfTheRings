module LordOfTheRings.Treachery.Cards.SuddenAssault (suddenAssault) where

import LordOfTheRings.Helpers.Character (getCommittedWillpower)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenAssault = SuddenAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenAssault :: TreacheryCard SuddenAssault
suddenAssault = treachery SuddenAssault Cards.suddenAssault

-- | When Revealed: If the total willpower of characters committed to the quest is greater
-- than the total threat in the staging area, each enemy in the staging area makes an
-- immediate attack against the first player. If no attack is made this way, shuffle Sudden
-- Assault and the encounter discard pile into the encounter deck.
instance RunMessage SuddenAssault where
  runMessage msg t@(SuddenAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      willpower <- getCommittedWillpower
      threat <- getStagingThreat
      enemies <- if willpower > threat then select EnemyInStagingArea else pure []
      lead <- getLead
      for_ enemies (immediateAttack lead)
      when (null enemies) do
        shuffleEncounterDiscardIntoDeck AnyCard
        shuffleIntoEncounterDeck attrs
      pure t
    _ -> SuddenAssault <$> runMessage msg attrs
