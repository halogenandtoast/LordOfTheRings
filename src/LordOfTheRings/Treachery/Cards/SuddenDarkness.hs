module LordOfTheRings.Treachery.Cards.SuddenDarkness (suddenDarkness) where

import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (flipToNight, isNight, wargEnemy)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenDarkness = SuddenDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenDarkness :: TreacheryCard SuddenDarkness
suddenDarkness = treachery SuddenDarkness Cards.suddenDarkness

-- | When Revealed: If it is Night, raise each player's threat by 4. If it is Day, flip the
-- Time objective to Nightfall.
-- Shadow: Put the topmost Warg enemy from the discard pile into play engaged with you and
-- deal it a shadow card.
instance RunMessage SuddenDarkness where
  runMessage msg t@(SuddenDarkness attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      night <- isNight
      if night then eachPlayer \p -> encounterRaisesThreat p 4 else flipToNight
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardPile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      for_ (find (cardMatches wargEnemy) discardPile) \c -> do
        eid <- getRandom
        removeFromEncounterDiscard c
        push $ CreateEnemyEngaged pid eid c
        dealShadowCard eid
      pure t
    _ -> SuddenDarkness <$> runMessage msg attrs
