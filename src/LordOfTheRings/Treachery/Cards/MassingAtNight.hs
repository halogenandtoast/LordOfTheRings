module LordOfTheRings.Treachery.Cards.MassingAtNight (massingAtNight) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MassingAtNight = MassingAtNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

massingAtNight :: TreacheryCard MassingAtNight
massingAtNight = treachery MassingAtNight Cards.massingAtNight

-- | When Revealed: Reveal X additional cards from the encounter deck. X is the number of
-- players in the game.
-- Shadow: Deal X shadow cards to this attacker. X is the number of players in the game.
instance RunMessage MassingAtNight where
  runMessage msg t@(MassingAtNight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getPlayerOrder
      revealEncounterCards players
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      players <- length <$> getPlayerOrder
      replicateM_ players $ dealShadowCard eid
      pure t
    _ -> MassingAtNight <$> runMessage msg attrs
