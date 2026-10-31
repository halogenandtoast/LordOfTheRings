module LordOfTheRings.Treachery.Cards.SentinelOfShadow (sentinelOfShadow) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SentinelOfShadow = SentinelOfShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sentinelOfShadow :: TreacheryCard SentinelOfShadow
sentinelOfShadow = treachery SentinelOfShadow Cards.sentinelOfShadow

-- | (Cannot be canceled.)
instance HasModifiersFor SentinelOfShadow where
  getModifiersFor (SentinelOfShadow a) = modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: The staging area gets +X threat until the end of the phase, where X is
-- twice the number of players in the game. If the players quest unsuccessfully this phase,
-- put the Nazgul of Dol Guldur into play (from any out of play area), engaged with the first
-- player. The bonus comes from every copy of the card, which Escape from Dol Guldur reads as
-- the players quest unsuccessfully.
instance RunMessage SentinelOfShadow where
  runMessage msg t@(SentinelOfShadow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      phaseModifier (CardCodeSource $ toCardCode attrs) GameTarget (StagingThreatBonus $ 2 * players)
      pure t
    _ -> SentinelOfShadow <$> runMessage msg attrs
