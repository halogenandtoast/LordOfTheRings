module LordOfTheRings.Treachery.Cards.BlastOfSavageWind (blastOfSavageWind) where

import LordOfTheRings.Trait (Trait (EmynMuil))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlastOfSavageWind = BlastOfSavageWind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blastOfSavageWind :: TreacheryCard BlastOfSavageWind
blastOfSavageWind = treachery BlastOfSavageWind Cards.blastOfSavageWind

-- | Doomed 2.
-- When Revealed: Remove all progress from each Emyn Muil location in play. Each Emyn Muil
-- location gets +2 threat until the end of the round.
-- Shadow: Remove all progress from each Emyn Muil location in play.
instance RunMessage BlastOfSavageWind where
  runMessage msg t@(BlastOfSavageWind attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- clear
      for_ locations \l -> roundModifier attrs l (ThreatModifier 2)
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      _ <- clear
      pure t
    _ -> BlastOfSavageWind <$> runMessage msg attrs
   where
    clear = do
      locations <- select (LocationWithTrait EmynMuil)
      for_ locations \lid -> do
        l <- getLocation lid
        when (l.progress > 0) $ removeTokens attrs lid Progress l.progress
      pure locations
