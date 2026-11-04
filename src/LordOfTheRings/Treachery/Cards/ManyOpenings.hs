module LordOfTheRings.Treachery.Cards.ManyOpenings (manyOpenings) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ManyOpenings = ManyOpenings TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

manyOpenings :: TreacheryCard ManyOpenings
manyOpenings = treachery ManyOpenings Cards.manyOpenings

-- | When Revealed: Each location in the staging area gets +1 threat until the end of the
-- round. If the number of locations in the staging area is less than the number of players
-- in the game, reveal an additional encounter card.
-- Shadow: Attacking enemy gets +X attack, where X is the active location's threat.
instance RunMessage ManyOpenings where
  runMessage msg t@(ManyOpenings attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- select LocationInStagingArea
      for_ locations \l -> roundModifier attrs l (ThreatModifier 1)
      players <- length <$> getRemainingPlayers
      when (length locations < players) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure t
    _ -> ManyOpenings <$> runMessage msg attrs
