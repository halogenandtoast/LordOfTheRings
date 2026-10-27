module LordOfTheRings.Treachery.Cards.OrcAmbush (orcAmbush) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Orc))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcAmbush = OrcAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcAmbush :: TreacheryCard OrcAmbush
orcAmbush = treachery OrcAmbush Cards.orcAmbush

-- | Surge.
-- When Revealed: All Orc enemies in the staging area engage the first player. If there
-- are no Orc enemies in the staging area, return all Orc enemies in the encounter discard
-- pile to the staging area, if able.
instance RunMessage OrcAmbush where
  runMessage msg t@(OrcAmbush attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      orcs <- select (EnemyInStagingArea <> EnemyWithTrait Orc)
      if null orcs
        then do
          discards <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
          traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Orc) discards
        else do
          lead <- getLead
          for_ orcs (engageEnemy lead)
      pure t
    _ -> OrcAmbush <$> runMessage msg attrs
