module LordOfTheRings.Treachery.Cards.ChanceEncounter (chanceEncounter) where

import LordOfTheRings.Card (CardType (EnemyType), toCardDef)
import LordOfTheRings.Card.CardDef (CardDef (..))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ChanceEncounter = ChanceEncounter TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chanceEncounter :: TreacheryCard ChanceEncounter
chanceEncounter = treachery ChanceEncounter Cards.chanceEncounter

-- | When Revealed: Put the top enemy in the encounter discard pile into play, engaged with
-- the first player. If this effect put no enemies into play, Chance Encounter gains surge.
-- Shadow: attacking enemy gets +1 attack (+3 attack instead if engaged with the first
-- player).
instance RunMessage ChanceEncounter where
  runMessage msg t@(ChanceEncounter attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discards <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      case find ((== EnemyType) . cdCardType . toCardDef) discards of
        Just card -> do
          lead <- getLead
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged lead card
        Nothing -> revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      lead <- getLead
      enemyGetsAttack attrs eid (if pid == lead then 3 else 1)
      pure t
    _ -> ChanceEncounter <$> runMessage msg attrs
