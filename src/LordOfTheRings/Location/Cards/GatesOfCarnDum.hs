module LordOfTheRings.Location.Cards.GatesOfCarnDum (gatesOfCarnDum) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GatesOfCarnDum = GatesOfCarnDum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gatesOfCarnDum :: LocationCard GatesOfCarnDum
gatesOfCarnDum = location GatesOfCarnDum Cards.gatesOfCarnDum

-- | Forced: After Gates of Carn Dûm is explored, deal it as a shadow card to the enemy in
-- the staging area with the lowest engagement cost.
instance HasAbilities GatesOfCarnDum where
  getAbilities (GatesOfCarnDum a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Explored, it is discarded as usual, then leaves the discard pile as the enemy's shadow
-- card; of enemies tied, the first player chooses.
-- Shadow: Attacking enemy gets +1 attack and cannot take damage this round.
instance RunMessage GatesOfCarnDum where
  runMessage msg l@(GatesOfCarnDum a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      staged <- select EnemyInStagingArea
      costs <- for staged \e -> (e,) <$> getEngagementCost e
      let lowest = minimumMay (map snd costs)
          card = MkEncounterCard a.cardId (toCardCode a)
      lead <- getLead
      unless (null staged) do
        cancelMatching \case
          Discard (LocationTarget lid) -> lid == a.id
          _ -> False
        discard a
        removeFromEncounterDiscard card
        chooseOrRunOneM lead do
          questionLabeled "gatesOfCarnDum.enemy"
          causedBy a
          targets [e | (e, c) <- costs, Just c == lowest] \e -> push $ AttachShadowCard e card
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 1
      roundModifier (CardIdSource cid) eid CannotTakeDamage
      pure l
    _ -> GatesOfCarnDum <$> runMessage msg a
