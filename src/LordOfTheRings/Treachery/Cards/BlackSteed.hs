module LordOfTheRings.Treachery.Cards.BlackSteed (blackSteed) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackSteed = BlackSteed TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
blackSteed :: TreacheryCard BlackSteed
blackSteed = treachery BlackSteed Cards.blackSteed

-- | Counts as a Mount attachment with the text: "Limit 1 per enemy. Attached enemy gets -10
-- engagement cost and gains: 'Forced: At the end of the refresh phase, return attached enemy
-- to the staging area.'"
instance HasModifiersFor BlackSteed where
  getModifiersFor (BlackSteed a) = withHost a \host -> modified a host [EngagementCostModifier (-10)]

instance HasAbilities BlackSteed where
  getAbilities (BlackSteed a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> [restricted a 1 (EnemyExists $ EnemyWithId eid <> NotEnemy EnemyInStagingArea) $ forced $ PhaseEndsWindow When RefreshPhase]
    _ -> []

-- | When Revealed: Attach to a Nazgûl enemy in play, if able: one without a Black Steed.
-- The player who revealed it chooses.
instance RunMessage BlackSteed where
  runMessage msg t@(BlackSteed attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      taken <- hostsOfTreachery (toCardCode Cards.blackSteed)
      nazgul <- filter (\e -> not $ any (isTarget e) taken) <$> select (EnemyWithTrait Nazgul)
      chooseOrRunOneM you do
        questionLabeled "blackSteed.attach"
        causedBy attrs
        targets nazgul $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \case
        EnemyTarget eid -> placeEnemy eid StagingArea
        _ -> pure ()
      pure t
    _ -> BlackSteed <$> runMessage msg attrs
