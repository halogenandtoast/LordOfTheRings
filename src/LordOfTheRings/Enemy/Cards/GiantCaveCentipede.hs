module LordOfTheRings.Enemy.Cards.GiantCaveCentipede (giantCaveCentipede) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Player.Types (discardableHand)

newtype GiantCaveCentipede = GiantCaveCentipede EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantCaveCentipede :: EnemyCard GiantCaveCentipede
giantCaveCentipede = enemy GiantCaveCentipede Cards.giantCaveCentipede

-- | Forced: At the end of the combat phase, if Giant Centipede is engaged with a player,
-- that player chooses and adds it to a different player's staging area.
instance HasAbilities GiantCaveCentipede where
  getAbilities (GiantCaveCentipede a) = case a.placement of
    EngagedWith _ -> [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase]
    _ -> []

-- | When Revealed: Each player at this stage shuffles the card in his hand with the
-- highest printed cost back into his deck.
instance RunMessage GiantCaveCentipede where
  runMessage msg e@(GiantCaveCentipede attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        hand <- discardableHand <$> getPlayer p
        let most = maximumMay (map printedCostOf hand)
        chooseOrRunOneM p do
          questionLabeled "giantCaveCentipede.shuffle"
          causedBy attrs
          targets [c | c <- hand, Just (printedCostOf c) == most] \c -> push $ ShuffleFromHandIntoOwnersDeck p c.id
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      when (length players > 1) $ placeEnemy attrs.id StagingArea
      pure e
    _ -> GiantCaveCentipede <$> runMessage msg attrs
