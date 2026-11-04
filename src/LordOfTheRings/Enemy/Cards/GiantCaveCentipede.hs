module LordOfTheRings.Enemy.Cards.GiantCaveCentipede (giantCaveCentipede) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gamePlayerOrder)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Player.Types (discardableHand)

newtype GiantCaveCentipede = GiantCaveCentipede EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantCaveCentipede :: EnemyCard GiantCaveCentipede
giantCaveCentipede = enemy GiantCaveCentipede Cards.giantCaveCentipede

-- | Forced: At the end of the combat phase, if Giant Centipede is engaged with a player,
-- that player chooses and adds it to a different player's staging area: while the players
-- are separated, his area's, else the one they share.
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
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      -- the players of every area, not only of this one
      others <- getsGame (filter (/= pid) . gamePlayerOrder)
      chooseOrRunOneM pid do
        questionLabeled "giantCaveCentipede.player"
        causedBy attrs
        targets others \p -> placeInStagingAreaOf p attrs.id
      pure e
    _ -> GiantCaveCentipede <$> runMessage msg attrs
