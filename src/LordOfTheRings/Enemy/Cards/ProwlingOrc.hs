module LordOfTheRings.Enemy.Cards.ProwlingOrc (prowlingOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (isScour, placeDamageOnActiveLocation)

newtype ProwlingOrc = ProwlingOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

prowlingOrc :: EnemyCard ProwlingOrc
prowlingOrc = enemy ProwlingOrc Cards.prowlingOrc

-- | Forced: After Prowling Orc attacks, place 1 damage on the active location.
instance HasAbilities ProwlingOrc where
  getAbilities (ProwlingOrc a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Scour: Prowling Orc engages the player with the highest threat and makes an immediate
-- attack. The first player chooses among players tied for it. Triggered from the discard
-- pile, it is not in play to engage anyone, so it does nothing.
instance RunMessage ProwlingOrc where
  runMessage msg e@(ProwlingOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeDamageOnActiveLocation attrs 1
      pure e
    _ | isScour attrs msg -> do
      unless (attrs.placement == Unplaced) do
        players <- traverse getPlayer =<< getRemainingPlayers
        let highest = foldr (max . (.threat)) 0 players
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "prowlingOrc.engage"
          causedBy attrs
          targets [p.id | p <- players, p.threat == highest] \pid -> do
            engageEnemy pid attrs.id
            immediateAttack pid attrs.id
      pure e
    _ -> ProwlingOrc <$> runMessage msg attrs
