module LordOfTheRings.Location.Cards.GoblinTunnels (goblinTunnels) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype GoblinTunnels = GoblinTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTunnels :: LocationCard GoblinTunnels
goblinTunnels = location GoblinTunnels Cards.goblinTunnels

-- | While Goblin Tunnels is in the staging area, it gains: "Forced: After a Goblin is
-- revealed from the encounter deck, remove a progress token from the current quest card."
instance HasAbilities GoblinTunnels where
  getAbilities (GoblinTunnels a) =
    [ restricted a 1 (exists $ LocationWithId a.id <> LocationInStagingArea)
        $ forced
        $ EnemyWithTraitRevealedWindow After Goblin
    ]

-- | Shadow: attacking enemy gets +1 [Attack] (+3 [Attack] instead if attacking enemy is a
-- Goblin.)
instance RunMessage GoblinTunnels where
  runMessage msg l@(GoblinTunnels a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeProgressFromQuest a 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      goblin <- selectAny (EnemyWithId eid <> EnemyWithTrait Goblin)
      enemyGetsAttack a eid (if goblin then 3 else 1)
      pure l
    _ -> GoblinTunnels <$> runMessage msg a
