module LordOfTheRings.Location.Cards.BlightOfCarnDum (blightOfCarnDum) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (isThaurdir)

newtype BlightOfCarnDum = BlightOfCarnDum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blightOfCarnDum :: LocationCard BlightOfCarnDum
blightOfCarnDum = location BlightOfCarnDum Cards.blightOfCarnDum

-- | Forced: After an enemy attacks, heal 1 damage from it.
instance HasAbilities BlightOfCarnDum where
  getAbilities (BlightOfCarnDum a) = [mkAbility a 1 $ forced $ AnyEnemyAttacksWindow After]

-- | Travel: Deal each enemy in play 1 shadow card to travel here.
-- Shadow: If the attacking enemy is Thaurdir, heal 3 damage from him.
instance RunMessage BlightOfCarnDum where
  runMessage msg l@(BlightOfCarnDum a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyAttacking eid _) <- ws] \eid -> healDamage a eid 1
      pure l
    TravelTo lid | lid == a.id -> do
      traverse_ dealShadowCard =<< select AnyEnemy
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      thaurdir <- isThaurdir eid
      when thaurdir $ healDamage a eid 3
      pure l
    _ -> BlightOfCarnDum <$> runMessage msg a
