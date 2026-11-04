module LordOfTheRings.Location.Cards.BlockedTunnel (blockedTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Weapon))

newtype BlockedTunnel = BlockedTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blockedTunnel :: LocationCard BlockedTunnel
blockedTunnel = location BlockedTunnel Cards.blockedTunnel

-- | Action: Exhaust a character with a Weapon attachment to place 3 progress on Blocked
-- Tunnel. (Any player may trigger this effect.)
instance HasAbilities BlockedTunnel where
  getAbilities (BlockedTunnel a) =
    [anyPlayer $ mkAbility a 1 $ ActionAbility $ ExhaustCharacterCost armedHero armedAlly]
   where
    armedHero = HeroControlledBy ActivePlayer <> HeroWithAttached Weapon
    armedAlly = AllyControlledBy ActivePlayer <> AllyWithAttached Weapon

-- | When Revealed: Make Blocked Tunnel the active location, returning any other active
-- location to the staging area.
instance RunMessage BlockedTunnel where
  runMessage msg l@(BlockedTunnel a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      active <- filter (/= a.id) <$> select IsActiveLocation
      for_ active (`placeLocation` StagingArea)
      makeActive a.id
      pure l
    -- the character is exhausted as the cost is paid
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 3
      pure l
    _ -> BlockedTunnel <$> runMessage msg a
