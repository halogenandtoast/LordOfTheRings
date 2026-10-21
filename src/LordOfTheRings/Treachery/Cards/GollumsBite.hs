module LordOfTheRings.Treachery.Cards.GollumsBite (gollumsBite) where

import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard, passGollum, tantrumTargets)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GollumsBite = GollumsBite TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumsBite :: TreacheryCard GollumsBite
gollumsBite = treachery GollumsBite Cards.gollumsBite

-- | When Revealed: Deal 4 damage to a hero controlled by the player guarding Gollum. That
-- player must choose a new player to guard Gollum, if able.
-- Shadow: Deal 2 damage to a hero controlled by the player guarding Gollum.
instance RunMessage GollumsBite where
  runMessage msg t@(GollumsBite attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (bite 4) =<< tantrumTargets
      passGollum False
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      traverse_ (bite 2) =<< getGuard
      pure t
    _ -> GollumsBite <$> runMessage msg attrs
   where
    bite n p = do
      heroes <- select (HeroControlledBy $ PlayerWithId p)
      chooseOrRunOneM p do
        questionLabeled "gollumsBite.hero"
        causedBy attrs
        targets heroes \h -> dealDamage attrs h n
