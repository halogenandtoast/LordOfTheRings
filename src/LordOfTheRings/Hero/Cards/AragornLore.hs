module LordOfTheRings.Hero.Cards.AragornLore (aragornLore) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype AragornLore = AragornLore HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, as its card def says.
aragornLore :: HeroCard AragornLore
aragornLore = hero AragornLore Cards.aragornLore

-- | Refresh Action: Reduce your threat to your starting threat level. (Limit once per
-- game.)
instance HasAbilities AragornLore where
  getAbilities (AragornLore a) =
    [limited (PlayerLimit PerGame 1) $ restricted a 1 (DuringPhase RefreshPhase) $ ActionAbility Free]

instance RunMessage AragornLore where
  runMessage msg h@(AragornLore attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      player <- getPlayer pid
      when (player.threat > player.startingThreat) $ lowerThreatBy attrs pid (player.threat - player.startingThreat)
      pure h
    _ -> AragornLore <$> runMessage msg attrs
