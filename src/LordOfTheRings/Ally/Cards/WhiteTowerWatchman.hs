module LordOfTheRings.Ally.Cards.WhiteTowerWatchman (whiteTowerWatchman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype WhiteTowerWatchman = WhiteTowerWatchman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

whiteTowerWatchman :: AllyCard WhiteTowerWatchman
whiteTowerWatchman = ally WhiteTowerWatchman Cards.whiteTowerWatchman

-- | If each hero you control belongs to the same sphere of influence, you may assign
-- damage from undefended enemy attacks to White Tower Watchman instead of a hero you
-- control.
instance HasModifiersFor WhiteTowerWatchman where
  getModifiersFor (WhiteTowerWatchman a) = for_ (placementController a.placement) \pid -> do
    spheres <- ordNub . map (.sphere) <$> (traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid))
    when (length spheres == 1) $ modified a (PlayerTarget pid) [UndefendedDamageMayGoTo (toTarget a)]
