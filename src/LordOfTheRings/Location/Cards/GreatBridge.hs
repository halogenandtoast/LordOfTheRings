module LordOfTheRings.Location.Cards.GreatBridge (greatBridge) where

import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownDamage)

newtype GreatBridge = GreatBridge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatBridge :: LocationCard GreatBridge
greatBridge = location GreatBridge Cards.greatBridge

instance RunMessage GreatBridge where
  runMessage msg l@(GreatBridge a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      damage <- lakeTownDamage
      players <- traverse getPlayer =<< getPlayerOrder
      for_ players \player -> discardTopOfDeck player.id damage
      when (damage == 0 || all (null . (.deck)) players) $
        phaseModifier a a (GainKeyword Surge)
      pure l
    _ -> GreatBridge <$> runMessage msg a
