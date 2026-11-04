module LordOfTheRings.Location.Cards.TempleOfSauron (templeOfSauron) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers (placeDamageOnTribute, questStagesInVictoryDisplay)

newtype TempleOfSauron = TempleOfSauron LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

templeOfSauron :: LocationCard TempleOfSauron
templeOfSauron = location TempleOfSauron Cards.templeOfSauron

-- | Forced: When Temple of Sauron leaves play, place 1 damage token on Tribute to Mordor.
-- Read in the window as it leaves, which nothing comes before.
instance HasAbilities TempleOfSauron where
  getAbilities (TempleOfSauron a) = [mkAbility a 1 $ forced $ LocationLeftPlayWindow After a.id]

-- | Shadow: Attacking enemy gets +1 Attack for each quest stage in the victory display.
instance RunMessage TempleOfSauron where
  runMessage msg l@(TempleOfSauron a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeDamageOnTribute a
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      stages <- questStagesInVictoryDisplay
      when (stages > 0) $ enemyGetsAttack (CardIdSource cid) eid stages
      pure l
    _ -> TempleOfSauron <$> runMessage msg a
