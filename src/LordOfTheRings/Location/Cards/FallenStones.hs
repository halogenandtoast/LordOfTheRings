module LordOfTheRings.Location.Cards.FallenStones (fallenStones) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FallenStones = FallenStones LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fallenStones :: LocationCard FallenStones
fallenStones = location FallenStones Cards.fallenStones

instance RunMessage FallenStones where
  runMessage msg l@(FallenStones attrs) = case msg of
    TravelTo lid | lid == attrs.id -> do
      lead <- getLead
      resources <- getBagginsResources
      chooseAmountM lead "fallenStones.spend" attrs 0 (min 3 resources) \n -> do
        when (n > 0) $ getBilbo >>= traverse_ \b -> spendResourcesFrom lead b n
        doStep n (WithTarget (toTarget attrs) msg)
      FallenStones <$> runMessage msg attrs
    DoStep n (WithTarget (isTarget attrs -> True) (TravelTo lid)) | lid == attrs.id -> do
      characters <- selectCharacters ReadyHero ReadyAlly
      when (n < 3) $ for_ characters \c -> dealDamage attrs c (3 - n)
      pure l
    _ -> FallenStones <$> runMessage msg attrs
