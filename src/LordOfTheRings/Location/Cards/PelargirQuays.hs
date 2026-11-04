module LordOfTheRings.Location.Cards.PelargirQuays (pelargirQuays) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PelargirQuays = PelargirQuays LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirQuays :: LocationCard PelargirQuays
pelargirQuays = location PelargirQuays Cards.pelargirQuays

-- | Forced: At the end of the round, place 1 damage token here. Then, if there are 4
-- damage tokens here, discard Pelargir Quays and assign 4 damage among characters in play.
instance HasAbilities PelargirQuays where
  getAbilities (PelargirQuays a) = [mkAbility a 1 $ forced RoundEndsWindow]

-- | The first player assigns the damage, the characters in play being every player's.
instance RunMessage PelargirQuays where
  runMessage msg l@(PelargirQuays a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeTokens a a Damage 1
      doStep 2 msg
      pure l
    DoStep 2 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Damage a.locationTokens >= 4) do
        discard a
        lead <- getLead
        characters <- selectCharacters AnyHero (AllyControlledBy AnyPlayer)
        dealDamageDividedAmong a lead characters 4
      pure l
    _ -> PelargirQuays <$> runMessage msg a
