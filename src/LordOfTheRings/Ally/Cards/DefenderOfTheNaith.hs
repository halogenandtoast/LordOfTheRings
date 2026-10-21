module LordOfTheRings.Ally.Cards.DefenderOfTheNaith (defenderOfTheNaith) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype DefenderOfTheNaith = DefenderOfTheNaith AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

defenderOfTheNaith :: AllyCard DefenderOfTheNaith
defenderOfTheNaith = ally DefenderOfTheNaith Cards.defenderOfTheNaith

-- | Sentinel. Response: After a Silvan ally you control leaves play, ready Defender of
-- the Naith.
instance HasAbilities DefenderOfTheNaith where
  getAbilities (DefenderOfTheNaith a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> ExhaustedAlly)
        $ response
        $ AllyWithTraitLeftPlayWindow After a.owner Silvan
    ]

instance RunMessage DefenderOfTheNaith where
  runMessage msg s@(DefenderOfTheNaith a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> DefenderOfTheNaith <$> runMessage msg a
