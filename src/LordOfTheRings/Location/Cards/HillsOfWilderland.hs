module LordOfTheRings.Location.Cards.HillsOfWilderland (hillsOfWilderland) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype HillsOfWilderland = HillsOfWilderland LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hillsOfWilderland :: LocationCard HillsOfWilderland
hillsOfWilderland = location HillsOfWilderland Cards.hillsOfWilderland

-- | X is the number of characters controlled by the player with the most characters: its
-- threat and its quest points.
instance HasModifiersFor HillsOfWilderland where
  getModifiersFor (HillsOfWilderland a) = do
    x <- mostCharactersControlled
    when (x > 0) $ modifySelf a [ThreatModifier x, QuestPointModifier x]

-- | Quest Action: Exhaust a character to reduce Hills of Wilderland's threat by 1 until the
-- end of the phase. Only the first player can trigger this effect.
instance HasAbilities HillsOfWilderland where
  getAbilities (HillsOfWilderland a) =
    [ anyPlayer
        $ restricted a 1 (DuringPhase QuestPhase <> PlayerExists (ActivePlayer <> FirstPlayer))
        $ ActionAbility (ExhaustCharacterCost (HeroControlledBy ActivePlayer) (AllyControlledBy ActivePlayer))
    ]

instance RunMessage HillsOfWilderland where
  runMessage msg l@(HillsOfWilderland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a a (ThreatModifier (-1))
      pure l
    _ -> HillsOfWilderland <$> runMessage msg a
