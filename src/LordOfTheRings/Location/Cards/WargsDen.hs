module LordOfTheRings.Location.Cards.WargsDen (wargsDen) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (searchToRevealWarg, wargEnemy)

newtype WargsDen = WargsDen LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargsDen :: LocationCard WargsDen
wargsDen = location WargsDen Cards.wargsDen

-- | Immune to player card effects.
-- With no Warg enemy in the encounter deck or discard pile to find, its Travel cost cannot
-- be paid.
instance HasModifiersFor WargsDen where
  getModifiersFor (WargsDen a) = do
    found <- getsGame \g -> let s = toAttrs g.scenario in any (cardMatches wargEnemy) (s.encounterDeck <> s.encounterDiscard)
    modified a a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | not found]

-- | Travel: The first player searches the encounter deck and discard pile for a Warg enemy,
-- reveals it, and puts it into play engaged with him. Shuffle the encounter deck.
instance RunMessage WargsDen where
  runMessage msg (WargsDen a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      searchToRevealWarg lead a True
      WargsDen <$> runMessage msg a
    _ -> WargsDen <$> runMessage msg a
