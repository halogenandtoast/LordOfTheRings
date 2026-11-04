module LordOfTheRings.Location.Cards.ParthGalen (parthGalen) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ParthGalen = ParthGalen LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

parthGalen :: LocationCard ParthGalen
parthGalen = location ParthGalen Cards.parthGalen

-- | Immune to player card effects. While Parth Galen is in play, the players cannot win the
-- game.
instance HasModifiersFor ParthGalen where
  getModifiersFor (ParthGalen a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    modified a GameTarget [PlayersCannotWin]

-- | Travel: Search the encounter deck and discard pile for an enemy and add it to the
-- staging area to travel here. Shuffle the encounter deck.
instance RunMessage ParthGalen where
  runMessage msg (ParthGalen a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getStageLead
      searchEncounterDeckAndDiscard lead a (CardWithType EnemyType)
      ParthGalen <$> runMessage msg a
    _ -> ParthGalen <$> runMessage msg a
