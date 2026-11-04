module LordOfTheRings.Location.Cards.Catacombs (catacombs) where

import LordOfTheRings.Card (CardDef (cdCardType), CardType (EnemyType), toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Catacombs = Catacombs LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

catacombs :: LocationCard Catacombs
catacombs = location Catacombs Cards.catacombs

-- | While Catacombs is in the staging area, enemies in the staging area cannot take damage.
instance HasModifiersFor Catacombs where
  getModifiersFor (Catacombs a) = when (a.placement == StagingArea) do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [CannotTakeDamage]

-- | Travel: Each player discards the top card of the encounter deck. If a player discards
-- an enemy this way, he puts it into play engaged with him.
instance RunMessage Catacombs where
  runMessage msg l@(Catacombs a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> discardTopOfEncounterDeckFor a (PlayerTarget p) 1
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) (PlayerTarget pid) cards -> do
      for_ (filter ((== EnemyType) . cdCardType . toCardDef) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure l
    _ -> Catacombs <$> runMessage msg a
