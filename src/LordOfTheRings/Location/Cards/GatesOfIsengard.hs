module LordOfTheRings.Location.Cards.GatesOfIsengard (gatesOfIsengard) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GatesOfIsengard = GatesOfIsengard LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

gatesOfIsengard :: LocationCard GatesOfIsengard
gatesOfIsengard = location GatesOfIsengard Cards.gatesOfIsengard

-- | Immune to player card effects.
instance HasModifiersFor GatesOfIsengard where
  getModifiersFor (GatesOfIsengard a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When Gates of Isengard is explored, each player searches the encounter deck and
-- discard pile for an enemy and puts it into play engaged with him. Shuffle the encounter
-- deck.
instance HasAbilities GatesOfIsengard where
  getAbilities (GatesOfIsengard a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage GatesOfIsengard where
  runMessage msg l@(GatesOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p a (CardWithType EnemyType)
      pure l
    _ -> GatesOfIsengard <$> runMessage msg a
