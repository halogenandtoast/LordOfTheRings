module LordOfTheRings.Location.Cards.DeepingCulvert (deepingCulvert) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype DeepingCulvert = DeepingCulvert LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

deepingCulvert :: LocationCard DeepingCulvert
deepingCulvert = location DeepingCulvert Cards.deepingCulvert

-- | X is 1 more than the number of allies controlled by the first player (0 printed).
instance HasModifiersFor DeepingCulvert where
  getModifiersFor (DeepingCulvert a) = do
    lead <- getLead
    allies <- length <$> select (AllyControlledBy $ PlayerWithId lead)
    modifySelf a [ThreatModifier (allies + 1)]

-- | Forced: When Deeping Culvert is explored, each player searches the encounter deck and
-- discard pile for an enemy, reveals it, and adds it to the staging area. Shuffle the
-- encounter deck. Each player reveals his as its revealer.
instance HasAbilities DeepingCulvert where
  getAbilities (DeepingCulvert a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage DeepingCulvert where
  runMessage msg l@(DeepingCulvert a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> asRevealer p $ searchEncounterDeckAndDiscardToReveal p a (CardWithType EnemyType)
      pure l
    _ -> DeepingCulvert <$> runMessage msg a
