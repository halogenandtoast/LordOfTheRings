module LordOfTheRings.Location.Cards.SpiderDen (spiderDen) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Spider))

newtype SpiderDen = SpiderDen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

spiderDen :: LocationCard SpiderDen
spiderDen = location SpiderDen Cards.spiderDen

instance HasAbilities SpiderDen where
  getAbilities (SpiderDen a) =
    [ anyPlayer $ restricted a 1 (DuringPhase QuestPhase <> CardInEncounterDeckOrDiscard spiders) $ ActionAbility Free
    ]
   where
    spiders = CardWithType EnemyType <> CardWithTrait Spider

instance RunMessage SpiderDen where
  runMessage msg l@(SpiderDen a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchEncounterDeckAndDiscardToEngage pid a (CardWithType EnemyType <> CardWithTrait Spider)
      discard a a
      pure l
    _ -> SpiderDen <$> runMessage msg a
