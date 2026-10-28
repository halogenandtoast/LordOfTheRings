module LordOfTheRings.Event.Cards.AGoodHarvest (aGoodHarvest) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))

newtype AGoodHarvest = AGoodHarvest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

aGoodHarvest :: EventCard AGoodHarvest
aGoodHarvest = event AGoodHarvest Cards.aGoodHarvest

-- | Action: Name a sphere. Until the end of the phase, you can spend resources of any
-- sphere when paying for cards that belong to the named sphere. Each hero you control has
-- that sphere's resource icon while paying for them.
instance RunMessage AGoodHarvest where
  runMessage msg e@(AGoodHarvest attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "aGoodHarvest.nameSphere"
        causedBy attrs
        for_ [(Leadership, "leadership"), (Tactics, "tactics"), (Spirit, "spirit"), (Lore, "lore")] \(sphere, key) ->
          labeled ("aGoodHarvest." <> key)
            $ for_ heroes \h -> phaseModifier attrs h (GainResourceIconFor (CardWithSphere sphere) sphere)
      pure e
    _ -> AGoodHarvest <$> runMessage msg attrs
