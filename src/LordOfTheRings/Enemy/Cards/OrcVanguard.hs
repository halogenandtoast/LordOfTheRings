module LordOfTheRings.Enemy.Cards.OrcVanguard (orcVanguard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Sphere (Sphere (Leadership, Lore, Spirit))

newtype OrcVanguard = OrcVanguard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcVanguard :: EnemyCard OrcVanguard
orcVanguard = enemy OrcVanguard Cards.orcVanguard

-- | While Orc Vanguard is in the staging area, resources cannot be spent from the resource
-- pools of heroes who have a leadership, spirit, or lore resource icon, printed or gained.
instance HasModifiersFor OrcVanguard where
  getModifiersFor (OrcVanguard a) = when (a.placement == StagingArea) do
    heroes <- concatMapM (select . HeroWithSphere) [Leadership, Spirit, Lore]
    for_ (ordNub heroes) \h -> modified a h [CannotSpendResources]

-- | Shadow: Deal the attacking enemy two additional shadow cards.
instance RunMessage OrcVanguard where
  runMessage msg e@(OrcVanguard attrs) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      dealShadowCardsDuringAttack eid pid defenders 2
      pure e
    _ -> OrcVanguard <$> runMessage msg attrs
