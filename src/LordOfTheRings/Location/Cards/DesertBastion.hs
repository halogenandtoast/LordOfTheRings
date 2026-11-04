module LordOfTheRings.Location.Cards.DesertBastion (desertBastion) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradObjectives)

newtype DesertBastion = DesertBastion LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

desertBastion :: LocationCard DesertBastion
desertBastion = location DesertBastion Cards.desertBastion

-- | The Harad objectives attached to heroes: those the players control.
onHeroes :: HasGame m => m Int
onHeroes = length . filter (\o -> case o.placement of AttachedTo (HeroTarget _) -> True; _ -> False) <$> haradObjectives

-- | Desert Bastion gets +1 threat for each Harad objective attached to a hero.
instance HasModifiersFor DesertBastion where
  getModifiersFor (DesertBastion a) = do
    n <- onHeroes
    when (n > 0) $ modifySelf a [ThreatModifier n]

-- | Shadow: Attacking enemy gets +1 attack for each Harad objective the players control.
instance RunMessage DesertBastion where
  runMessage msg l@(DesertBastion a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      n <- onHeroes
      when (n > 0) $ enemyGetsAttack a eid n
      pure l
    _ -> DesertBastion <$> runMessage msg a
