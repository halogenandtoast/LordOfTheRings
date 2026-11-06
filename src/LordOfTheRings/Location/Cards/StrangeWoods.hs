module LordOfTheRings.Location.Cards.StrangeWoods (strangeWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StrangeWoods = StrangeWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

strangeWoods :: LocationCard StrangeWoods
strangeWoods = location StrangeWoods Cards.strangeWoods

instance HasModifiersFor StrangeWoods where
  getModifiersFor (StrangeWoods a) = when (a.placement == StagingArea) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \target -> modified a target [CannotBeHealed]

instance RunMessage StrangeWoods where
  runMessage msg l@(StrangeWoods a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      attackModifier a eid (AttackModifier 1)
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      eachPlayer (`encounterRaisesThreat` 2)
      pure l
    _ -> StrangeWoods <$> runMessage msg a
