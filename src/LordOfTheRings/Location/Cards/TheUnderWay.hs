module LordOfTheRings.Location.Cards.TheUnderWay (theUnderWay) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Orc))

newtype TheUnderWay = TheUnderWay LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theUnderWay :: LocationCard TheUnderWay
theUnderWay = location TheUnderWay Cards.theUnderWay

-- | While The Under-way is in the staging area, each Orc enemy gets -5 engagement cost and
-- gains: "Forced: After this enemy engages a player, deal it a shadow card."
instance HasModifiersFor TheUnderWay where
  getModifiersFor (TheUnderWay a) = when (a.placement == StagingArea) do
    orcs <- select (EnemyWithTrait Orc)
    for_ orcs \e -> modified a e [EngagementCostModifier (-5)]

-- | The Forced effect each Orc enemy gains, one for each Under-way in the staging area.
instance HasAbilities TheUnderWay where
  getAbilities (TheUnderWay a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After (EnemyWithTrait Orc) | a.placement == StagingArea]

-- | Shadow: Deal the attacking enemy 2 additional shadow cards.
instance RunMessage TheUnderWay where
  runMessage msg l@(TheUnderWay a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- ws] dealShadowCard
      pure l
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      dealShadowCardsDuringAttack eid pid defenders 2
      pure l
    _ -> TheUnderWay <$> runMessage msg a
