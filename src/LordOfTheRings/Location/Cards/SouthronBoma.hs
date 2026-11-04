module LordOfTheRings.Location.Cards.SouthronBoma (southronBoma) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (isObjectiveHero)

newtype SouthronBoma = SouthronBoma LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronBoma :: LocationCard SouthronBoma
southronBoma = location SouthronBoma Cards.southronBoma

-- | X is the stage number of the main quest.
-- Travel: Each player spends 1 resource to travel here: barred while a player has none to
-- spend.
instance HasModifiersFor SouthronBoma where
  getModifiersFor (SouthronBoma a) = do
    x <- getMainStage
    players <- getRemainingPlayers
    broke <- anyM (\p -> selectNone (HeroControlledBy (PlayerWithId p) <> HeroWithResources)) players
    modifySelf a $ ThreatModifier x : [CannotBeTraveledTo | broke]

-- | Shadow: If the defending character is an objective-hero, ready it and cancel all
-- damage from this attack.
instance RunMessage SouthronBoma where
  runMessage msg l@(SouthronBoma a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> spendResourceFromAHero p a
      SouthronBoma <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      defenders <- getDefendersAgainst eid
      objectiveHeroes <- filterM isObjectiveHero defenders
      unless (null objectiveHeroes) do
        for_ objectiveHeroes ready
        cancelMatching \case
          AttackDamage e _ -> e == eid
          _ -> False
      pure l
    _ -> SouthronBoma <$> runMessage msg a
