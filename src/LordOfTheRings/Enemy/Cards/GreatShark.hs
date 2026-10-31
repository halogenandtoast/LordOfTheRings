module LordOfTheRings.Enemy.Cards.GreatShark (greatShark) where

import LordOfTheRings.Card (Card (EncounterCard), cdCardTraits, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Trait (Trait (Underwater))

newtype GreatShark = GreatShark EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

greatShark :: EnemyCard GreatShark
greatShark = enemy GreatShark Cards.greatShark

-- | Great Shark gets +1 threat, +1 attack, and +1 defense for each Underwater location in
-- the victory display.
-- Great Shark cannot be engaged or damaged unless the active location is Underwater.
instance HasModifiersFor GreatShark where
  getModifiersFor (GreatShark a) = do
    victory <- getsGame (scenarioVictoryDisplay . toAttrs . gameScenario)
    let x = length [c | EncounterCard c <- victory, Underwater `member` cdCardTraits (toCardDef c)]
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x, DefenseModifier x]
    unlessM underwaterActive do
      players <- getRemainingPlayers
      modifySelf a (CannotTakeDamage : map CannotEngagePlayer players)

-- | Forced: At the beginning of the encounter phase, return Great Shark to the staging area
-- unless the active location is Underwater.
instance HasAbilities GreatShark where
  getAbilities (GreatShark a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase | EngagedWith _ <- [a.placement]]

instance RunMessage GreatShark where
  runMessage msg e@(GreatShark attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      unlessM underwaterActive $ placeEnemy attrs.id StagingArea
      pure e
    _ -> GreatShark <$> runMessage msg attrs
