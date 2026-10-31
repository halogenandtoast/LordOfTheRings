module LordOfTheRings.Location.Cards.OrcTrail (orcTrail) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (searches)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcTrail = OrcTrail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcTrail :: LocationCard OrcTrail
orcTrail = location OrcTrail Cards.orcTrail

-- | Forced: After Orc Trail leaves play as an explored location, choose an out of play Orc
-- enemy, either set aside or in the victory display, and add it to the staging area.
-- Then, the first player searches 4.
instance HasAbilities OrcTrail where
  getAbilities (OrcTrail a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage OrcTrail where
  runMessage msg l@(OrcTrail a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let orc = CardWithType EnemyType <> CardWithTrait Orc
          asideOrcs = filter (cardMatches orc) scenario.scenarioSetAside
          victory = [c | EncounterCard c <- scenario.victoryDisplay, cardMatches orc c]
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "orcTrail.choose"
        causedBy a
        targets asideOrcs addToStaging
        targets victory \c -> removeFromVictoryDisplay (EncounterCard c) >> addToStaging c
      searches lead 4
      pure l
    _ -> OrcTrail <$> runMessage msg a
