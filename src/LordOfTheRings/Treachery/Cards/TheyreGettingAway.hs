module LordOfTheRings.Treachery.Cards.TheyreGettingAway (theyreGettingAway) where

import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (scrollEnemy)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheyreGettingAway = TheyreGettingAway TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theyreGettingAway :: TreacheryCard TheyreGettingAway
theyreGettingAway = treachery TheyreGettingAway Cards.theyreGettingAway

-- | Surge.
-- When Revealed: If Alcaron's Scroll is attached to an enemy, choose one: either add 1
-- progress token to that enemy, or each card in the staging area gets +2 threat until the
-- end of the phase.
instance RunMessage TheyreGettingAway where
  runMessage msg t@(TheyreGettingAway attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      scrollEnemy >>= traverse_ \e -> do
        lead <- getLead
        enemies <- select EnemyInStagingArea
        locations <- select LocationInStagingArea
        chooseOneM lead do
          questionLabeled "theyreGettingAway.choose"
          causedBy attrs
          labeled "theyreGettingAway.progress" $ placeProgress attrs e 1
          labeled "theyreGettingAway.threat" do
            for_ enemies \x -> phaseModifier attrs x (ThreatModifier 2)
            for_ locations \x -> phaseModifier attrs x (ThreatModifier 2)
      pure t
    _ -> TheyreGettingAway <$> runMessage msg attrs
