module LordOfTheRings.Objective.Cards.Daybreak (daybreak, nightfall) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (returnEngagedEnemiesToStaging)

-- | The Time objective of The Wastes of Eriador: one card, Daybreak on one side and
-- Nightfall on the other, the side faceup kept as its card code.
newtype Daybreak = Daybreak ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

daybreak :: ObjectiveCard Daybreak
daybreak = objective Daybreak Cards.daybreak

nightfall :: ObjectiveCard Daybreak
nightfall = objective Daybreak Cards.nightfall

night :: ObjectiveAttrs -> Bool
night a = toCardCode a == toCardCode Cards.nightfall

-- | Daybreak: It is Day. Enemies do not make engagement checks.
-- Nightfall: It is Night. Progress cannot be placed on quest cards. Encounter card effects
-- cannot be canceled.
instance HasModifiersFor Daybreak where
  getModifiersFor (Daybreak a)
    | night a = do
        modified a GameTarget [CannotCancelEncounterEffects]
        quests <- getQuestCards
        for_ quests \q -> modified a q [ProgressPerRoundLimit 0]
    | otherwise = modified a GameTarget [NoEngagementChecks]

-- | Forced: At the end of the round, flip this objective.
-- Daybreak: Forced: When it becomes Day, return each enemy in play to the staging area.
-- Nightfall: Forced: When it becomes Night, reveal an encounter card.
instance HasAbilities Daybreak where
  getAbilities (Daybreak a) =
    [ mkAbility a 1 $ forced RoundEndsWindow
    , mkAbility a 2 $ forced $ BecomesWindow $ if night a then Nighttime else Daytime
    ]

instance RunMessage Daybreak where
  runMessage msg o@(Daybreak attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ Flip (toTarget attrs)
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _
      | night attrs -> revealEncounterCard >> pure o
      | otherwise -> returnEngagedEnemiesToStaging >> pure o
    Flip (isTarget attrs -> True) -> do
      let (side, time) = if night attrs then (Cards.daybreak, Daytime) else (Cards.nightfall, Nighttime)
      push $ CheckWindows [mkAfter $ Became time]
      pure $ Daybreak $ attrs {objectiveCardCode = toCardCode side}
    _ -> Daybreak <$> runMessage msg attrs
