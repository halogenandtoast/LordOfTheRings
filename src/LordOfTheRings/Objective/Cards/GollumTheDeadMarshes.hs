module LordOfTheRings.Objective.Cards.GollumTheDeadMarshes (gollumTheDeadMarshes) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype GollumTheDeadMarshes = GollumTheDeadMarshes ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumTheDeadMarshes :: ObjectiveCard GollumTheDeadMarshes
gollumTheDeadMarshes = objective GollumTheDeadMarshes Cards.gollumTheDeadMarshes

-- | Forced: At the end of the quest phase, the party must make an escape test, dealing 1
-- card per player from the encounter deck. If this test is failed, place 2 resource
-- tokens on Gollum.
--
-- ponytail: the first player commits for the party.
instance HasAbilities GollumTheDeadMarshes where
  getAbilities (GollumTheDeadMarshes a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

-- | If Gollum ever has 8 or more resource tokens on him, shuffle him back into the
-- encounter deck.
instance RunMessage GollumTheDeadMarshes where
  runMessage msg o@(GollumTheDeadMarshes attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      players <- length <$> getRemainingPlayers
      escapeTest lead attrs players EscapeWillpower (placeResources attrs attrs 2) (pure ())
      pure o
    PlaceTokens _ (isTarget attrs -> True) Resource n | attrs.resources + n >= 8 -> do
      shuffleIntoEncounterDeck attrs
      GollumTheDeadMarshes <$> runMessage msg attrs
    _ -> GollumTheDeadMarshes <$> runMessage msg attrs
