module LordOfTheRings.Objective.Cards.TheThreatToWilderland (theThreatToWilderland) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype TheThreatToWilderland = TheThreatToWilderland ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, from The King's Quest. Its Setup, to the staging
-- area with X resources, is the campaign's.
theThreatToWilderland :: ObjectiveCard TheThreatToWilderland
theThreatToWilderland = objective TheThreatToWilderland Cards.theThreatToWilderland

{- | While the players' choice holds, until the end of the phase: all encounter cards are
immune to player card effects, those that enter play during it too. The choice is a
lasting effect of this card's on itself, read back here.
-}
instance HasModifiersFor TheThreatToWilderland where
  getModifiersFor (TheThreatToWilderland a) = do
    immune <- getsGame (any (\e -> e.effectSource == toSource a && e.effectTarget == toTarget a && EncounterCardsImmune `elem` e.effectModifiers) . gameEffects)
    when immune do
      es <- getsGame (.entities)
      for_ (toList es.enemies) \x -> modified a (toTarget x) [ImmuneToPlayerCardEffects]
      for_ (toList es.locations) \x -> modified a (toTarget x) [ImmuneToPlayerCardEffects]
      for_ (toList es.treacheries) \x -> modified a (toTarget x) [ImmuneToPlayerCardEffects]
      for_ (toList es.objectives) \x -> modified a (toTarget x) [ImmuneToPlayerCardEffects]

{- | Forced: At the beginning of the quest phase, remove a resource from The Threat to
Wilderland. Then, either raise each player's threat by 2 or all encounter cards are immune
to player card effects until the end of the phase. The first player chooses.
-}
instance HasAbilities TheThreatToWilderland where
  getAbilities (TheThreatToWilderland a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheThreatToWilderland where
  runMessage msg o@(TheThreatToWilderland attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      when (attrs.resources > 0) $ removeTokens attrs attrs Resource 1
      chooseOneM lead do
        questionLabeled "theThreatToWilderland.choose"
        causedBy attrs
        labeled "theThreatToWilderland.threat" $ eachPlayer \p -> encounterRaisesThreat p 2
        labeled "theThreatToWilderland.immune" $ phaseModifier attrs attrs EncounterCardsImmune
      pure o
    _ -> TheThreatToWilderland <$> runMessage msg attrs
