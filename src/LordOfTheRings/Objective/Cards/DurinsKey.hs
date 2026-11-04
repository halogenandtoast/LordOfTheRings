module LordOfTheRings.Objective.Cards.DurinsKey (durinsKey) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype DurinsKey = DurinsKey ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

durinsKey :: ObjectiveCard DurinsKey
durinsKey = objective DurinsKey Cards.durinsKey

-- | While Durin's Key is free of encounters, the first player attaches it to a hero he
-- controls.
instance HasModifiersFor DurinsKey where
  getModifiersFor (DurinsKey a) = when (a.placement == StagingArea) $ modifySelf a [FirstPlayerClaimsWhenFree]

-- | Action: Exhaust Durin's Key to give each location -1 threat until the end of the phase.
-- Forced: At the beginning of the quest phase, the player who controls Durin's Key looks at
-- the top 3 cards of the encounter deck, chooses 1 to reveal, and discards the rest.
instance HasAbilities DurinsKey where
  getAbilities (DurinsKey a) = case a.placement of
    AttachedTo (HeroTarget hid) ->
      [ anyPlayer $ restricted a 1 (exists $ HeroWithId hid <> HeroControlledBy ActivePlayer) $ ActionAbility Free
      | not a.objectiveExhausted
      ]
        <> [mkAbility a 2 $ forced $ PhaseBeginsWindow After QuestPhase]
    _ -> []

-- | The cards not chosen are discarded before the chosen one is revealed, so that what it
-- reveals next, as by surge, comes from below them.
instance RunMessage DurinsKey where
  runMessage msg o@(DurinsKey attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      exhaust attrs
      locations <- select AnyLocation
      for_ locations \l -> phaseModifier attrs l (ThreatModifier (-1))
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [attrs.placement]] \hid -> do
        pid <- (.owner) <$> getHero hid
        top <- getsGame (take 3 . (.encounterDeck) . toAttrs . (.scenario))
        chooseOneM pid do
          questionLabeled "durinsKey.reveal"
          causedBy attrs
          showing (map EncounterCard top)
          targets top \c -> do
            traverse_ discardFromEncounterDeck (filter (/= c) top)
            revealFromEncounterDeck c
      pure o
    _ -> DurinsKey <$> runMessage msg attrs
