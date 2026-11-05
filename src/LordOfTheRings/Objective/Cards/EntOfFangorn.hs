module LordOfTheRings.Objective.Cards.EntOfFangorn (entOfFangorn) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype EntOfFangorn = EntOfFangorn ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally in the staging area; once a player takes control of it, it is an
-- ally (see 'LordOfTheRings.Ally.Cards.EntOfFangorn').
entOfFangorn :: ObjectiveCard EntOfFangorn
entOfFangorn = objective EntOfFangorn Cards.entOfFangorn

-- | While Ent of Fangorn is in the staging area, it gains: "Quest Action: Exhaust a hero you
-- control to discard the top card of the encounter deck. If that hero's willpower is
-- greater than the discarded card's threat, place 1 resource here. Then, if there are 3
-- resources here, discard them to take control of Ent of Fangorn. Any player may trigger
-- this effect."
instance HasAbilities EntOfFangorn where
  getAbilities (EntOfFangorn a) =
    [ anyPlayer $ restricted a 1 (DuringPhase QuestPhase) $ ActionAbility (ExhaustHeroCost $ HeroControlledBy ActivePlayer)
    | a.placement == StagingArea
    ]

-- | The player who exhausted the hero is the one to take control.
instance RunMessage EntOfFangorn where
  runMessage msg o@(EntOfFangorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [h | Window After (Exhausted h@(HeroTarget _)) <- ws] \h -> discardTopOfEncounterDeckFor attrs h 1
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) hero cards -> do
      willpower <- getWillpower hero
      let threat = sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- cards]
      when (notNull cards && willpower > threat) $ placeResources attrs attrs 1
      doStep 1 msg
      pure o
    DoStep 1 (DiscardedTopOfEncounterDeck (isSource attrs -> True) hero _) -> do
      when (attrs.resources >= 3) do
        removeTokens attrs attrs Resource attrs.resources
        characterController hero >>= traverse_ (putObjectiveAllyIntoPlay attrs.id)
      pure o
    _ -> EntOfFangorn <$> runMessage msg attrs
