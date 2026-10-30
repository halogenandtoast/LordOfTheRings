module LordOfTheRings.Quest.Cards.AcrossTheWastes (acrossTheWastes) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isDay)

newtype AcrossTheWastes = AcrossTheWastes QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheWastes :: QuestCard AcrossTheWastes
acrossTheWastes = quest AcrossTheWastes Cards.acrossTheWastes

-- | While it is Day, heroes get +1 willpower.
instance HasModifiersFor AcrossTheWastes where
  getModifiersFor (AcrossTheWastes a) = when a.flipped $ whenM isDay do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [WillpowerModifier 1]

-- | Forced: When it becomes Night, raise each player's threat by 1.
instance HasAbilities AcrossTheWastes where
  getAbilities (AcrossTheWastes a) = [mkAbility a 1 $ forced $ BecomesWindow Nighttime | a.flipped]

-- | Setup: Set Pack Leader aside, out of play. Make Shrouded Hills the active location.
-- Add the Time objective to the staging area with Daybreak faceup. The first player takes
-- control of Amarthiúl. Shuffle the encounter deck. Reveal 1 card from the encounter deck
-- per player.
-- Shrouded Hills is Eriador Wilds' (The Lost Realm 048), found by its code.
instance RunMessage AcrossTheWastes where
  runMessage msg q@(AcrossTheWastes attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let copies code = filter ((== code) . toCardCode) deck
      traverse_ setAside $ copies (toCardCode Enemies.packLeader)
      for_ (take 1 $ copies "09048") \c -> do
        removeFromEncounterDeck c
        putIntoPlayAsActiveLocation c
      -- Amarthiúl joins the first player as he comes out
      for_ (concatMap (copies . toCardCode) [Objectives.daybreak, Objectives.amarthiul]) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      shuffleEncounterDeck
      eachPlayer $ const revealEncounterCard
      AcrossTheWastes <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    _ -> AcrossTheWastes <$> runMessage msg attrs
