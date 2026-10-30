module LordOfTheRings.Event.Cards.RangerSummons (rangerSummons) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Dunedain))

newtype RangerSummons = RangerSummons EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a Dúnedain hero.
-- Planning Action: Shuffle 1 of your set aside Ranger of the North allies with the
-- encounter keyword into the encounter deck. Then, remove Ranger Summons from the game.
rangerSummons :: EventCard RangerSummons
rangerSummons =
  eventWith RangerSummons Cards.rangerSummons \a ->
    a & criteriaL .~ DuringPhase PlanningPhase <> exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Dunedain)

instance RunMessage RangerSummons where
  runMessage msg e@(RangerSummons attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      scenario <- toAttrs <$> getsGame (.scenario)
      -- one already shuffled in is still its owner's, but no longer set aside
      let shuffled = map (.id) $ scenario.encounterDeck <> scenario.encounterDiscard
      rangers <- filter (\c -> cardMatches (cardIs Allies.rangerOfTheNorth) c && c.id `notElem` shuffled) . (.setAsideCards) <$> getPlayer pid
      chooseOrRunOneM pid do
        questionLabeled "rangerSummons.shuffle"
        causedBy attrs
        targets rangers shuffleIntoEncounterDeck
      removeSourceCardFromGame (toSource attrs)
      pure e
    _ -> RangerSummons <$> runMessage msg attrs
