module LordOfTheRings.Event.Cards.RavensOfTheMountain (ravensOfTheMountain) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype RavensOfTheMountain = RavensOfTheMountain EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a hero you control to shuffle the encounter deck and look at its top
-- card. Place progress tokens on the current quest equal to the [threat] of the card you
-- looked at. Then, put that card back on top of the encounter deck. (FAQ 1.9 errata; as
-- printed, "Exhaust a hero".)
ravensOfTheMountain :: EventCard RavensOfTheMountain
ravensOfTheMountain =
  eventWith RavensOfTheMountain Cards.ravensOfTheMountain \a ->
    a & criteriaL .~ exists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero)

instance RunMessage RavensOfTheMountain where
  runMessage msg e@(RavensOfTheMountain attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOrRunOneM pid do
        questionLabeled "ravensOfTheMountain.chooseHero"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          shuffleEncounterDeck
          doStep 1 msg
      pure e
    -- the card looked at stays where it is, on top of the encounter deck
    DoStep 1 (PlayEvent pid card _) | card.id == toId attrs -> do
      lookAtTopOfEncounterDeck pid attrs False
      top <- getTopOfEncounterDeck
      for_ (cdThreat . toCardDef =<< top) \x -> when (x > 0) $ placeProgressOnCurrentQuest attrs x
      pure e
    _ -> RavensOfTheMountain <$> runMessage msg attrs
