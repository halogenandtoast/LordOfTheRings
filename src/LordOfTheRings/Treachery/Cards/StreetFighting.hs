module LordOfTheRings.Treachery.Cards.StreetFighting (streetFighting) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (InPlayArea, Unplaced))
import LordOfTheRings.Trait (Trait (Osgiliath))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StreetFighting = StreetFighting TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

streetFighting :: TreacheryCard StreetFighting
streetFighting = treachery StreetFighting Cards.streetFighting

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor StreetFighting where
  getModifiersFor (StreetFighting a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | When Revealed: Discard cards from the top of the encounter deck until an Osgiliath
-- location is discarded. The first player may exhaust a hero to take control of that
-- location. Otherwise, add that location to the staging area. This effect cannot be
-- canceled.
instance RunMessage StreetFighting where
  runMessage msg t@(StreetFighting attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardTopOfEncounterDeck attrs 1
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      case filter (cardMatches osgiliathLocation) cards of
        location : _ -> do
          lead <- getLead
          heroes <- select (HeroControlledBy (PlayerWithId lead) <> ReadyHero)
          removeFromEncounterDiscard location
          chooseOrRunOneM lead do
            questionLabeled "streetFighting.choose"
            causedBy attrs
            targets heroes \h -> do
              exhaust h
              putLocationIntoPlay location (InPlayArea lead)
            labeled "streetFighting.addToStaging" $ addToStaging location
        [] -> do
          scenario <- getsGame (toAttrs . (.scenario))
          questPhase <- getsGame ((== QuestPhase) . (.phase))
          let deck = scenario.encounterDeck
              hasLocation = any (cardMatches osgiliathLocation)
          -- the deck empty in the quest phase is reset and the search goes on; with no
          -- Osgiliath location in either the search would never end, so it stops
          if
            | hasLocation deck || (questPhase && hasLocation scenario.encounterDiscard) -> discardTopOfEncounterDeck attrs 1
            | not questPhase && notNull deck -> discardTopOfEncounterDeck attrs (length deck)
            | otherwise -> pure ()
      pure t
    _ -> StreetFighting <$> runMessage msg attrs
   where
    osgiliathLocation = CardWithType LocationType <> CardWithTrait Osgiliath
