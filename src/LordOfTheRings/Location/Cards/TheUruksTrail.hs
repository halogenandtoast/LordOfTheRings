module LordOfTheRings.Location.Cards.TheUruksTrail (theUruksTrail) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (reducePursuit)
import LordOfTheRings.Trait (Trait (UrukHai))

newtype TheUruksTrail = TheUruksTrail LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theUruksTrail :: LocationCard TheUruksTrail
theUruksTrail = location TheUruksTrail Cards.theUruksTrail

nonUniqueUruk :: CardMatcher
nonUniqueUruk = CardWithType EnemyType <> CardWithTrait UrukHai <> NotCard UniqueCard

instance HasModifiersFor TheUruksTrail where
  getModifiersFor (TheUruksTrail a) = do
    scenario <- getsGame (toAttrs . (.scenario))
    -- A travel cost must be paid in full (core rules, p. 23): finding an
    -- eligible enemy is required, not merely performing an empty search.
    unless (any (cardMatches nonUniqueUruk) $ scenario.encounterDeck <> scenario.encounterDiscard) $
      modifySelf a [CannotBeTraveledTo]

instance HasAbilities TheUruksTrail where
  getAbilities (TheUruksTrail a) = [mkAbility a 1 $ response $ ExploredWindow After a.id | a.placement == ActiveLocation]

instance RunMessage TheUruksTrail where
  runMessage msg l@(TheUruksTrail a) = case msg of
    TravelTo lid | lid == a.id -> do
      whenM (notElem Blank <$> getModifiers a) do
        lead <- getLead
        searchEncounterDeckAndDiscard lead a nonUniqueUruk
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      reducePursuit 3
      pure l
    _ -> TheUruksTrail <$> runMessage msg a
