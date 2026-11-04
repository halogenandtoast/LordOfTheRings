module LordOfTheRings.Location.Cards.TheStoneOfErech (theStoneOfErech) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (oathbreakerEnemy, theDeadAreFollowing)

newtype TheStoneOfErech = TheStoneOfErech LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theStoneOfErech :: LocationCard TheStoneOfErech
theStoneOfErech = location TheStoneOfErech Cards.theStoneOfErech

-- | Immune to player card effects. Its travel cost needs 5 progress on stage 3B to remove.
instance HasModifiersFor TheStoneOfErech where
  getModifiersFor (TheStoneOfErech a) = do
    stage <- theDeadAreFollowing
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | maybe True ((< 5) . snd) stage]

-- | Forced: After The Stone of Erech becomes the active location, each player searches the
-- encounter deck and discard pile for an Oathbreaker enemy and adds it to the staging area.
-- Shuffle the encounter deck.
instance HasAbilities TheStoneOfErech where
  getAbilities (TheStoneOfErech a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | Travel: Remove 5 progress from stage 3B to travel here.
instance RunMessage TheStoneOfErech where
  runMessage msg l@(TheStoneOfErech a) = case msg of
    TravelTo lid | lid == a.id -> do
      theDeadAreFollowing >>= traverse_ \(qid, _) -> removeTokens a (QuestTarget qid) Progress 5
      TheStoneOfErech <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p a oathbreakerEnemy
      pure l
    _ -> TheStoneOfErech <$> runMessage msg a
