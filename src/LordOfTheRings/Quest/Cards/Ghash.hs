module LordOfTheRings.Quest.Cards.Ghash (ghash) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype Ghash = Ghash QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Search for an Exit (Ghâsh!), from Flight from Moria's nightmare set.
ghash :: QuestCard Ghash
ghash = quest Ghash Cards.ghash

instance HasAbilities Ghash where
  getAbilities (Ghash a) = searchForAnExit a <> bypass a

-- | When Revealed: Deal 1 damage to each questing character.
instance RunMessage Ghash where
  runMessage msg (Ghash attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      questing <- selectCharacters CommittedHero CommittedAlly
      for_ questing \c -> dealDamage attrs c 1
      Ghash <$> runSearchForAnExit msg attrs
    _ -> Ghash <$> runSearchForAnExit msg attrs
