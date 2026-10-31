module LordOfTheRings.Treachery.Cards.AGreatHost (aGreatHost) where

import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AGreatHost = AGreatHost TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aGreatHost :: TreacheryCard AGreatHost
aGreatHost = treachery AGreatHost Cards.aGreatHost

-- | When Revealed: Each player returns each enemy engaged with him to the staging area.
-- Either search the encounter deck, discard pile and victory display for Orc Rearguard and
-- reveal it, or each player must deal 4 damage to a character he controls. Shuffle the
-- encounter deck. The first player chooses.
instance RunMessage AGreatHost where
  runMessage msg t@(AGreatHost attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      returnEngagedEnemiesToStaging
      lead <- getLead
      chooseOneM lead do
        questionLabeled "aGreatHost.choose"
        causedBy attrs
        labeled "aGreatHost.orcRearguard" $ searchAndRevealEncounterCard (cardIs Quests.orcRearguard)
        labeled "aGreatHost.damage" do
          eachPlayer \p -> dealDamageToOneCharacter attrs p 4
          shuffleEncounterDeck
      pure t
    _ -> AGreatHost <$> runMessage msg attrs
