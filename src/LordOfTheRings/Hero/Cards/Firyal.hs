module LordOfTheRings.Hero.Cards.Firyal (firyal) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor.Helpers (eliminatedIfLeavesPlay)

newtype Firyal = Firyal HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Objective-hero.
firyal :: HeroCard Firyal
firyal = hero Firyal Cards.firyal

-- | Response: After Firyal commits to a quest, look at the top card of the encounter deck.
-- Then, you may discard that card.
instance HasAbilities Firyal where
  getAbilities (Firyal a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After (toTarget a)]

-- | If Firyal leaves play, you are eliminated from the game.
instance RunMessage Firyal where
  runMessage msg h@(Firyal attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      getTopOfEncounterDeck >>= traverse_ \top -> chooseOneM pid do
        questionLabeled "topOfEncounterDeck"
        causedBy attrs
        showing [EncounterCard top]
        labeled "discardIt" $ discardTopOfEncounterDeck attrs 1
        labeled "leaveOnTop" (pure ())
      pure h
    _ -> do
      eliminatedIfLeavesPlay attrs msg
      Firyal <$> runMessage msg attrs
