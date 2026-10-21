module LordOfTheRings.Quest.Cards.TheHuntBegins (theHuntBegins) where

import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheHuntBegins = TheHuntBegins QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHuntBegins :: QuestCard TheHuntBegins
theHuntBegins = quest TheHuntBegins Cards.theHuntBegins

-- | Forced: After the players quest successfully, the first player looks at the top 3
-- cards of the encounter deck. Reveal and add 1 of those cards to the staging area, and
-- discard the other 2 cards.
instance HasAbilities TheHuntBegins where
  getAbilities (TheHuntBegins a) = [mkAbility a 1 $ forced $ QuestedSuccessfullyWindow After]

-- | Setup: Reveal 1 card per player from the encounter deck, and add it to the staging
-- area.
instance RunMessage TheHuntBegins where
  runMessage msg q@(TheHuntBegins attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      eachPlayer $ const revealEncounterCard
      pure $ TheHuntBegins $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCards lead attrs 3
      pure q
    _ -> TheHuntBegins <$> runMessage msg attrs
