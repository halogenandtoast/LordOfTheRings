module LordOfTheRings.Location.Cards.TheLeapingFish (theLeapingFish) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Scroll))

newtype TheLeapingFish = TheLeapingFish LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLeapingFish :: LocationCard TheLeapingFish
theLeapingFish = location TheLeapingFish Cards.theLeapingFish

-- | If Alcaron's Scroll is attached to a hero, The Leaping Fish gains: "Forced: At the
-- beginning of the quest phase discard X cards from the top of the encounter deck where X
-- is the number of players in the game. Add each enemy discarded by this effect to the
-- staging area."
instance HasAbilities TheLeapingFish where
  getAbilities (TheLeapingFish a) =
    [restricted a 1 (exists $ HeroWithAttached Scroll) $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TheLeapingFish where
  runMessage msg l@(TheLeapingFish a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardTopOfEncounterDeck a . length =<< getRemainingPlayers
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard [c | c <- cards, cdCardType (toCardDef c) == EnemyType]
      pure l
    _ -> TheLeapingFish <$> runMessage msg a
