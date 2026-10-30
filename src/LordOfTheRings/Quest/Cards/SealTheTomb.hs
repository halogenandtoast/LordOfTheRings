module LordOfTheRings.Quest.Cards.SealTheTomb (sealTheTomb) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (encounterDiscardPile)
import LordOfTheRings.Trait (Trait (Undead))

newtype SealTheTomb = SealTheTomb QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
sealTheTomb :: QuestCard SealTheTomb
sealTheTomb = quest SealTheTomb Cards.sealTheTomb

-- | Forced: At the end of the refresh phase, discard the top 3 cards of the encounter
-- deck. Return the topmost Undead enemy in the encounter discard pile to the staging area.
instance HasAbilities SealTheTomb where
  getAbilities (SealTheTomb a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

-- | The whole discard pile is searched once the 3 cards are on it.
instance RunMessage SealTheTomb where
  runMessage msg q@(SealTheTomb attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 3
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      let undead c = cdCardType (toCardDef c) == EnemyType && Undead `member` cdCardTraits (toCardDef c)
      traverse_ addToStagingFromDiscard . find undead =<< encounterDiscardPile
      pure q
    _ -> SealTheTomb <$> runMessage msg attrs
