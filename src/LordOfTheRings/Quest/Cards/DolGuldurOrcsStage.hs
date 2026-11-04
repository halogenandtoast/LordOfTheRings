module LordOfTheRings.Quest.Cards.DolGuldurOrcsStage (dolGuldurOrcsStage) where

import LordOfTheRings.EncounterSet (EncounterSet (DolGuldurOrcs))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (attackDamagedCharacter, runEncounterSetStage)
import LordOfTheRings.Trait (Trait (Orc))

newtype DolGuldurOrcsStage = DolGuldurOrcsStage QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 2C of Lost in Mirkwood. No quest points.
dolGuldurOrcsStage :: QuestCard DolGuldurOrcsStage
dolGuldurOrcsStage = quest DolGuldurOrcsStage Cards.dolGuldurOrcsStage

-- | The first Orc enemy revealed each quest phase gains surge.
instance HasAbilities DolGuldurOrcsStage where
  getAbilities (DolGuldurOrcsStage a) =
    [ limited (GroupLimit PerPhase 1) $ restricted a 1 (DuringPhase QuestPhase) $ forced $ EnemyWithTraitRevealedWindow After Orc
    | a.flipped
    ]

{- | When Revealed: Shuffle the Dol Guldur Orcs encounter set into the encounter deck.
Then, reveal 1 encounter card per player.
Forced: When an Orc enemy attacks and damages a character, that character's controller
chooses and discards 1 card from his hand.
-}
instance RunMessage DolGuldurOrcsStage where
  runMessage msg q@(DolGuldurOrcsStage attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    _ -> do
      when attrs.flipped do
        damaged <- attackDamagedCharacter Orc msg
        for_ damaged \c -> characterController c >>= traverse_ \p -> chooseAndDiscardFromHand p attrs 1
      DolGuldurOrcsStage <$> runEncounterSetStage DolGuldurOrcs msg attrs
