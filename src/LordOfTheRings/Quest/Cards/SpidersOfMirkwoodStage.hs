module LordOfTheRings.Quest.Cards.SpidersOfMirkwoodStage (spidersOfMirkwoodStage) where

import LordOfTheRings.EncounterSet (EncounterSet (SpidersOfMirkwood))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (attackDamagedCharacter, runEncounterSetStage)
import LordOfTheRings.Trait (Trait (Spider))

newtype SpidersOfMirkwoodStage = SpidersOfMirkwoodStage QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Stage 2A of Lost in Mirkwood. No quest points.
spidersOfMirkwoodStage :: QuestCard SpidersOfMirkwoodStage
spidersOfMirkwoodStage = quest SpidersOfMirkwoodStage Cards.spidersOfMirkwoodStage

-- | The first Spider enemy revealed each quest phase gains surge.
instance HasAbilities SpidersOfMirkwoodStage where
  getAbilities (SpidersOfMirkwoodStage a) =
    [ limited (GroupLimit PerPhase 1) $ restricted a 1 (DuringPhase QuestPhase) $ forced $ EnemyWithTraitRevealedWindow After Spider
    | a.flipped
    ]

{- | When Revealed: Shuffle the Spiders of Mirkwood encounter set into the encounter deck.
Then, reveal 1 encounter card per player.
Forced: When a Spider enemy attacks and damages a character, that character cannot ready
until the end of the round.
-}
instance RunMessage SpidersOfMirkwoodStage where
  runMessage msg q@(SpidersOfMirkwoodStage attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure q
    _ -> do
      when attrs.flipped $ attackDamagedCharacter Spider msg >>= traverse_ \c -> roundModifier attrs c CannotReady
      SpidersOfMirkwoodStage <$> runEncounterSetStage SpidersOfMirkwood msg attrs
