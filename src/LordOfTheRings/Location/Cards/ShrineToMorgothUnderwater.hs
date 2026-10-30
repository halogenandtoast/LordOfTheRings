module LordOfTheRings.Location.Cards.ShrineToMorgothUnderwater (shrineToMorgothUnderwater) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | The Drowned Ruins' Shrine to Morgoth, flooded: its Underwater side.
newtype ShrineToMorgothUnderwater = ShrineToMorgothUnderwater LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shrineToMorgothUnderwater :: LocationCard ShrineToMorgothUnderwater
shrineToMorgothUnderwater = location ShrineToMorgothUnderwater Cards.shrineToMorgothUnderwater

-- | Shrine to Morgoth cannot have attachments and cannot enter the staging area.
instance HasModifiersFor ShrineToMorgothUnderwater where
  getModifiersFor (ShrineToMorgothUnderwater a) = modifySelf a [CannotHaveAttachments]

-- | Forced: If Shrine to Morgoth is the active location at the end of the quest phase,
-- raise each player's threat by 5.
instance HasAbilities ShrineToMorgothUnderwater where
  getAbilities (ShrineToMorgothUnderwater a) =
    [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

-- | When Shrine to Morgoth is placed in the victory display, the players win the game.
instance RunMessage ShrineToMorgothUnderwater where
  runMessage msg l@(ShrineToMorgothUnderwater a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 5
      pure l
    AddToVictoryDisplay (isTarget a -> True) -> do
      winGame
      ShrineToMorgothUnderwater <$> runMessage msg a
    _ -> ShrineToMorgothUnderwater <$> runMessage msg a
