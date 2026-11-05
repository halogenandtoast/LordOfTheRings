module LordOfTheRings.Location.Cards.GlitteringCaves (glitteringCaves) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype GlitteringCaves = GlitteringCaves LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

glitteringCaves :: LocationCard GlitteringCaves
glitteringCaves = location GlitteringCaves Cards.glitteringCaves

instance HasModifiersFor GlitteringCaves where
  getModifiersFor (GlitteringCaves a) = do
    reached <- getsGame (any (\q -> toCardCode q == toCardCode Quests.theWallIsBreached && q.flipped) . toList . (.entities.quests))
    modifySelf a $ [ImmuneToPlayerCardEffects] <> [CannotBeTraveledTo | not reached]
    when (a.placement == ActiveLocation) $ modified a GameTarget [SkipPhase CombatPhase]

instance HasAbilities GlitteringCaves where
  getAbilities (GlitteringCaves a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == ActiveLocation]

instance RunMessage GlitteringCaves where
  runMessage msg l@(GlitteringCaves a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      addToVictoryDisplay a
      eachPlayer \pid -> encounterRaisesThreat pid 2
      pure l
    _ -> GlitteringCaves <$> runMessage msg a
