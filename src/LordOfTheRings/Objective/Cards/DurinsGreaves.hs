module LordOfTheRings.Objective.Cards.DurinsGreaves (durinsGreaves) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype DurinsGreaves = DurinsGreaves ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

durinsGreaves :: ObjectiveCard DurinsGreaves
durinsGreaves = objective DurinsGreaves Cards.durinsGreaves

-- | Attached hero gets +1 defense.
instance HasModifiersFor DurinsGreaves where
  getModifiersFor (DurinsGreaves a) = case a.placement of
    AttachedTo host -> modified a host [DefenseModifier 1]
    _ -> pure ()

-- | When Revealed: The first player attaches Durin's Greaves to a hero of his choice as an
-- attachment.
instance RunMessage DurinsGreaves where
  runMessage msg o@(DurinsGreaves attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      heroes <- select AnyHero
      chooseOrRunOneM lead do
        questionLabeled "durinsGreaves.attach"
        causedBy attrs
        targets heroes $ attachObjective attrs.id
      pure o
    _ -> DurinsGreaves <$> runMessage msg attrs
