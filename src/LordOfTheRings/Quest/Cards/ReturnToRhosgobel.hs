module LordOfTheRings.Quest.Cards.ReturnToRhosgobel (returnToRhosgobel) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ReturnToRhosgobel = ReturnToRhosgobel QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

returnToRhosgobel :: QuestCard ReturnToRhosgobel
returnToRhosgobel = quest ReturnToRhosgobel Cards.returnToRhosgobel

-- | When Revealed: Heal 5 wounds from Wilyador for each Athelas objective card the players
-- control. If Wilyador is completely healed when this effect resolves, Wilyador survives
-- and the players have won the game. Otherwise, the players have lost the game.
instance RunMessage ReturnToRhosgobel where
  runMessage msg q@(ReturnToRhosgobel attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      ReturnToRhosgobel <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      athelas <- getsGame \g -> [o | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.athelas, isAttached o.placement]
      wilyador <- select (AllyWithCardCode $ toCardCode Objectives.wilyador)
      for_ wilyador \w -> for_ athelas \o -> healDamage o w 5
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      wilyador <- select (AllyWithCardCode $ toCardCode Objectives.wilyador)
      healed <- allM (fmap (== 0) . getDamage . AllyTarget) wilyador
      if notNull wilyador && healed then winGame else gameOver Lost
      pure q
    _ -> ReturnToRhosgobel <$> runMessage msg attrs
   where
    isAttached = \case
      AttachedTo _ -> True
      _ -> False
