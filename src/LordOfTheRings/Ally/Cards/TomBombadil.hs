module LordOfTheRings.Ally.Cards.TomBombadil (tomBombadil) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype TomBombadil = TomBombadil AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter, on its card def: revealed from the encounter deck, it enters play under the
-- first player (see the game's reveal). Its When Revealed effect cannot be canceled, as no
-- player card's with the encounter keyword can (MEC78 rules).
tomBombadil :: AllyCard TomBombadil
tomBombadil = ally TomBombadil Cards.tomBombadil

-- | A player card with the encounter keyword that leaves play is removed from the game
-- (MEC78 rules, "Encounter Keyword").
instance HasModifiersFor TomBombadil where
  getModifiersFor (TomBombadil a) = modifySelf a [RemoveFromGameIfLeavesPlay]

-- | At the end of the round, remove Tom Bombadil and a copy of Tom Bombadillo! in the
-- victory display from the game.
instance HasAbilities TomBombadil where
  getAbilities (TomBombadil a) = [mkAbility a 1 $ forced RoundEndsWindow]

{- | When Revealed: Choose a player. That player gains control of Tom Bombadil, ready and
committed to the quest. This effect cannot be cancelled.

The first player chooses, as for the other allies with the encounter keyword. A character
is committed to the quest only in the quest phase: revealed at another time, Tom joins the
chosen player ready, committed to nothing.
-}
instance RunMessage TomBombadil where
  runMessage msg s@(TomBombadil a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      phase <- getsGame (.phase)
      choosePlayerM lead a \pid -> do
        when (pid /= lead) $ takeControl a.id pid
        when (phase == QuestPhase) $ commitToQuest a
        ready a
      pure s
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeFromGame a
      display <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      traverse_ removeFromVictoryDisplay $ find (cardMatches $ cardIs Events.tomBombadillo) display
      pure s
    _ -> TomBombadil <$> runMessage msg a
