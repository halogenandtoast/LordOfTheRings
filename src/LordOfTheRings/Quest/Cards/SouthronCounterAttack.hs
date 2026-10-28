module LordOfTheRings.Quest.Cards.SouthronCounterAttack (southronCounterAttack) where

import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SouthronCounterAttack = SouthronCounterAttack QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Siege. After this stage is completed, advance to stage 4A (bypassing stage 3): the
-- scenario takes stage 3 out of the quest deck.
southronCounterAttack :: QuestCard SouthronCounterAttack
southronCounterAttack = quest SouthronCounterAttack Cards.southronCounterAttack

-- | Archery X. X is the number of players in the game.
instance HasModifiersFor SouthronCounterAttack where
  getModifiersFor (SouthronCounterAttack a) = do
    players <- length <$> getRemainingPlayers
    modified a a [GainKeyword (Archery players)]
