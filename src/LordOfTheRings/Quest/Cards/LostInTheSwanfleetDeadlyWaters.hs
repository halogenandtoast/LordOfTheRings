module LordOfTheRings.Quest.Cards.LostInTheSwanfleetDeadlyWaters (lostInTheSwanfleetDeadlyWaters) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype LostInTheSwanfleetDeadlyWaters = LostInTheSwanfleetDeadlyWaters QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheSwanfleetDeadlyWaters :: QuestCard LostInTheSwanfleetDeadlyWaters
lostInTheSwanfleetDeadlyWaters = quest LostInTheSwanfleetDeadlyWaters Cards.lostInTheSwanfleetDeadlyWaters

instance HasAbilities LostInTheSwanfleetDeadlyWaters where
  getAbilities (LostInTheSwanfleetDeadlyWaters a) = wanderingStage a

-- | The Nightmare Deck's B side of Lost in the Swanfleet.
-- Treachery card effects cannot be canceled.
instance HasModifiersFor LostInTheSwanfleetDeadlyWaters where
  getModifiersFor (LostInTheSwanfleetDeadlyWaters a) = when a.flipped do
    treacheries <- getsGame (toList . (.entities.treacheries))
    for_ treacheries \t -> modified a t [WhenRevealedCannotBeCanceled]

instance RunMessage LostInTheSwanfleetDeadlyWaters where
  runMessage msg (LostInTheSwanfleetDeadlyWaters attrs) = LostInTheSwanfleetDeadlyWaters <$> runWanderingStage msg attrs
