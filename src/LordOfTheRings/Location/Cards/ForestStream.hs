module LordOfTheRings.Location.Cards.ForestStream (forestStream) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonedCharacters)

newtype ForestStream = ForestStream LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

forestStream :: LocationCard ForestStream
forestStream = location ForestStream Cards.forestStream

-- | While Forest Stream is the active location, poisoned characters cannot quest.
instance HasModifiersFor ForestStream where
  getModifiersFor (ForestStream a) = when (a.placement == ActiveLocation) do
    poisoned <- poisonedCharacters
    for_ poisoned \c -> modified a c [CannotCommitToQuest]

-- | Response: After Forest Stream leaves play as an explored location, discard 4 poison.
instance HasAbilities ForestStream where
  getAbilities (ForestStream a) =
    [restricted a 1 (AnyCriteria [exists PoisonedHero, exists PoisonedAlly]) $ response $ ExploredWindow When a.id]

-- | The responding player picks the poison to discard, 1 at a time, from any characters.
instance RunMessage ForestStream where
  runMessage msg l@(ForestStream a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      doStep 4 msg
      pure l
    DoStep n inner@(UseCardAbility pid (isSource a -> True) 1 _) | n > 0 -> do
      poisoned <- poisonedCharacters
      chooseOrRunOneM pid do
        questionLabeled "forestStream.discardPoison"
        causedBy a
        targets poisoned \c -> do
          discardPoison c 1
          doStep (n - 1) inner
      pure l
    _ -> ForestStream <$> runMessage msg a
