module LordOfTheRings.Quest.Cards.OutOfTheDungeons (outOfTheDungeons) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OutOfTheDungeons = OutOfTheDungeons QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheDungeons :: QuestCard OutOfTheDungeons
outOfTheDungeons = quest OutOfTheDungeons Cards.outOfTheDungeons

-- | Forced: At the beginning of each quest phase, each player places the top card of his
-- deck, face down in front of him, as if it just engaged him from the staging area.
instance HasAbilities OutOfTheDungeons where
  getAbilities (OutOfTheDungeons a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | Players cannot defeat this stage while Nazgul of Dol Guldur is in play. If this stage is
-- defeated and Nazgul of Dol Guldur is not in play, the players have won the game.
instance RunMessage OutOfTheDungeons where
  runMessage msg q@(OutOfTheDungeons attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer $ push . PlaceOrcGuard
      pure q
    _ -> OutOfTheDungeons <$> runBlockedStage (EnemyWithCardCode $ toCardCode Enemies.nazgulOfDolGuldur) msg attrs
