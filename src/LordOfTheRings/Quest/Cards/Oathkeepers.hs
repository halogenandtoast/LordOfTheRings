module LordOfTheRings.Quest.Cards.Oathkeepers (oathkeepers) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype Oathkeepers = Oathkeepers QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

oathkeepers :: QuestCard Oathkeepers
oathkeepers = quest Oathkeepers Cards.oathkeepers

instance HasModifiersFor Oathkeepers where
  getModifiersFor (Oathkeepers a) = when a.flipped do
    blocked <- selectAny (EnemyWithCardCode $ toCardCode Enemies.goblinChieftain)
    when blocked $ modifySelf a [CannotBeDefeated]

instance RunMessage Oathkeepers where
  runMessage msg q@(Oathkeepers attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      Oathkeepers <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.goblinChieftain)
      pure q
    _ -> Oathkeepers <$> runMessage msg attrs
