module LordOfTheRings.Enemy.Cards.GreatWarg (greatWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (currentProgress)

newtype GreatWarg = GreatWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatWarg :: EnemyCard GreatWarg
greatWarg = enemy GreatWarg Cards.greatWarg

instance HasAbilities GreatWarg where
  getAbilities (GreatWarg a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage GreatWarg where
  runMessage msg e@(GreatWarg a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      whenM (attackDestroyedACharacter a.id) $ removeProgressFromQuest a =<< currentProgress
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      chooseOrRunOneM pid do
        questionLabeled "greatWarg.discard"
        causedBy a
        targets attachments $ discard (CardIdSource cid)
      pure e
    _ -> GreatWarg <$> runMessage msg a
