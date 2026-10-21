module LordOfTheRings.Ally.Cards.EmynArnenRanger (emynArnenRanger) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Trait (Trait (Trap))

newtype EmynArnenRanger = EmynArnenRanger AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

emynArnenRanger :: AllyCard EmynArnenRanger
emynArnenRanger = ally EmynArnenRanger Cards.emynArnenRanger

-- | Response: After Emyn Arnen Ranger commits to the quest, choose an enemy with a Trap
-- attachment. Until the end of the phase, Emyn Arnen Ranger gets +X willpower, where X
-- is the chosen enemy's printed threat.
instance HasAbilities EmynArnenRanger where
  getAbilities (EmynArnenRanger a) =
    [restricted a 1 (exists $ EnemyWithAttached Trap) $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage EmynArnenRanger where
  runMessage msg e@(EmynArnenRanger a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyWithAttached Trap)
      chooseOrRunOneM pid do
        questionLabeled "emynArnenRanger.chooseEnemy"
        causedBy a
        targets enemies \eid -> do
          x <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
          phaseModifier a a (WillpowerModifier x)
      pure e
    _ -> EmynArnenRanger <$> runMessage msg a
